#!/usr/bin/env bash
# -*- coding: utf-8 -*-
# bash
set -o nounset
set -o errexit
msg="colecho -b"

html_gen="generate_page.py"
method_gen="../extract_methods/extract_methods.nim.bin"
flowchart_gen="../flowchart_generator/flowchart_generator.nim.bin"

in_file_dir=${1:-".prt_3.java.tmp.d/"}
split_methods_dir="methods.tmp.d"
flowchart_images="flowcharts.tmp.d"
final_dir="output.tmp.d"
image_ext="png"
external_flowchart_absp="$HOME/workspace/tmp/$in_file_dir"

mkdir -p "$final_dir"
mkdir -p "$flowchart_images"
mkdir -p "$split_methods_dir"
mkdir -p "$external_flowchart_absp"

find . -type f -name "*.tmp.*" -print0 | xargs -0 rm -f

colecho -g -i:3 "Starting"

while read -r file
do
    $msg -w:1 "Removing '$file'"
    rm -- "$file"
done < <(find "$in_file_dir" -type f ! -name "*.java")

function split_classes {
    $msg -i:1 "Splitting files in directory '$in_file_dir'"
    while read -r class_file
    do
        $msg "Working with $class_file"
        if ! ./$method_gen \
             --verbose \
             --input-file:"$in_file_dir/$class_file" \
             --output-dir:"$split_methods_dir" \
             --maxlen:200 \
             --ignored-methods:"initComponents,main" \
             --debug
           then
               $msg -e:2 "Failed while splitting file '$class_file'"
               bat --paging=never "$in_file_dir/$class_file"
               exit 1
        fi

    done < <(find "$in_file_dir" -type f -printf "%f\n")
}

split_classes

$msg -i:1 "Formatting generated files"
while read -r method_file
do
    clang-format -i -style=file -verbose "$method_file"
done < <(find $split_methods_dir -type f)

function copy_to_external {
    $msg -i:1 "Copying files for external flowchart drawing"
    while read -r method_file
    do
        $msg -I:4 "Working with '$method_file'"
        local abs_method="$split_methods_dir/$method_file"
        cp "$abs_method" "$external_flowchart_absp/$method_file"
    done < <(find $split_methods_dir -type f -printf "%f\n")
}

copy_to_external

function generate_flowcharts {
    $msg -i:1 "Generating flowcharts for files in directory '$split_methods_dir'"
    while read -r method_file
    do
        $msg -I:4 "Working with '$method_file'"
        local abs_method="$split_methods_dir/$method_file"

        $msg -I:8 "generate dot file ..."
        if ! ./$flowchart_gen \
             --input:"$abs_method" \
             --output:"$flowchart_images/$method_file.dot"
        then
            $msg -I:8 -e "Error ocurred duing file parse:"
            bat "$abs_method"
            exit
        fi

        $msg -I:8 "generate synt file ..."
        ./$flowchart_gen \
            --dump-tree \
            --input:"$abs_method" \
            --output:"$flowchart_images/$method_file.synt"

    done < <(find $split_methods_dir -type f -printf "%f\n")
}

generate_flowcharts

$msg -i:1 "Generating images for files in directory '$flowchart_images'"
while read -r dot_file
do
    $msg -I:4 "Working with '$dot_file'"
    if ! dot -T$image_ext "$dot_file" > "$dot_file.$image_ext"
       then
           $msg -I:8 -e "Error ocurred during file conversion"
           bat "$dot_file"
           exit
    fi
done < <(find $flowchart_images -type f -name "*.dot")

$msg -i:1 "Generating debug html page"
./$html_gen \
    --output-file="out.tmp.html" \
    --image-suffix=".dot.$image_ext" \
    --image-prefix="$flowchart_images/" \
    --syntax-suffix=".synt" \
    --syntax-prefix="$flowchart_images/" \
    --files-glob="$split_methods_dir/*" \
    --files-prefix="$split_methods_dir/"

image_files=""

function collect_autogenerated_images {
    while read -r flowchart
    do
        $msg -I:4 "Renaming $flowchart_images/$flowchart"
        oldname=$flowchart
        newname="${oldname//'.'/'_'}.$image_ext"
        cp "$flowchart_images/$oldname" "$final_dir/$newname"
        image_files="$image_files,$newname"
    done < <(find $flowchart_images -type f -name "*.$image_ext" -printf "%f\n")
}




image_suffix=""
function collect_manual_images {
    $msg -i:1 "Collecting manually created images from '$external_flowchart_absp'"
    while read -r flowchart
    do
        $msg -I:4 "$flowchart"
        oldname=$(basename -- "$flowchart")
        newname="${oldname//'.'/'_'}.$image_ext"
        image_suffix=".tmp.c.drawio.$image_ext.$image_ext"

        $msg -I:8 "Found image $flowchart"
        $msg -I:8 "New name is: '$newname'"
        $msg -I:8 "Old name is: '$oldname'"

        cp "$flowchart" "$final_dir/$newname"
        if [[ "$image_files" -eq "" ]]; then
            image_files="$newname"
        else
            image_files="$image_files,$newname"
        fi

    done < <(find "$external_flowchart_absp" -type f -name "*.$image_ext")
}





collect_manual_images

$msg -i:1 "Running report template"

touch "$in_file_dir/task.txt"

# shellcheck disable=SC2001
in_file_dir_no_trail=$(echo "$in_file_dir" | sed 's:/*$::')

task_formulation=$(find "$in_file_dir" -name "task.txt")
source_files=$(find "$in_file_dir" -name "*.java" -printf \
                    "$in_file_dir_no_trail/%f\n" \
                   | tee /dev/tty \
                   | tr '\n' ',')

source_files=${source_files%?}


cp $flowchart_images/*.$image_ext $final_dir
echo "Input files: $source_files"

if ! tpage \
     --define "files=$source_files" \
     --define "task=$task_formulation" \
     --define "image_files=$image_files" \
     --define "image_suffix=$image_suffix" \
     --define "image_prefix=$external_flowchart_absp/" \
     --define "class_prefix=$in_file_dir" \
     report_template > "$final_dir/report.tex"
   then
       echo
       $msg -e:1 "tpage failed"
       exit 1
fi

$msg -i:1 "Building pdf"

cd $final_dir

latexmk -C report.tex
if ! latexmk -latexoption="-shell-escape" \
     -pdflua --interaction=nonstopmode report.tex
   then
       $msg -e:3 "Latex build failed"
else
    $msg -i:1 "Latex build ok"
    rm -- *.png
fi

$msg -i:3 "Done"