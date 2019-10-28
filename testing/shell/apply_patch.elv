#!/usr/bin/elvish

# For very verbose logging can be set to "true"
verbose = "false"

fn msg [a @rest]{
  e:colecho -v $a -- $@rest > /dev/tty
}

fn msg-1 [a @rest]{
  e:colecho -I1 -v $a -- $@rest > /dev/tty
}

fn die [@rest]{
  e:colecho -e3 $@rest
}

tmp_dir = "/tmp/apply_patch.d"
exports_dir = $tmp_dir"/exports"
patches_dir = $tmp_dir"/patches"
splits_dir = $tmp_dir"/splits"
new_dir_prefix = ".new"

fn gtmp [file]{
  echo $tmp_dir"/"$file
}



rm -rf $tmp_dir/*
mkdir -p $tmp_dir/{patches exports target_sum splits patches}
try { rm -f /tmp/apply_patch.d/.tmp* } except e { echo "" }

fn parse_diff [diff_file patch_dir repo_dir]{
  diff_abs = $patch_dir/$diff_file
  msg -i0 "Parsing diff: "$diff_file

  start = (pwd)

  cd $repo_dir

  hash = (head -n1 $diff_abs | cut -d' ' -f2)
  parent = (git rev-list --parents -n1 $hash | cut -d' ' -f2)
  msg "Commit hash_______: "$hash
  msg "Parent commit hash: "$parent
  msg -i0 "Checking out commit"
  git checkout --quiet $parent

  grep -- "diff --git" $diff_abs \
  | cut -d' ' -f4 \
  | sed 's!b/!!' \
  | each [file]{
    try {
      echo (git hash-object $file 2> /dev/null)" "$file" edit"
    } except e {
      echo "==== "$file" create"
    }
  } > $exports_dir/$diff_file"_changes"

  cd $start
}

fn split_diff [diff_file]{
  diff_file = (echo $diff_file | sed 's/_changes//')
  msg -i2 "Splitting" $diff_file
  abs_path = $patches_dir/$diff_file
  awk '/diff --git/{x=++i;next}
  {
    if ($0 !~ /index */ && $1 != "new") {
      print $0 > "/tmp/apply_patch.d/.tmp"x;
    }
  }' \
  $abs_path

  msg "Done splitting"
}

fn apply_patch [patch_file commit_hash to_repo]{
  cd $to_repo

  msg -w3 "Applying patch on top of the commit "$commit_hash
  diff_file = (echo $patch_file | sed 's/_changes//')
  msg "Patch file:" $diff_file
  msg "From directory:" $exports_dir
  split_diff $diff_file
  msg "Checking out commit"

  git checkout --quiet $commit_hash
  msg "Searching for requested files"

  cat $exports_dir/$patch_file \
  | eawk [line hash name mode]{
    echo "="
    try {
      # Find path to the file to apply commit to
      target_path = (if (==s $mode "create") {
        msg-1 "Creating new file" $new_dir_prefix"/"$name
        res = $new_dir_prefix"/"$name
        mkdir -p (e:dirname $to_repo"/"$res)
        e:touch $to_repo"/"$res
        echo $res
      } else {
        git ls-tree -r $commit_hash \
        | grep $hash \
        | awk '{print $4}'
      })

      msg-1 "Applying patch to file" $target_path


      # Find path ot original file from changes in diff
      target_diff = (if (==s $mode "create") {
          # If file was created create new one and apply patch onto it
          msg-1 "Searching for original created path"
          created_path = (grep $name $exports_dir"/"$patch_file | awk '{print $2}')
          msg-1 "Original file was created at" $created_path

          # Use "+++ b/" because it the _new_ file in diff
          grep -w /tmp/apply_patch.d/.tmp* -e "+++ b/"$created_path \
          | cut -d':' -f1
      } else {
          msg-1 "Searching for original modification path"
          source_path = (grep $hash $exports_dir"/"$patch_file | awk '{print $2}')
          msg-1 "Original changes were made to file" $source_path

          # Use "--- a/" because it is the name of the _old_ file in diff
          grep -w /tmp/apply_patch.d/.tmp* -e "--- a/"$source_path \
          | cut -d':' -f1
      })


      msg-1 "Target diff" $target_diff
      msg-1 -i2 (patch -u $to_repo"/"$target_path $target_diff)

    } except e {
      msg-1 -e3 $e
      exit 1
    }
  }
}

fn get_starting_commit [patch_file]{
  missing_file = ""
  missing_hash = ""
  try {
    res = (git rev-list --all \
    | each [commit]{
      try {
        # Check if all files are availiable
        cat $exports_dir/$patch_file \
        | eawk [line hash name mode]{
          # If file is modified it should be visible
          if (!=s $mode "create") {
            try {
              echo $commit" "(git ls-tree -r $commit \
              | grep $hash \
              | head -n1 \
              | awk '{print $4}')" "$mode
            } except e {
              missing_file = $name
              missing_hash = $hash
              fail "hash not found"
            }
          }
        # We are not interested in the output, only in the fact that
        # commit has all the required files with correct checksums
        # (i.e. no exceptions were thrown)
        } > /dev/null
      } except e {
          # We use exceptions to signal about missing files in the
          # commit so exception is expected to happen pretty often.

          if (==s $verbose "true") {
            msg-1 -i1 "Commit" (git log --format="%B" -n1 $commit) "does not match"
          }
      } else {
        echo $commit
      }
    } \
    | each [commit]{
      # Debug list all commits that can be used
      msg-1 (git log --format="%aD %B" -n1 $commit)
      echo $commit
    } \
    | head -n1)
    # Taking last matching commit in the list since it is closest one to
    # current head
    echo $res
  } except e {
    msg -e3 "Failed to find starting commit"
    msg "Missing file:" $missing_file
    msg "Missing hash:" $missing_hash
    msg "Working directory:" (pwd)
    msg "git rev-list --all | xargs -i git ls-tree -r '{}' | grep" $missing_hash

    exit 1
  }
}

fn apply_patches [to_repo to_branch]{
  msg -i1 "Applying patches"
  msg "Target repository:" $to_repo
  msg "Target branch:" $to_branch

  msg "Processing patches"
  cd $to_repo

  msg-1 "Matching commits"
  ls $exports_dir \
  | each [patch_file]{
    msg "Patch file :" $patch_file
    starting_commit = (get_starting_commit $patch_file)

    msg -i1 "Can be applied on top of commit" $starting_commit
    msg (git log --format=%B -n1 $starting_commit)
    apply_patch $patch_file $starting_commit $to_repo
    msg -i3 "Applied" $patch_file
    try { rm -f $tmp_dir/.tmp? } except e { msg -e0 $e }

    try {
      git add .
      git diff --staged > $tmp_dir"/"$patch_file"_result"
      git commit -m "[>>>][TMP] Applied "(echo $patch_file \
      | sed 's/.patch_changes//')
    } except e {
      msg -e3 $e
      msg -e0 "Failed to commit changes"
      exit 1
    }

    echo
  }
}

fn export_patches [from_repo diff_from]{
  msg "Patch dir:" $patches_dir

  start = (pwd)
  cd $from_repo
  git format-patch --quiet $diff_from -o $patches_dir

  ls $patches_dir | grep '.patch' | each [l]{
    parse_diff $l $patches_dir $from_repo
  }

  cd $start
}


fn reclone [url path]{
  msg "Recloning" $url "=>" $path
  rm -rf $path
  git clone --quiet -- $url $path
}

fn setup {
  reclone ../clean-clone/hmerge hmerge
  reclone ../clean-clone/qiucus-development/source/Support Support
  mkdir -p /tmp/apply_patch.d/exports
}

setup
export_patches (pwd)/Support origin/master
apply_patches (pwd)/hmerge origin/master
