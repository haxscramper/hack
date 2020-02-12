#!/usr/bin/env sh
# -*- mode: Makefile; -*-
#? Setting emacs major mode for files that have incorrect extension

#? How to use GNU make as executable script
tmp=$(mktemp "/tmp/XXXXXXXXXX.make")
sed '0,/^# --- start ---$/ s/.*/ /' "$0" > $tmp
echo $tmp
exec make --directory=$(pwd) --makefile=$tmp

# --- start ---

printer := $(shell																						\
	if [[ res=$(which colecho 2> /dev/null) && ! -z "$res" ]];	\
	then																												\
		echo "colecho";																						\
		else																											\
		echo "echo -- ";																					\
	fi																													\
)

log := $(printer)
wrn := $(printer) -w
err := $(printer) -e
#? Get path to the current makefile
mkfile_path := $(abspath $(lastword $(MAKEFILE_LIST)))
SHELL=bash

## Project documentation

.ONESHELL:
default: \
	show-message-printers \
	start-automatic-rebuild \
	format-code \
	help


.oneshell:
show-message-printers:
	@echo $(printer)
	$(log) hello
	$(wrn) test
	$(err) asdfasdf

#? Show multiline help message for commands
.oneshell:
help:
	@true
	## Show documentation for all build targets
	cat $(mkfile_path) | {
	  # Remove everything but targets and their bodies
	  sed -r '/(^[-_a-zA-Z]+:)|(\t.*?)/!d'
	} | {
	  # Add target name before each line of the body
		awk -f <(cat - <<-'EOFCAT' # Using heredocument for awk scripts
	    # AWK start
			BEGIN { curr="project"; }
			{
				if (/^[-_a-zA-Z]+:/) {
					curr=$$0;
				} else {
					print curr" : "$$0;
				}
			}
	    # AWK end
			EOFCAT
		)
	  cat $(mkfile_path) |
		sed -r '/^##/!d; s/^##(.*?)$$/makefile-doc: : ## \1/'
	} | {
	  tee /dev/tty
	} | {
	  # Clean up target names (remove trailing newlines, dependencies)
	  # IDEA print list of dependencies too
		awk -F: '{print $$1": "$$3}'
	} | {
	  # Remove all lines that don't contain documentation comments
	  sed -r '/^[-_a-zA-Z]+:\s*##/!d'
	} | {
	  # Remove leading shitespace and documentation comments from each line
	  sed -r 's/([-_a-zA-Z]+):\s+##\s*(.*)/\1:::\2/'
	} | {
		# tee -a /dev/tty
		cat
	} | {
	  # Merge lines with the same prefix
		gawk -F':::' -f <(cat - <<-'EOFCAT'
		  # AWK begin
			BEGIN { curr="default"; }
		  # Pattern matching begin
		  {
			  if ($$1 != curr) {
					curr=$$1;
			    assoc[curr]=$$2;
				} else {
			    assoc[curr]=assoc[curr]" "$$2;
				}
		  }
		  # Pattern matching end
		  END {
			  for (key in assoc) {
					print "# "key"\n";
					print assoc[key]"\n";
				}
			}
		  # AWK end
		  EOFCAT
		)
	} | {
	  # Fortat output
	  fmt
	}

# IMPLEMENT format documentation as either markdown or troff

# TODO sort documentation in order of appearance (assign index and
# then do numerical sort of the output before formatting)

.oneshell:
build:
	@true
	## Perform single build
	## ANother documentation line
	@$(log) Starting build

.oneshell:
format-code:
	@$(log) Format code for the project
	fd -e cpp -e hpp | while read -r file; do
		$(log) -I:3 Formatting "$$file"
		clang-format -i "$$file"
	done

.oneshell:
start-automatic-rebuild:
	@$(wrn) Starting automatic rebuild.
	$(log) To exit press 'q'
