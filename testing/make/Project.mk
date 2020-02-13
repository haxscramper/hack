#!/usr/bin/env sh
# -*- mode: Makefile; -*-
#? Setting emacs major mode for files that have incorrect extension

# TODO IDEA makefile documentation for functions. Due to the make's
# invability to have comments in `define` blocks (it is not like
# anyone will need this anyway) documentation has to be added before
# the function defintion. This makes parsing harder because now we
# need to distinguis between makefile documentation and docs for each
# fucntion. This /can/ be done like this: parse all lines that match
# `^##` and then remove all lines that are followed/not-followed
# immediately by `^define ([-_a-zA-Z]+)`. In awk it might look like
# this: /^##/ `{ buf = buf + $0; in_doc = true; } /^define/ { if
# (in_doc) { func_docs[name_now] = buf; buf = ""; } END { for (func in
# func_docs) { printf "%s:::%s", func, func_docs} } }` Somewhat
# pseudo-code like but in general it should look like this.


define self-launch-makefile
#? How to use GNU make as executable script
tmp=$(mktemp "/tmp/XXXXXXXXXX.mk")
sed '0,/^# --- start ---$/ s/.*/ /' "$0" > $tmp
echo $tmp
exec make --directory=$(pwd) --makefile=$tmp
endef

$(self-launch-makefile)


# --- start ---

## Return name of the command if it exists (can be found by `which`)
## or return fallback command.
define get-software-or-fallback
	$(shell																					\
		if [[ res=$$(which $1 2> /dev/null) && ! -z "$$res" ]] ;	\
		then																												\
			echo "$1";																						\
			else																											\
			echo "$1";																					\
		fi																													\
	)
endef

## Get command for printing messages
define get-message-printer
	$(call get-software-or-fallback,colecho,"echo --")
endef


printer := $(get-message-printer)

log := $(printer)
wrn := $(printer) -w
err := $(printer) -e
#? Get path to the current makefile
mkfile_path := $(abspath $(lastword $(MAKEFILE_LIST)))
SHELL=bash

## Project documentation. This line will be parsed as `makefile-doc`
## comment and added on top of the documentation.

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

define parse-help
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
		# tee /dev/tty
		cat
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
		# TODO Refactor into separate script
	  # Merge lines with the same prefix
		awk -F':::' -f <(cat - <<-'EOFCAT'
		  # AWK begin
			BEGIN { curr="default"; idx=0; }
		  # Pattern matching begin
		  {
			  if ($$1 != curr) {
					idx++;
					curr=idx":::"$$1;
			    assoc[curr]=$$2;
				} else {
			    assoc[curr]=assoc[curr]" "$$2;
				}
		  }
		  # Pattern matching end
		  END {
			  for (key in assoc) {
					printf "%s:::%s\n", key, assoc[key];
				}
			}
		  # AWK end
		  EOFCAT
		)
	} | {
	  # Sort targets in order of documentation
	  sort -n
	} | {
	  # Remove index and separate fields. Add sectioning
	  awk -F':::' -f <(cat - <<-'EOFCAT'
		  # Pattern matching begin
		  {
		    printf "# %s\n\n%s\n\n", $$2, $$3;
		  }
		  # Pattern matching end
		  EOFCAT
		)
	} | {
	  # format to width
	  fmt
	} | {
		# TODO separate into single script for naive syntax highlighting
		# of text. In most cases it will be more than enough.

	  # Pseudo-markdown highlighting
		sed -r -f <(cat - <<-'EOFCAT'
			s/^# (.*)/# \x1b[34m\1\x1b[0m/
		  EOFCAT
		)
	}
endef

# IDEA Generate documentation for all bash scripts by piping
# double-hash comments into troff. This should be very simple to
# implement. Add simple cheat-sheet for formatting help messages
# (simple troff syntax reference). This is good enough alternative to
# `argcheck`.

#? Show multiline help message for commands
.oneshell:
help:
	@true
	## Show documentation for all build targets
	$(parse-help)

# IMPLEMENT format documentation as either markdown or troff

# TODO sort documentation in order of appearance (assign index and
# then do numerical sort of the output before formatting)

# TODO Move all help parsing code into file that can be incuded into
# other files.

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
