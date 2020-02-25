## Test makefile function definition support

## Return name of the command if it exists (can be found by `which`)
## or return fallback command.
define get-software-or-fallback
	$(shell																					\
		if [[ res=$$(which $1 2> /dev/null) && ! -z "$$res" ]] ;	\
		then																												\
			echo "$1";																						\
			else																											\
			echo "$2";																					\
		fi																													\
	)
endef

## Get command for printing messages
define get-message-printer
	$(call get-software-or-fallback,colecho,"echo --")
endef

printer := $(get-message-printer)

default:
	@echo $(printer)
