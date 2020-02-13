## Test makefile function definition support

define has-software
[[ res=$$(which $1 2> /dev/null) && ! -z "$$res" ]]
endef

define get-message-printer
	$(shell																					\
		if $(call has-software,colecho) ;	\
		then																												\
			echo "colecho";																						\
			else																											\
			echo "echo -- ";																					\
		fi																													\
	)
endef

printer := $(get-message-printer)

default:
	@echo $(printer)
