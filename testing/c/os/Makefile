target=`cat target.txt`
run:
	make $(target)
	./$(target)

compile=gcc -g

#################################  lab 4  #################################

lab_4: lab_4.o common.o
	gcc -o lab_4 lab_4.o common.o
	echo "done lab 4"

lab_4.o: lab_4.c common.h
	gcc -c lab_4.c
	echo "done lab 4.o"

common.o: common.h common.c
	gcc -c common.c
	echo "done common.o"

#################################  lab 5  #################################

lab_5: lab_5.c common.h common.c
	gcc -o lab_5 lab_5.c common.c

#################################  lab 6  #################################

lab_6: lab_6.c common.h common.c
	$(compile) -o lab_6 lab_6.c common.c
  # Если я все правильно понял (не очень уверен если честно) то для
  # того чтобы процесс мог работать с разделенной памятью ему нужно
  # иметь на это права которые по умолчанию не доступны.
	sudo setcap cap_ipc_lock=ep lab_6

#################################  lab 7  #################################

lab_7: lab_7.c common.h common.c
	gcc -o lab_7 lab_7.c common.c

#################################  lab 8  #################################

lab_8: lab_8.c common.h common.c
	gcc -o lab_8 lab_8.c common.c
