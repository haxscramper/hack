main:
	verilator -Wall --trace -cc alu.sv --exe tb_alu.cpp
	make -C obj_dir -f Valu.mk Valu
	echo "Executing value"
	./obj_dir/Valu
