all:
	cd src; ${MAKE} -f Makefile; ./gt < ../mlcf.gra; make emitted; ./mlcf ../functions/*.mlcf; mv dump_gt_Make ../../src/Makefile; mv dump_gt_main.ml ../../src/main.ml; mv dump_mlcf_* ../../src; make clean; 

#rm mlcf*; rm most_*; rm parse.out*

clean:
	cd src; make clean
