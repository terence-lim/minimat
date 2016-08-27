all : minimat gnuplot tests examples 


# make noplot -- if gnuplot not installed
noplot : minimat tests examples-noplot


minimat:
	cd src && $(MAKE)

examples: minimat
	cd examples && $(MAKE)

examples-noplot: minimat
	cd examples && $(MAKE) noplot

tests: minimat
	./testmm.sh && ./testall.sh

gnuplot:
	cd include/gnuplot_i && $(MAKE)

clean:
	@rm -f ./testall.log
	@rm -f ./testmm.log
	@rm -f ./*.out
	@rm -f ./*.err
	@rm -f ./*.diff
	@rm -f ./*.ll
	@rm -f ./*.o
	@rm -f ./*.s
	@rm -f ./*~
	@cd src/ && $(MAKE) clean; cd ../
	@cd examples/ && $(MAKE) clean; cd ../
	@cd include/gnuplot_i/ && $(MAKE) clean; cd ../../

# Building the tarball

TARFILES = doc/final.pdf examples/ src/ tests/ include/ testmm.sh testall.sh mmc mmc-noplot llci Makefile README

tarball : 
	tar czvf minimat-llvm.tar.gz $(TARFILES)

