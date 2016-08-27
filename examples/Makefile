MMC = ./mmc-demo
MMC-NOPLOT = ./mmc-demo-noplot

all : 
	$(MMC) hello.mm
	$(MMC) tutorial.mm
	$(MMC) regression.mm
	$(MMC) logistic.mm

noplot :
	$(MMC-NOPLOT) hello.mm
	$(MMC-NOPLOT) tutorial.mm
	$(MMC-NOPLOT) regression.mm

clean :
	rm -rf *.s *.ll *.o *.out *~
	rm -f hello tutorial regression logistic


