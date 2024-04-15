default: qe_band_plot

include intel.make                    # use Intel compiler

OBJS = obj/bs.o   obj/Plot_bands.o   obj/qe_band_plot.o   

qe_band_plot: $(OBJS)
	$(FC) $(FFLAGS) -o ~/bin/qe_band_plot $(OBJS)

clean:
	rm -rf ~/bin/qe_band_plot obj/*.o obj/*.mod

Plot_bands.o:        bs.o
qe_band_plot.o:      bs.o   Plot_bands.o



