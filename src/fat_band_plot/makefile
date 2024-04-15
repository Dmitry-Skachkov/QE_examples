default: fat_band_plot

include intel.make                    # use Intel compiler

OBJS =   obj/regridpack_module.o   obj/pdos.o   obj/Kpath_module.o  obj/Plot_bands.o   obj/fat_band_plot.o     

fat_band_plot: $(OBJS)
	$(FC) $(FFLAGS) -o ~/bin/fat_band_plot $(OBJS)

clean:
	rm -rf ~/bin/fat_band_plot obj/*.o obj/*.mod

Plot_bands.o:           Kpath_module.o   regridpack_module.o   pdos.o
fat_band_plot.o:        Kpath_module.o   Plot_bands.o          pdos.o



