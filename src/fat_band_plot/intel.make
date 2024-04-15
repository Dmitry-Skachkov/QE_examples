FC = ifort
FFLAGS = 

obj/Kpath_module.o: Kpath_module.f90
	@mkdir -p obj
	$(FC) $(FFLAGS) -c Kpath_module.f90         -o obj/Kpath_module.o      -module obj

obj/regridpack_module.o: regridpack_module.f90
	$(FC) $(FFLAGS) -c regridpack_module.f90    -o obj/regridpack_module.o -module obj

obj/pdos.o: pdos.f90
	$(FC) $(FFLAGS) -c pdos.f90                 -o obj/pdos.o              -module obj

obj/Plot_bands.o: Plot_bands.f90
	$(FC) $(FFLAGS) -c Plot_bands.f90           -o obj/Plot_bands.o        -module obj

obj/fat_band_plot.o: fat_band_plot.f90
	$(FC) $(FFLAGS) -c fat_band_plot.f90        -o obj/fat_band_plot.o     -module obj

