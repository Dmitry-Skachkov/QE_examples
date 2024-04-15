FC = ifort
FFLAGS = 

obj/bs.o: bs.f90
	@mkdir -p obj
	$(FC) $(FFLAGS) -c bs.f90            -o obj/bs.o           -module obj

obj/Plot_bands.o: Plot_bands.f90
	$(FC) $(FFLAGS) -c Plot_bands.f90       -o obj/Plot_bands.o      -module obj

obj/qe_band_plot.o: qe_band_plot.f90
	$(FC) $(FFLAGS) -c qe_band_plot.f90      -o obj/qe_band_plot.o     -module obj
