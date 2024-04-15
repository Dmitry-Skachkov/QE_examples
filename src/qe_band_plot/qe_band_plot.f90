





   Program qe_band_plot                             ! convert bands.dat to Tecplot format for plotting
    use BS
    use Plot
    call read_bands
    call plot_bands                                 ! TecPlot format
 !   call plot_bands_2                               ! .band format
   end Program qe_band_plot










