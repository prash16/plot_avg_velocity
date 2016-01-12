## Plot average velocity
Tiffany Timbers, Dec 7, 2015

### Overview
This program creates a plot of average velocity (ms) versus position (um) with variability
represented as +/- SEM from `Velocity_vs_position_forward.txt` file outputs from 
[Kymograph Direct](https://sites.google.com/site/kymographanalysis/).

### Dependencies
* R version 3.2.2 or higher
* R packages reshape, stringr, plyr, dplyr, tidyr, ggplot2, testit

### How to use
At the terminal type:
~~~
rscript plot_avg_velocity.R  plotname.pdf data/strain_name/kymograph/kymograph_1/Results/Velocity_vs_position_forward.strain_name
~~~

This program can take multiple input files, simply list them, separating each with a 
space. To make this easier for a lot of files, use the Shells find command, for example:

~~~
rscript plot_avg_velocity.R  plotname.pdf $(find data -name Velocity_vs_position_forward*)
~~~

If data are in two separate folders (e.g., on for each strain) use two find commands:

~~~
rscript plot_avg_velocity.R  plotname.pdf $(find data/strain1 -name Velocity_vs_position_forward*) $(find data/strain2 -name Velocity_vs_position_forward*)
~~~