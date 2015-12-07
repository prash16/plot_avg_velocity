## Plot average velocity
Tiffany Timbers, Dec 7, 2015

### Overview
This program creates a plot of average velocity (ms) versus position (um) with variability
represented as +/- SEM from `Velocity_vs_position_forward.txt` file outputs from 
[Kymograph Direct](https://sites.google.com/site/kymographanalysis/).

### Dependencies
* R version 3.2.1 or higher

### How to use
At the terminal type:
~~~
rscript plot_avg_velocity.R  --input data/test/kymograph/kymograph_1/Results/Velocity_vs_position_forward.txt --output plotname.pdf
~~~

Note - this program can take multiple input files, simply pass them after `--input` with a 
space after the name of each.

## Note - this code is a work in progress and does not yet work