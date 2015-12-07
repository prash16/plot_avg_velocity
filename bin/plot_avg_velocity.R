# Tiffany Timbers
# December 7, 2015
#
# This program creates a plot of average velocity (ms) versus position (um) with variability 
# represented as +/- SEM from Velocity_vs_position_forward.txt file outputs from Kymograph Direct.

main <- function(){
  
  # assign command line arguments
  args <- commandArgs(trailingOnly = TRUE)
  output_filename <- args[1]
  input_files <- args[2:length(args)]
  
  print(input_files)
  
}

main()