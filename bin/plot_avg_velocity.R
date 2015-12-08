# Tiffany Timbers
# December 7, 2015
#
# This program creates a plot of average velocity (ms) versus position (um) with variability 
# represented as +/- SEM from Velocity_vs_position_forward.txt file outputs from Kymograph Direct.

# import libraries
require(reshape)
require(stringr)
require(dplyr)
require(tidyr)
require(ggplot2)

main <- function(){
  
  # assign command line arguments
  args <- commandArgs(trailingOnly = TRUE)
  output_filename <- args[1]
  input_files <- args[2:length(args)]
  # output_filename <- "test.pdf"
  # input_files <- c("./data/B6/kymograph/kymograph_1/Results/Velocity_vs_position_forward.txt", "./data/test/kymograph/kymograph_1/Results/Velocity_vs_position_forward.txt")
  
  # initialize dataframe with correct column names and add an extra column for worm #
  velocities <- data.frame(wormID=character(), position=double(), measureID=integer(),velocity=double()) 
  
  # load all data to be plotted
  for (i in 1:length(input_files)){
    # load data
    temp_data <- read.table(input_files[i], sep = '\t', header=TRUE)
    
    # append worm ID to a column
    temp_data$wormID <- rep(i, dim(temp_data)[1])
    
    # append strain to a column
    #strain_ID <- str_extract(input_files[i], 'somepattern')
    #temp_data$strain <- rep(strain_ID, dim(temp_data)[1])
    
    # munge data from wide to long
    long_temp_data <- wide_to_long_velocity(temp_data)
    
    # append dataframe to general dataframe
    velocities <- rbind(velocities, long_temp_data)
  }
  
  # plot and save data
  vel_plot <- plot_avg_vel(velocities)
  ggsave(filename = output_filename, plot = vel_plot)
}

wide_to_long_velocity <- function(df){
  
  # make data long format
  # first make all long
  df <- df %>% gather(obstype,obs_values,starts_with('pos'),starts_with('vel'))
  # next do regex to split velocity and position from number, making the number a new column
  df$measureID <- str_extract(as.character(df$obstype), '[0-9]+')
  df$obstype <- sub('[0-9]+', '', df$obstype)
  
  # get a dataframe that is only positions
  df_pos <- df[df$obstype == 'position',]
  # remove position column
  df_pos$obstype <- NULL
  # rename columns
  colnames(df_pos) <- c('wormID', 'position', 'measureID')
  
  # get a dataframe that is only velocities
  df_vel <- df[df$obstype == 'velocity',]
  # remove position column
  df_vel$obstype <- NULL
  # rename columns
  colnames(df_vel) <- c('wormID', 'velocity', 'measureID')
  
  # join temp_vel & temp_pos
  long_df <- full_join(df_pos, df_vel)
  
  return(long_df)
}

plot_avg_vel <- function(df) {
  plot_object <- ggplot(df, aes(position, velocity)) +
    geom_smooth() +
    scale_y_continuous(limits = c(0, 1.2)) +
    geom_vline(aes(xintercept = c(1, 4))) +
    geom_text(aes(x,y, label = "TZ-MS"), 
              data = data.frame(x = 0.9, y = 0.05), size = 3, hjust = 0,
              vjust = 0, angle = 90) +
    geom_text(aes(x,y, label = "MS-DS"), 
              data = data.frame(x = 3.9, y = 0.05), size = 3, hjust = 0, 
              vjust = 0, angle = 90)
  
  return(plot_object)
}

main()