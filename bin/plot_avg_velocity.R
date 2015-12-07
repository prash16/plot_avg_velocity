# Tiffany Timbers
# December 7, 2015
#
# This program creates a plot of average velocity (ms) versus position (um) with variability 
# represented as +/- SEM from Velocity_vs_position_forward.txt file outputs from Kymograph Direct.

# import libraries
require(reshape)
require(stringr)
require(dplyr)

main <- function(){
  
  # assign command line arguments
  args <- commandArgs(trailingOnly = TRUE)
  output_filename <- args[1]
  input_files <- args[2:length(args)]
  # output_filename <- "test.pdf"
  # input_files <- c("./data/B6/kymograph/kymograph_1/Results/Velocity_vs_position_forward.txt", "./data/test/kymograph/kymograph_1/Results/Velocity_vs_position_forward.txt")
  
  # initialize dataframe with correct column names and add an extra column for worm #
  velocities <- data.frame(wormID=character(), position=double(), measureID=integer(),velocity=double()) 
  
  # load data
  for (i in 1:length(input_files)){
    # load data
    temp_data <- read.table(input_files[i], sep = '\t', header=TRUE)
    
    # append worm ID to a column
    temp_data$wormID <- rep(i, dim(temp_data)[1])
    
    # munge data from wide to long
    long_temp_data <- wide_to_long_velocity(temp_data)
    
    # append dataframe to general dataframe
    velocities <- rbind(velocities, long_temp_data)
  }
  # load data
  #temp_data <- read.table(input_files[1], sep = '\t', header=TRUE, stringsAsFactors = FALSE)
  
  
  # append dataframe to general dataframe
  
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

main()