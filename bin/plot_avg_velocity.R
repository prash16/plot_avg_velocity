# Tiffany Timbers
# December 7, 2015
#
# This program creates a plot of average velocity (ms) versus position (um) with variability 
# represented as +/- SEM from Velocity_vs_position_forward.txt file outputs from Kymograph Direct.

# import libraries
require(reshape)
require(stringr)
require(plyr)
require(dplyr)
require(tidyr)
require(ggplot2)
require(testit)

main <- function(){
  
  # assign command line arguments
  args <- commandArgs(trailingOnly = TRUE)
  
  # check that there are at least 2 command line inputs
  assert("there should be at least 2 command line inputs, 1) filename to save figure to and 2) at least 1 Velocity_vs_position_forward.txt", length(args) > 1)
  
  output_filename <- args[1]
  input_files <- args[2:length(args)]
  # output_filename <- "results/test.pdf"
  # input_files <- c("./data/B6/kymograph/kymograph_1/Results/Velocity_vs_position_forward.B6", "./data/test/kymograph/kymograph_1/Results/Velocity_vs_position_forward.test")
  
  # initialize dataframe with correct column names and add an extra column for worm #
  velocities <- data.frame(strain=character(), wormID=character(), position=double(), measureID=integer(),velocity=double()) 
  
  # load all data to be plotted
  for (i in 1:length(input_files)){
    # load data
    temp_data <- read.table(input_files[i], sep = '\t', header=TRUE)
    
    # append strain to a column
    strain_ID <- str_match(input_files[i], 'Velocity_vs_position_forward\\.(.+)')
    temp_data$strain <- rep(strain_ID[2], dim(temp_data)[1])
    
    # append worm ID to a column
    temp_data$wormID <- rep(i, dim(temp_data)[1])
    
    # munge data from wide to long
    long_temp_data <- wide_to_long_velocity(temp_data)
    
    # append dataframe to general dataframe
    velocities <- rbind(velocities, long_temp_data)
  }
  
  # plot and save data
  vel_plot <- plot_avg_vel(velocities)
  ggsave(filename = output_filename, plot = vel_plot, width = 4, height = 3)
}

# Function to transform wide data from a Velocity_vs_position_forward file 
# (from Kymograph Direct program) where each position has a column for velocity 
# and for position, and make this a wide dataframe with columns: strain, wormID,
# position, measureID and velocity
wide_to_long_velocity <- function(df){
  # check that the input to the function is of type dataframe
  assert('The input to wide_to_long_velocity() should be a dataframe', typeof(df) == 'list')
  
  # check that correct dataframe format (e.g. a Velocity_vs_position_forward file)
  # was passed into the function
  assert('one of the input files was not a Velocity_vs_position_forward file', colnames(df)[1] == 'position1' & colnames(df)[2] == 'velocity1' & colnames(df)[3] == 'position2' & colnames(df)[4] == 'velocity2')
  
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
  colnames(df_pos) <- c('strain', 'wormID', 'position', 'measureID')
  
  # get a dataframe that is only velocities
  df_vel <- df[df$obstype == 'velocity',]
  # remove position column
  df_vel$obstype <- NULL
  # rename columns
  colnames(df_vel) <- c('strain', 'wormID', 'velocity', 'measureID')
  
  # join temp_vel & temp_pos
  long_df <- full_join(df_pos, df_vel)
  
  # remove rows with NAs
  long_df <- long_df[complete.cases(long_df),]
  
  return(long_df)
}

# plot mean particle velocity over position for each strain and represent variance as +/- SEM
# bin into position intervals to make it quicker to plot (average velocity over every 0.20 um)
# expects a dataframe with the following columns: strain, wormID,
# position, measureID and velocity (this can be the output of wide_to_long_velocity() function)
plot_avg_vel <- function(dataframe) {
  # check that the input to the function is of type dataframe
  assert('The input to plot_avg_vel() should be a dataframe', typeof(dataframe) == 'list')
  # check that the correct format of dataframe was passed to the function
  assert('The input to plot_avg_vel() should be a dataframe with columns: strain, wormID, position, measureID and velocity', colnames(dataframe) == c('strain', 'wormID', 'position', 'measureID', 'velocity'))
  
  # divide distance  into intervals (e.g. 0.2) to the last position
  cut1 <- cut(dataframe$position, breaks=seq(0, max(dataframe$position), by = 0.2))
  
  # converts these to numbers & replace position column with the position interval
  dist.interval <- as.numeric(str_extract(cut1, "[1-9]{1}[0-9.]+"))
  dataframe.dint <- dataframe
  dataframe.dint$position <- dist.interval
  
  # average over each strain for each time period
  vel.dint.strain <- ddply(dataframe.dint,.(strain,position),summarise,N=length(position),mean.velocity=mean(velocity),sd=sd(velocity), se=sd/sqrt(N))
  
  ##make plot with error bars
  g  <- ggplot(vel.dint.strain, aes(x = position, y = mean.velocity, colour = strain)) + 
    geom_errorbar(aes(ymin = mean.velocity-se, ymax = mean.velocity+se), width = 0.1) +
    geom_line(aes(group = strain)) + 
    geom_point() +
    labs(x="Position (um)", y="Velocity (um/us)") +
    scale_y_continuous(limits = c(0, 1.2)) +
    #theme_bw(legend.title=element_blank())
    theme(plot.title = element_text(size = 16, vjust=2), ## Make the plot title larger and higher
          legend.title=element_blank(), ## remove the legend label
          legend.key=element_rect(fill='white'), ## remove the blocks around the legend items
          legend.text=element_text(size = 12), ## make the legend text font larger
          panel.background = element_rect(fill = 'grey96'), ## make the plot background grey
          axis.text.x=element_text(colour="black", size = 12), ## change the x-axis values font to black and make larger
          axis.text.y=element_text(colour="black", size = 12), ## change the y-axis values font to black and make larger
          axis.title.x = element_text(size = 12, vjust = -0.2), ## change the x-axis label font to black, make larger, and move away from axis
          axis.title.y = element_text(size = 12, vjust = 1.3)) ## change the y-axis label font to black, make larger, and move away from axis
  
  return(g)
}

main()