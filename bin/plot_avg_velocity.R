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
require(plyr)

main <- function(){
  
  # assign command line arguments
  args <- commandArgs(trailingOnly = TRUE)
  output_filename <- args[1]
  input_files <- args[2:length(args)]
  # output_filename <- "test.pdf"
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

#plot_avg_vel <- function(df) {
#  # make strain a factor
#  df$strain <- as.factor(df$strain)
#  
#  plot_object <- ggplot(df, aes(x = position, y = velocity, color = df$strain)) +
#    #geom_point() +
#    geom_smooth() +
#    scale_y_continuous(limits = c(0, 1.2)) +
#    geom_vline(aes(xintercept = c(1, 4))) +
#    geom_text(aes(x,y, label = "TZ-MS"), 
#              data = data.frame(x = 0.9, y = 0.05), size = 3, hjust = 0,
#              vjust = 0, angle = 90) +
#    geom_text(aes(x,y, label = "MS-DS"), 
#              data = data.frame(x = 3.9, y = 0.05), size = 3, hjust = 0, 
#              vjust = 0, angle = 90)
#  
#  return(plot_object)
#}

plot_avg_vel <- function(dataframe) {
  
  ##plot particle velocity over position
  ##bin into position intervals to make it quicker to plot (average velocity over every 0.20 um)
  
  ##divide time into intervals (e.g. 0.2) to the last position
  cut1 <- cut(dataframe$position, breaks=seq(0, max(dataframe$position), by = 0.2))
  
  ##converts these to numbers
  dist.interval <- as.numeric(str_extract(cut1, "[1-9]{1}[0-9.]+"))
  
  dataframe.dint <- dataframe
  
  ##replace position column with the position interval
  dataframe.dint$position <- dist.interval
  
  ##average over each strain for each time period
  vel.dint.strain <- ddply(dataframe.dint,.(strain,position),summarise,N=length(position),mean.velocity=mean(velocity),sd=sd(velocity), se=sd/sqrt(N))
  
  ##make plot with error bars
  g  <- ggplot(vel.dint.strain, aes(x = position, y = mean.velocity, colour = vel.dint.strain$strain)) + 
    geom_errorbar(aes(ymin=mean.velocity-se, ymax=mean.velocity+se), width=.1) +
    geom_line(aes(group = strain)) + geom_point() +
    labs(x="Position (um)", y="Velocity (um/us)") +
    scale_y_continuous(limits = c(0, 1.2)) +
    theme_bw()
  
  return(g)
}

main()