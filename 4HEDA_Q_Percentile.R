# change the directory
setwd("D:/Ninalty/UCD_Hydropeaking/HPK_Imagery/HPK_HPKEventsTag/")

# get the data
plot_file <- read.csv(...)

# plot figures. This code automatically truncated records in 500-800. Is there a way to make the user be able to change the value.
plot_file <- plot_file[500:800,]
  
sample_plot <- Reversal_Count_plot(plot_file)
  
  
ggsave(filename = paste(unique(plot_file$location_id),"_Reversal.png", sep = ""), plot = sample_plot, width = 40, height = 20, units = "cm")



