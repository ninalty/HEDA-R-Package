
setwd("D:/Ninalty/UCD_Hydropeaking/HPK_Imagery/HPK_HPKEventsTag/HPK_Annthre_Orign/")

# plot_file <- HPK_SM_Q
plot_file <- data.byLocation

for (i in 1:length(plot_file)) {
  
  kk <- plot_file[[i]]
  
  kk <- kk[500:800,]
  
  sample_plot <- Reversal_Count_plot(kk)
  
  ggsave(paste(kk$location_id[1],".jpg"), plot = sample_plot, width = 40, height = 20, units = "cm")
  
  # ggsave(filename = paste(unique(kk$location_id),"_Reversal.png", sep = ""), plot = sample_plot, path = "D:/Ninalty/UCD_Hydropeaking/HPK_Imagery/HPK_HPKEventsTag/HPK_90%_plt/", width = 40, height = 20, units = "cm")
}


# 161
kk <- plot_file$`161`
kk <- kk[16600:17100,]
sample_plot <- Reversal_Count_plot(kk)
sample_plot

ggsave(paste(kk$location_id[1],".jpg"), plot = sample_plot, width = 40, height = 20, units = "cm")

# 170
kk <- plot_file$`170`
kk <- kk[18800:19300,]
sample_plot <- Reversal_Count_plot(kk)
sample_plot

ggsave(paste(kk$location_id[1],".jpg"), plot = sample_plot, width = 40, height = 20, units = "cm")

# BUL
kk <- plot_file$BUL
kk <- kk[6600:7100,]
sample_plot <- Reversal_Count_plot(kk)
sample_plot

ggsave(paste(kk$location_id[1],".jpg"), plot = sample_plot, width = 40, height = 20, units = "cm")

# oxb
kk <- plot_file$OXB
kk <- kk[50000:50500,]
sample_plot <- Reversal_Count_plot(kk)
sample_plot

ggsave(paste(kk$location_id[1],".jpg"), plot = sample_plot, width = 40, height = 20, units = "cm")

# PMN
kk <- plot_file$PMN
kk <- kk[5100:5600,]
sample_plot <- Reversal_Count_plot(kk)
sample_plot

ggsave(paste(kk$location_id[1],".jpg"), plot = sample_plot, width = 40, height = 20, units = "cm")


# WHI
kk <- plot_file$WHI
kk <- kk[5100:5600,]
sample_plot <- Reversal_Count_plot(kk)
sample_plot

ggsave(paste(kk$location_id[1],".jpg"), plot = sample_plot, width = 40, height = 20, units = "cm")


# CBR
kk <- plot_file$CBR
kk <- kk[5100:5600,]
sample_plot <- Reversal_Count_plot(kk)
sample_plot

ggsave(paste(kk$location_id[1],".jpg"), plot = sample_plot, width = 40, height = 20, units = "cm")

# CLE
kk <- plot_file$CLE
kk <- kk[5100:5600,]
sample_plot <- Reversal_Count_plot(kk)
sample_plot

ggsave(paste(kk$location_id[1],".jpg"), plot = sample_plot, width = 40, height = 20, units = "cm")

# LWS
kk <- plot_file$LWS
kk <- kk[3100:3600,]
sample_plot <- Reversal_Count_plot(kk)
sample_plot

ggsave(paste(kk$location_id[1],".jpg"), plot = sample_plot, width = 40, height = 20, units = "cm")

# MMF
kk <- plot_file$MMF
kk <- kk[3500:4000,]
sample_plot <- Reversal_Count_plot(kk)
sample_plot

ggsave(paste(kk$location_id[1],".jpg"), plot = sample_plot, width = 40, height = 20, units = "cm")

