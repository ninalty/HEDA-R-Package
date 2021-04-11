# set working directory
HPK_Path <- "D:/Ninalty/UCD_Hydropeaking/HPK_FlowData/HPK_RDS/HPK_OG/HPK_sm_OG/"

site_list <- list.files(path = HPK_Path, "_sm.csv")

for (j in 1:length(site_list)) {
  
  kk = read.csv(paste(HPK_Path, site_list[j], sep = ""))
  
  # get the smoothed data
  kk <- HPK_Count_Pre(kk)
  
  #--------------------Step1 Smooth the data----------------------
  
  # adjust the flow data
  kk <- Q_adj_bydift(kk)
  
  # calculate vector angle
  kk2 <- vector_angle(kk)
  
  #--------------------Step 2 tag reversal points------------
  # tag the four types of point
  kk2 <- hpk_up_dw(kk2)
  
  #------------------- Step 3 clean noises------------------------
  # clean the green point
  kk2_ct <- clean_position(kk2)
  
  # clean the black point
  kk2_ct <- clean_Spt(kk2_ct)
  
  #again
  kk2_ct <- clean_Spt(kk2_ct)
  
  # clean low retention pairs
  kk2_ct <- clean_conectD(kk2_ct)
  
  # clean the contin pts again
  kk2_ct <- clean_smfluc(kk2_ct)
  
  #again
  kk2_ct <- clean_Spt(kk2_ct)  
  
  #save the data
  saveRDS(kk2_ct, paste(HPK_Path, kk2_ct$location_id[1], "_ct.rds", sep = ""))
  
}

# end
print("HEDA Done!")