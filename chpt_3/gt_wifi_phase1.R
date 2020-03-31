dev.off()
rm(list=ls())
cat("\014")

#----Groundtruth and Wi-Fi Data from Phase 1----

#Produces a summary table of the Wi-Fi and groundtruth data across floors studied in phase 1

#Author(s): Brodie W. Hobson

#Set working directory
setwd("C:\\Users\\hobso\\OneDrive - Carleton University\\Working Directory\\Research Project WD\\Phase 2")

floors <- c("A","AS","B","BS","C","D")

for (i in c(1:6)){
  
  file_nam <- paste0("C:\\Users\\hobso\\OneDrive - Carleton University\\Working Directory\\Research Project WD\\Phase 1\\Data\\Floor ",floors[i],".csv")
  
  nam <- paste0("dat_",floors[i])
  
  dat <- read.csv(file = file_nam)
  
  dat <- cbind.data.frame(dat$Time,dat$devices,dat$groundtruth)
  
  colnames(dat) <- c("Time","DC","Groundtruth")
  
  dat[,1] <- as.POSIXct(dat[,1], format = "%Y-%m-%d %H:%M", tz = "GMT")
  
  assign(nam,dat)
  
  rm(dat)
}

dat <- rbind.data.frame(dat_A,dat_AS,dat_B,dat_BS,dat_C,dat_D)

write.csv(dat, "gt_wifi_phase1.csv", row.names = F)