dev.off()
rm(list=ls())
cat("\014")

#----Artificial Occupancy for Phase 2----

#Produce occupancy-count estimates for phase 2 based on linear model from phase 1

#Author(s): Brodie W. Hobson

#Set working directory
setwd("C:\\Users\\hobso\\OneDrive - Carleton University\\Working Directory\\Research Project WD\\Phase 2")

#Load or install required packages
if ("lubridate" %in% rownames(installed.packages()) == FALSE) {install.packages("lubridate")}
require(lubridate)
if ("BBmisc" %in% rownames(installed.packages()) == FALSE) {install.packages("BBmisc")}
require(BBmisc)
if ("DescTools" %in% rownames(installed.packages()) == FALSE) {install.packages("DescTools")}
require(DescTools)

#Read in groundtruth and Wi-Fi data
dat <- read.csv(file = paste0("C:\\Users\\hobso\\OneDrive - Carleton University\\Working Directory\\Research Project WD\\Phase 2\\Data\\JP\\wifi_elec_2018.csv"))

#Convert date information to POSIXct
dat[,1] <- as.POSIXct(dat[,1], format = "%Y-%m-%d %H:%M", tz = "GMT")

#----Artificial Occupancy Data----

#Create a blank list to store daily occupancy profile data
day.dat <- list()

#Create daily, hourly data for clustering purposes
for (i in c(yday(dat[1,1]):yday(dat[nrow(dat),1]))){
  
  temp <- subset(dat[,2], (yday(dat[,1]) == i))
  
  temp <- temp[seq(1,288,12)]
  
  temp <- (temp - (1/1.2)*min(temp[c(22:24)]))
  
  temp[which(temp < 0)] <- 0
  
  temp <- round(temp*0.877,0)
  
  day.dat[[length(day.dat)+1]] <- temp
  
  rm(temp)
}


gt_wifi_phase2 <- cbind.data.frame(dat[seq(1,59328,12),1],unlist(day.dat))
colnames(gt_wifi_phase2) <- c("Time","Occupancy")

write.csv(gt_wifi_phase2, 'gt_wifi_phase2.csv', row.names = F)

day.dat <- as.data.frame(matrix(unlist(day.dat), ncol = length(day.dat), byrow = F))
colnames(day.dat) <- as.Date(c(1:length(day.dat))-1, origin = dat[1,1])

write.csv(day.dat, 'gt_day_dat.csv', row.names = F)

#----Electrical Load Data----

#Create a blank list to store daily occupancy profile data
day.dat <- list()

#Create daily, hourly data for clustering purposes
for (i in c(yday(dat[1,1]):yday(dat[nrow(dat),1]))){
  
  temp <- subset(dat[,3], (yday(dat[,1]) == i))
  
  temp <- temp[seq(1,288,12)]
  
  day.dat[[length(day.dat)+1]] <- temp
  
  rm(temp)
}

day.dat <- as.data.frame(matrix(unlist(day.dat), ncol = length(day.dat), byrow = F))
colnames(day.dat) <- as.Date(c(1:length(day.dat))-1, origin = dat[1,1])

write.csv(day.dat, 'elec_day_dat.csv', row.names = F)