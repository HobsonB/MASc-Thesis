dev.off()
rm(list=ls())
cat("\014")

#----Predicted Daytype----

#Determines what the forecast predicted via electrical load during the implementation

#Author(s): Brodie W. Hobson

#Set working directory
setwd("C:\\Users\\hobso\\OneDrive - Carleton University\\Working Directory\\Research Project WD\\Phase 3")

#Load or install required packages
if ("lubridate" %in% rownames(installed.packages()) == FALSE) {install.packages("lubridate")}
require(lubridate)
if ("matrixStats" %in% rownames(installed.packages()) == FALSE) {install.packages("matrixStats")}
require(matrixStats)

dat <- read.csv(file = "C:\\Users\\hobso\\OneDrive - Carleton University\\Working Directory\\Research Project WD\\Phase 3\\Data\\eSim\\submeter_2019.csv")
dat[,1] <- as.POSIXct(dat[,1], format = "%Y-%m-%d %H:%M", tz = "GMT")

day_dat <- list()
for(i in unique(yday(dat[,1]))){
  temp <- rowSums(dat[which(yday(dat[,1])==i),c(2:14)])
  day_dat[[length(day_dat)+1]] <- temp
}
day_dat <- as.data.frame(matrix(unlist(day_dat), byrow = F, ncol = length(unique(yday(dat[,1])))))
colnames(day_dat) <- dat[seq(1,4007,24),1]

profiles <- read.csv(file = "C:\\Users\\hobso\\OneDrive - Carleton University\\Working Directory\\Research Project WD\\Phase 3\\Data\\eSim\\kmeans_elec.csv")
occ_ind <- read.csv(file = "C:\\Users\\hobso\\OneDrive - Carleton University\\Working Directory\\Research Project WD\\Phase 3\\Data\\eSim\\daytypes.csv")$Forecast

obs <- vector()

for(i in 1:4){
  
  obs[length(obs)+1] <- length(which(occ_ind==i))
  
}

my_grey <- rgb(0,0,0, alpha = 80, maxColorValue = 255)
my_red <- rgb(255,0,0, alpha = 40, maxColorValue = 255)
my_green <- rgb(0,202,14, alpha = 40, maxColorValue = 255)
my_blue <- rgb(22,93,239, alpha = 40, maxColorValue = 255)
my_yellow <- rgb(255,233,0, alpha = 40, maxColorValue = 255)
my_col <- c(my_red,my_yellow,my_blue,my_green)

prof_letter <- c('h','m','l','L')

legend.text <- vector()

for(i in 1:4){
  
  temp <- paste0("'", prof_letter[i], "', n = ", obs[i])
  
  legend.text <- c(legend.text, temp)
  
}
rm(i)

png(filename = "measured_daytype.png", width = 2.75, height = 2.75, units = 'in', res = 600)
par(mai = c(0.6,0.6,0.05,0.05), mgp = c(0,0.6,0))

plot(x=c(0:24), y=c(0:24), xlim = c(0,24), ylim= c(15,45), type = "n", axes = "F", 
     main = "", xlab = "", ylab = "")

for(i in 1:4){
  temp <- as.data.frame(day_dat[,which(occ_ind==i)])
  for(j in 1:ncol(temp)){
    x <- spline(x=c(0:23), y=temp[,j])$x
    y <- spline(x=c(0:23), y=temp[,j])$y
    y[which(y<0)] <- 0
    lines(x = x, y = y, col = my_col[i])
  }
}

my_col <- c('red','yellow','blue','green')
for(i in 1:ncol(profiles)){
  x <- spline(x=c(0:23), y=profiles[,i])$x
  y <- spline(x=c(0:23), y=profiles[,i])$y
  y[which(y<0)] <- 0
  lines(x = x, y = y, col = noquote(my_col[i]), lwd = 3)
}

axis(side = 1, seq(from = 0, to = 23, by = 23/24), labels = rep("",25), tck = -0.02)
axis(side = 1, seq(from = 0, to = 23, by = 6*23/24), labels = c("00:00","06:00","12:00","18:00","23:55"), cex.axis = 0.6)
axis(side = 2, at = seq(15,45,2.5), labels = rep("",length(seq(15,45,2.5))), tck = -0.02)
axis(side = 2, at = seq(15,45,5), labels = seq(15,45,5), cex.axis = 0.6, las = 2)

title(ylab = "Electrical Load (kW)", line = 2, cex.lab = 0.6)

legend("topleft", legend = legend.text, bty = "n", col = my_col, lwd = 3, cex = 0.6)

dev.off()

#------------------------------------------------------------------------------------

dev.off()
rm(list=ls())
cat("\014")

#----Actual Daytype ----

#Determine what each daytype was during the study period via Wi-Fi device counts

#Author(s): Brodie W. Hobson

#Set working directory
setwd("C:\\Users\\hobso\\OneDrive - Carleton University\\Working Directory\\Research Project WD\\Phase 3")

#Load or install required packages
if ("lubridate" %in% rownames(installed.packages()) == FALSE) {install.packages("lubridate")}
require(lubridate)
if ("matrixStats" %in% rownames(installed.packages()) == FALSE) {install.packages("matrixStats")}
require(matrixStats)

dat <- read.csv(file = "C:\\Users\\hobso\\OneDrive - Carleton University\\Working Directory\\Research Project WD\\Phase 3\\Data\\eSim\\wifi_2019.csv")
dat[,1] <- as.POSIXct(dat[,1], format = "%Y-%m-%d %H:%M", tz = "GMT")

day_dat <- list()
for(i in unique(yday(dat[,1]))){
  temp <- dat[which(yday(dat[,1])==i),2]
  min_count <- quantile(temp[c(1:7,22:24)], probs = 0.75)
  temp <- round((temp - min_count)*0.88, digits = 0)
  temp[which(temp < 0)] <- 0
  day_dat[[length(day_dat)+1]] <- temp
}
day_dat <- as.data.frame(matrix(unlist(day_dat), byrow = F, ncol = length(unique(yday(dat[,1])))))
colnames(day_dat) <- dat[seq(1,4007,24),1]

colnames(day_dat)[125]

profiles <- read.csv(file = "C:\\Users\\hobso\\OneDrive - Carleton University\\Working Directory\\Research Project WD\\Phase 3\\Data\\eSim\\kmeans_profiles.csv")
centers <- read.csv(file = "C:\\Users\\hobso\\OneDrive - Carleton University\\Working Directory\\Research Project WD\\Phase 3\\Data\\eSim\\kmeans_centers.csv")

occ_ind <- vector()
for(i in 1:ncol(day_dat)){
  occ_ind[[length(occ_ind)+1]] <- as.numeric(which.min(colSums((centers[c(7:21),] - day_dat[c(7:21),i])^2)))
}

obs <- vector()

for(i in 1:4){
  
  obs[length(obs)+1] <- length(which(occ_ind==i))
  
}

my_grey <- rgb(0,0,0, alpha = 80, maxColorValue = 255)
my_red <- rgb(255,0,0, alpha = 40, maxColorValue = 255)
my_green <- rgb(0,202,14, alpha = 40, maxColorValue = 255)
my_blue <- rgb(22,93,239, alpha = 40, maxColorValue = 255)
my_yellow <- rgb(255,233,0, alpha = 40, maxColorValue = 255)
my_col <- c(my_red,my_yellow,my_blue,my_green)

prof_letter <- c('h','m','l','L')

legend.text <- vector()

for(i in 1:4){
  
  temp <- paste0("'", prof_letter[i], "', n = ", obs[i])
  
  legend.text <- c(legend.text, temp)
  
}

png(filename = "actual_daytype.png", width = 2.75, height = 2.75, units = 'in', res = 600)
par(mai = c(0.6,0.6,0.05,0.05), mgp = c(0,0.6,0))

plot(x=c(0:24), y=c(0:24), xlim = c(0,24), ylim= c(0,600), type = "n", axes = "F", 
     main = "", xlab = "", ylab = "")

for(i in 1:4){
  temp <- subset(day_dat[,which(occ_ind==i)])
  for(j in 1:ncol(temp)){
    x <- spline(x=c(0:23), y=temp[,j])$x
    y <- spline(x=c(0:23), y=temp[,j])$y
    y[which(y<0)] <- 0
    lines(x = x, y = y, col = my_col[i])
  }
}

my_col <- c('red','yellow','blue','green')
for(i in 1:ncol(profiles)){
  x <- spline(x=c(0:23), y=profiles[,i])$x
  y <- spline(x=c(0:23), y=profiles[,i])$y
  y[which(y<0)] <- 0
  lines(x = x, y = y, col = noquote(my_col[i]), lwd = 3)
}

axis(side = 1, seq(from = 0, to = 23, by = 23/24), labels = rep("",25), tck = -0.02)
axis(side = 1, seq(from = 0, to = 23, by = 6*23/24), labels = c("00:00","06:00","12:00","18:00","23:55"), cex.axis = 0.6)
axis(side = 2, at = seq(from = 0, to = 600, by = 50), labels = rep("",length(seq(from = 0, to = 600, by = 50))), tck = -0.02)
axis(side = 2, at = seq(from = 0, to = 600, by = 100), labels = seq(from = 0, to = 600, by = 100), cex.axis = 0.6, las = 2)

title(ylab = "Occupancy (Persons)", line = 2, cex.lab = 0.6)

legend("topleft", legend = legend.text, bty = "n", col = my_col, lwd = 3, cex = 0.6)

dev.off()