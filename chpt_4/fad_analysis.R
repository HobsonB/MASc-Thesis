dev.off()
rm(list=ls())
cat("\014")

#----Fresh Air Damper Analysis----

#Analysis of fresh air damper before and after implementation

#Author(s): Brodie W. Hobson

#Set working directory
setwd("C:\\Users\\hobso\\OneDrive - Carleton University\\Working Directory\\Research Project WD\\Phase 3")

#Load or install required packages
if ("lubridate" %in% rownames(installed.packages()) == FALSE) {install.packages("lubridate")}
require(lubridate)
if ("matrixStats" %in% rownames(installed.packages()) == FALSE) {install.packages("matrixStats")}
require(matrixStats)
if ("BBmisc" %in% rownames(installed.packages()) == FALSE) {install.packages("BBmisc")}
require(BBmisc)
if ("dplyr" %in% rownames(installed.packages()) == FALSE) {install.packages("dplyr")}
require(dplyr)

file_nam <- c("AHU1_2018","AHU1_2019", "AHU2_2018", "AHU2_2019")

for(i in file_nam){
  
  dat <- read.csv(file = paste0("C:\\Users\\hobso\\OneDrive - Carleton University\\Working Directory\\Research Project WD\\Phase 3\\Data\\eSim\\",i,".csv"))
  dat[,1] <- as.POSIXct(dat[,1], format = "%Y-%m-%d %H:%M", tz = "GMT")
  nam <- i
  assign(nam,dat)
  
}
rm(dat,nam,i)

fads1_2018 <- AHU1_2018$CS_0PA1RDMFA_TL[which(hour(AHU1_2018[,1]) %in% c(8:20) & wday(AHU1_2018[,1]) %in% c(2:6))]
fads1_2019 <- AHU1_2019$CS_0PA1RDMFA_TL[which(hour(AHU1_2019[,1]) %in% c(8:20) & wday(AHU1_2019[,1]) %in% c(2:6))]
fads2_2018 <- AHU2_2018$CS_0PA1RDMFA_TL[which(hour(AHU1_2018[,1]) %in% c(8:20) & wday(AHU1_2018[,1]) %in% c(2:6))]
fads2_2019 <- AHU2_2019$CS_0PA1RDMFA_TL[which(hour(AHU1_2019[,1]) %in% c(8:20) & wday(AHU1_2019[,1]) %in% c(2:6))]

fads_2018 <- c(unlist(fads1_2018,fads2_2018))
fads_2019 <- c(unlist(fads1_2019,fads2_2019))

y_up <- 1440*0.2
y_low <- 0
x_up <- 100
x_low <- 0

my_red <- rgb(255,0,0, alpha = 40, maxColorValue = 255)
my_grey <- rgb(0,0,0, alpha = 80, maxColorValue = 255)

png(filename = "fad_hist.png", width = 2.75, height = 2.75, units = 'in', res = 600)
par(mai = c(0.6,0.6,0.05,0.05), mgp = c(0,0.6,0))

plot(x=1,y=1,xlab = "", ylab = "", main = "", xlim = c(x_low,x_up), ylim = c(y_low, y_up), axes = F, cex.lab = 0.6, type = 'n')

par(new = T)
hist(fads_2018, breaks = seq(0,100,5), col = my_grey,xlab = "", ylab = "", main = "",xlim = c(x_low,x_up), ylim = c(y_low, y_up), axes = F, border = F)
par(new = T)
hist(fads_2019, breaks = seq(0,100,5), col = my_red,xlab = "", ylab = "", main = "",xlim = c(x_low,x_up), ylim = c(y_low, y_up), axes = F, border = F)

axis(side = 1, seq(x_low,x_up,10), labels = rep("",length(seq(x_low,x_up,10))), tck = -0.02)
axis(side = 1, seq(x_low,x_up,20), labels = seq(x_low,x_up,20), cex.axis = 0.6)
axis(side = 2, seq(y_low,y_up,y_up/20), labels = rep("",length(seq(y_low,y_up,y_up/20))), tck = -0.02)
axis(side = 2, seq(y_low,y_up,y_up/5), labels = seq(0,0.2,0.04), cex.axis = 0.6, las = 2)

title(xlab = "OA Damper Position (%)", line = 1.8, cex.lab = 0.6)
title(ylab = "Frequency (%)", line = 2, cex.lab = 0.6)

par(new = T)
rho <- density(fads_2018, adjust = 0.75)
plot(x=0,y=0, type = "n", xlim = c(x_low,x_up), ylim = c(0, 1), axes = F, main = "", xlab = "", ylab = "") 
rho$y <- rho$y*(100/sum(rho$y))
lines(rho$x[which(between(rho$x,x_low,x_up))], rho$y[which(between(rho$x,x_low,x_up))], lwd = 2)

par(new = T)
rho <- density(fads_2019, adjust = 0.75)
plot(x=0,y=0, type = "n", xlim = c(x_low,x_up), ylim = c(0, 1), axes = F, main = "", xlab = "", ylab = "") 
rho$y <- rho$y*(100/sum(rho$y))
lines(rho$x[which(between(rho$x,x_low,x_up))], rho$y[which(between(rho$x,x_low,x_up))], lwd = 2, col = "red")

legend("topleft", legend = c("Before","After"), bty = 'n', cex = 0.6, fill = c(my_grey,my_red), border = c(NA,NA))

dev.off()

length(which(AHU1_2018$CS_0PA1TSA00_TL > 16))/4008
length(which(AHU1_2019$CS_0PA1TSA00_TL > 16))/4008