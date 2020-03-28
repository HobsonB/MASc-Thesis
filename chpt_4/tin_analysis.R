dev.off()
rm(list=ls())
cat("\014")

#----eSim Indoor Temperature Analysis----

#Analysis of indoor temperature for eSim paper

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

yr <- c(2018,2019)

for(j in yr){
  
  #Read in groundtruth data
  tin <- read.csv(file = paste0("C:\\Users\\hobso\\OneDrive - Carleton University\\Working Directory\\Research Project WD\\Phase 3\\Data\\eSim\\tmp_",j,".csv"))
  tin[,1] <- as.POSIXct(tin[,1], format = "%Y-%m-%d %H:%M", tz = "GMT")
  
  tin <- tin[which(hour(tin[,1]) %in% c(8:20)),]
  
  tins <- list()
  
  for(i in c(2:nrow(tin))){
    
    tins[[length(tins)+1]] <- as.numeric(tin[i,-1])
    
  }
  
  nam <- paste0("tins_",j)
  
  assign(nam, tins)
  
  rm(tins,nam)
}
rm(i,j)

tins_2018 <- unlist(tins_2018)
tins_2018 <- tins_2018[-(which(tins_2018 > 30))]
tins_2018 <- tins_2018[-(which(tins_2018 < 15))]
tins_2019 <- unlist(tins_2019)
tins_2019 <- tins_2019[-(which(tins_2019 > 30))]
tins_2019 <- tins_2019[-(which(tins_2019 < 15))]

mu_2018 <- round(mean(tins_2018),digits=1)
sd_2018 <- round(1.645*sd(tins_2018), digits=1)
mu_2019 <- round(mean(tins_2019),digits=1)
sd_2019 <- round(1.645*sd(tins_2019), digits=1)

y_up <- 2171*138*0.30
y_low <- 0
x_up <- 27
x_low <- 17

my_red <- rgb(255,0,0, alpha = 40, maxColorValue = 255)
my_grey <- rgb(0,0,0, alpha = 80, maxColorValue = 255)

legend_text <- c(paste0("Before (µ = ",mu_2018," ± ",sd_2018,")"),
                 paste0("After (µ = ",mu_2019," ± ",sd_2019,")"))

png(filename = "tin_hist.png", width = 2.75, height = 2.75, units = 'in', res = 600)
par(mai = c(0.6,0.6,0.05,0.05), mgp = c(0,0.6,0))

plot(x=1,y=1,xlab = "", ylab = "", main = "", xlim = c(x_low,x_up), ylim = c(y_low, y_up), axes = F, cex.lab = 0.6, type = 'n')

par(new = T)
hist(tins_2018, breaks = seq(15,30,0.5), col = my_grey,xlab = "", ylab = "", main = "",xlim = c(x_low,x_up), ylim = c(y_low, y_up), axes = F, border = F)
par(new = T)
hist(tins_2019, breaks = seq(15,30,0.5), col = my_red,xlab = "", ylab = "", main = "",xlim = c(x_low,x_up), ylim = c(y_low, y_up), axes = F, border = F)

axis(side = 1, seq(x_low,x_up,1), labels = rep("",length(seq(x_low,x_up,1))), tck = -0.02)
axis(side = 1, seq(x_low,x_up,2), labels = seq(x_low,x_up,2), cex.axis = 0.6)
axis(side = 2, seq(y_low,y_up,y_up/30), labels = rep("",length(seq(y_low,y_up,y_up/30))), tck = -0.02)
axis(side = 2, seq(y_low,y_up,y_up/6), labels = seq(0,0.3,0.05), cex.axis = 0.6, las = 2)

title(xlab = "Indoor Air Temperature (°C)", line = 1.8, cex.lab = 0.6)
title(ylab = "Frequency (%)", line = 2, cex.lab = 0.6)

#par(new = T)
#rho <- density(tins_2018, adjust = 1)
#plot(x=0,y=0, type = "n", xlim = c(x_low,x_up), ylim = c(0, 1), axes = F, main = "", xlab = "", ylab = "") 
#rho$y <- normalize(rho$y, method = 'range', range = c(0,1*(max(hist(tins_2018, breaks = seq(15,30,0.5), plot = F)$counts)/y_up)))
#lines(rho$x[which(rho$x < x_up)], rho$y[which(rho$x < x_up)], lwd = 2)

#par(new = T)
#rho <- density(tins_2019, adjust = 1)
#plot(x=0,y=0, type = "n", xlim = c(x_low,x_up), ylim = c(0, 1), axes = F, main = "", xlab = "", ylab = "") 
#rho$y <- normalize(rho$y, method = 'range', range = c(0,1*(max(hist(tins_2019, breaks = seq(15,30,0.5), plot = F)$counts)/y_up)))
#lines(rho$x[which(rho$x < x_up)], rho$y[which(rho$x < x_up)], lwd = 2, col = "red")

legend("topright", legend = legend_text, bty = 'n', cex = 0.6, fill = c(my_grey,my_red), border = c(NA,NA))

dev.off()

sum(hist(tins_2018, breaks = seq(15,30,0.5))$counts[13:17])/(2171*138)*100

sum(hist(tins_2019, breaks = seq(15,30,0.5))$counts[13:17])/(2171*138)*100     