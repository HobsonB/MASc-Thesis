dev.off()
rm(list=ls())
cat("\014")

#----CO2 Analysis----

#Analysis of CO2 before and after implementation

#Author(s): Brodie W. Hobson

#Set working directory
setwd("C:\\Users\\hobso\\OneDrive - Carleton University\\Working Directory\\Research Project WD\\Phase 3")

#Load or install required packages
if ("lubridate" %in% rownames(installed.packages()) == FALSE) {install.packages("lubridate")}
require(lubridate)
if ("matrixStats" %in% rownames(installed.packages()) == FALSE) {install.packages("matrixStats")}
require(matrixStats)

yr <- c(2018,2019)

for(j in yr){
  
  #Read in groundtruth data
  co2 <- read.csv(file = paste0("C:\\Users\\hobso\\OneDrive - Carleton University\\Working Directory\\Research Project WD\\Phase 3\\Data\\eSim\\co2_",j,".csv"))
  co2[,1] <- as.POSIXct(co2[,1], format = "%Y-%m-%d %H:%M", tz = "GMT")
  co2 <- co2[which(hour(co2[,1]) %in% c(8:20)),]
  
  co2s <- list()
  
  for(i in c(2:nrow(co2))){
    
    co2s[[length(co2s)+1]] <- as.numeric(co2[i,-1])
    
  }
  
  nam <- paste0("co2s_",j)
  assign(nam, co2s)
  rm(co2s,nam)
}
rm(i,j)

co2s_2018 <- unlist(co2s_2018)
co2s_2019 <- unlist(co2s_2019)

co2s <- c(co2s_2018,co2s_2019)
yrs <- c(rep(2018, length(co2s_2018)), rep(2019, length(co2s_2019)))
dat <- cbind.data.frame(co2s, yrs)
t.test(co2s~yrs, data = dat, paired = T)

mu_2018 <- round(mean(co2s_2018),digits=0)
sd_2018 <- round(1.96*sd(co2s_2018), digits = 0)
mu_2019 <- round(mean(co2s_2019),digits=0)
sd_2019 <- round(1.96*sd(co2s_2019), digits = 0)

should_plot <- F

if(should_plot == T){
  
  y_up <- max(max(hist(co2s_2018, breaks = seq(0,1500,50)$counts)),max(hist(co2s_2018, breaks = seq(0,1500,50)$counts)))
  y_low <- 0
  x_up <- max(co2s)
  x_low <- 400
  
  my_red <- rgb(255,0,0, alpha = 40, maxColorValue = 255)
  my_grey <- rgb(0,0,0, alpha = 80, maxColorValue = 255)
  
  legend_text <- c(paste0("Before (µ = ",mu_2018," ± ",sd_2018,")"),
                   paste0("After (µ = ",mu_2019," ± ",sd_2019,")"))
  
  png(filename = "co2_hist.png", width = 2.75, height = 2.75, units = 'in', res = 600)
  par(mai = c(0.6,0.6,0.05,0.05), mgp = c(0,0.6,0))
  
  plot(x=1,y=1,xlab = "", ylab = "", main = "", xlim = c(x_low,x_up), ylim = c(y_low, y_up), axes = F, cex.lab = 0.6, type = 'n')
  
  par(new = T)
  hist(co2s_2018, breaks = seq(0,1500,50), col = my_grey,xlab = "", ylab = "", main = "",xlim = c(x_low,x_up), ylim = c(y_low, y_up), axes = F, border = F)
  par(new = T)
  hist(co2s_2019, breaks = seq(0,1500,50), col = my_red,xlab = "", ylab = "", main = "",xlim = c(x_low,x_up), ylim = c(y_low, y_up), axes = F, border = F)
  
  axis(side = 1, seq(x_low,x_up,100), labels = rep("",length(seq(x_low,x_up,100))), tck = -0.02)
  axis(side = 1, seq(x_low,x_up,200), labels = seq(x_low,x_up,200), cex.axis = 0.6)
  axis(side = 2, seq(y_low,y_up,y_up/16), labels = rep("",length(seq(y_low,y_up,y_up/16))), tck = -0.02)
  axis(side = 2, seq(y_low,y_up,y_up/4), labels = seq(0,0.16,0.04), cex.axis = 0.6, las = 2)
  
  title(xlab = expression(CO[2]~(ppm)), line = 1.8, cex.lab = 0.6)
  title(ylab = "Frequency (%)", line = 2, cex.lab = 0.6)
  
  legend("topright", legend = legend_text, bty = 'n', cex = 0.6, fill = c(my_grey,my_red), border = c(NA,NA))
  
  dev.off()
  
  length(which(co2s_2018 > 1100))/length(co2s_2018)*100
  
  length(which(co2s_2019 > 1100))/length(co2s_2019)*100
  
}