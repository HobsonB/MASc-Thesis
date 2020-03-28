dev.off()
rm(list=ls())
cat("\014")

#----Changepoint Analysis----

#Changepoint models for energy use before and after implementation

#Author(s): Brodie W. Hobson

#Set working directory
setwd("C:\\Users\\hobso\\OneDrive - Carleton University\\Working Directory\\Research Project WD\\Phase 3")

#Load or install required packages
if ("lubridate" %in% rownames(installed.packages()) == FALSE) {install.packages("lubridate")}
require(lubridate)
if ("matrixStats" %in% rownames(installed.packages()) == FALSE) {install.packages("matrixStats")}
require(matrixStats)

years <- c(2018,2019)

file_nam <- c("STM","CW","OAT")

for(j in years){
  
  for(i in file_nam){
    
    dat <- read.csv(file = paste0("C:\\Users\\hobso\\OneDrive - Carleton University\\Working Directory\\Research Project WD\\Phase 3\\Data\\eSim\\",i,"_",j,".csv"))
    dat[,1] <- as.POSIXct(dat[,1], format = "%Y-%m-%d %H:%M", tz = "GMT")
    nam <- i
    assign(nam,dat)
    
  }
  rm(dat,nam,i)
  
  #----Heating changepoint----
  
  r2s <- list()
  
  for(i in seq(0,18,0.5)){
    
    ind <- which(OAT$OAT[c(1:2208,2977:4008)] < i)
    x <- OAT$OAT[ind]
    y <- STM$kWh[ind]
    mdl <- lm(I(y-0) ~ I(x-i)+0)
    r2s[[length(r2s)+1]] <- summary(mdl)$r.squared
    
  }
  rm(i,x,y,mdl,ind)
  
  #Find changepoint
  heat_chgpt <- seq(0,18,0.5)[which.max(unlist(r2s))]
  
  #Train fit
  ind <- which(OAT$OAT[c(1:2208,2977:4008)] < heat_chgpt)
  x <- OAT$OAT[ind]
  y <- STM$kWh[ind]
  heat_mdl <- lm(I(y-0) ~ I(x-heat_chgpt)+0)
  
  #Extract fit parameters for plot
  heat_r2.fit <- round(summary(heat_mdl)$r.squared, digits = 2)
  heat_m.fit <- round(summary(heat_mdl)$coefficients[1], digits = 0)
  heat_y.fit <- predict(heat_mdl, newdata = data.frame(x = seq(-30,30,0.5)))
  heat_b.fit <- round(heat_y.fit[which(seq(-30,30,0.5)==0)], digits = 0)
  
  #----Cooling changepoint----
  
  r2s <- list()
  
  for(i in seq(0,18,0.5)){
    
    ind <- which(OAT$OAT > i)
    x <- OAT$OAT[ind]
    y <- CW$kWh[ind]
    mdl <- lm(I(y-0) ~ I(x-i)+0)
    r2s[[length(r2s)+1]] <- summary(mdl)$r.squared
    
  }
  rm(i,x,y,mdl,ind)
  
  #Find changepoint
  cool_chgpt <- seq(0,18,0.5)[which.max(unlist(r2s))]
  
  #Train fit
  ind <- which(OAT$OAT > cool_chgpt)
  x <- OAT$OAT[ind]
  y <- CW$kWh[ind]
  cool_mdl <- lm(I(y-0) ~ I(x-cool_chgpt)+0)
  
  #Extract fit parameters for plot
  cool_r2.fit <- round(summary(cool_mdl)$r.squared, digits = 2)
  cool_m.fit <- round(summary(cool_mdl)$coefficients[1], digits = 0)
  cool_y.fit <- predict(cool_mdl, newdata = data.frame(x = seq(-30,30,0.5)))
  cool_b.fit <- round(cool_y.fit[which(seq(-30,30,0.5)==0)], digits = 0)
  
  #----Changepoint Plots----
  
  should_plot <- F
  
  if(should_plot == T){
    
    my_grey <- rgb(0,0,0, alpha = 40, maxColorValue = 255)
    
    png(filename = paste0("changepoint_",j,".png"), width = 2.75, height = 2.75, units = 'in', res = 600)
    par(mai = c(0.6,0.6,0.05,0.05), mgp = c(0,0.6,0))
    
    plot(x = 0, y = 0, ylim = c(0,500), xlim = c(-25,35), axes = F, ylab = "", xlab = "", main = "", type = 'n')
    
    points(x = OAT$OAT[c(1:2208,2977:4008)], y = STM$kWh[c(1:2208,2977:4008)], col = my_grey, pch = 16, cex = 0.5)
    
    points(x = OAT$OAT, y = CW$kWh, col = my_grey, pch = 18, cex = 0.5)
    
    par(new=T)
    
    plot(x = 0, y= 0 ,ylim = c(0,500), xlim = c(-25,35), type  = 'n', axes = F, ylab = "", xlab = "", main = "")
    
    lines(x = seq(-25,heat_chgpt,0.5), y = predict(heat_mdl, newdata = data.frame(x = seq(-25,heat_chgpt,0.5))), col = 'red', lwd = 2) 
    lines(x = seq(heat_chgpt,35,0.5), y = rep(0, length(seq(heat_chgpt,35,0.5))), col = 'red', lwd = 2)
    
    lines(x = seq(cool_chgpt,35,0.5), y = predict(cool_mdl, newdata = data.frame(x = seq(cool_chgpt,35,0.5))), col = 'blue', lwd = 2) 
    lines(x = seq(-25,cool_chgpt,0.5), y = rep(0, length(seq(-25,cool_chgpt,0.5))), col = 'blue', lwd = 2)
    
    axis(side = 1, seq(-25,35,5), labels = rep("",length(seq(-25,35,5))), tck = -0.02)
    axis(side = 1, seq(-25,35,15), labels = seq(-25,35,15), cex.axis = 0.6)
    axis(side = 2, seq(0,500,50), labels = rep("",length(seq(0,500,50))), tck = -0.02)
    axis(side = 2, seq(0,500,100), labels = seq(0,500,100), cex.axis = 0.6, las = 2)
    
    title(xlab = "Outdoor Air Temperature (°C)", line = 1.6, cex.lab = 0.6)
    title(ylab = expression("Conditioning Load (kWh"[eq]*")"), line = 2, cex.lab = 0.6)
    
    legend("topright", c(paste0("y = max(", heat_m.fit,"x + ", heat_b.fit,", 0), R² = ", heat_r2.fit),
                         paste0("y = max(", cool_m.fit,"x - ", abs(cool_b.fit),", 0), R² = ", cool_r2.fit)), 
           bty = 'n', lty = c(1,1), col = c("red","blue"), cex = 0.6)
    
    dev.off()
    
  }
}

#----Cooling Energy Use----

#2018 model on 2019 temperature data
y <- (18*(OAT_2019$OAT))-302
y[which(y < 0)] <- 0
cw_2019_before <- sum(y[c(1:2208,3000:4008)])
#2019 model on 2019 temperature data
y <- (13*OAT_2019$OAT)-186
y[which(y < 0)] <- 0
cw_2019_after <- sum(CW_2019$kWh[c(1:2208,3000:4008)])

cw_sv_pc <- (cw_2019_before-cw_2019_after)/cw_2019_before

#----Heating Energy Use----

#2018 model on 2019 temperature data
y <- (-9*(OAT_2019$OAT))+153
y[which(y < 0)] <- 0
stm_2019_before <- sum(y[c(1:2208,3000:4008)])
#2019 model on 2019 temperature data
y <- (-7*OAT_2019$OAT)+103
y[which(y < 0)] <- 0
stm_2019_after <- sum(STM_2019$kWh[c(1:2208,3000:4008)])

stm_sv_pc <- (stm_2019_before-stm_2019_after)/stm_2019_before

#----Fan Energy Use----

#2018 fan data
y <- sum(submeter_2018[,c(22:28)])
y <- y*(2227/2611)
fan_2019_before <- y
#2019 fan data
y <- sum(submeter_2019[,c(15:21)])
fan_2019_after <- y

fan_sv_pc <- (fan_2019_before-fan_2019_after)/fan_2019_before