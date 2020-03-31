dev.off()
rm(list=ls())
cat("\014")

#----Linear Regression Analysis----

#Train a linear regression model between the normalized Wi-Fi device counts and ground truth

#Author(s): Brodie W. Hobson

#Set working directory
setwd("C:\\Users\\hobso\\OneDrive - Carleton University\\Working Directory\\Research Project WD\\Phase 2")

#Load or install required packages
if ("lubridate" %in% rownames(installed.packages()) == FALSE) {install.packages("lubridate")}
require(lubridate)
if ("caret" %in% rownames(installed.packages()) == FALSE) {install.packages("caret")}
require(caret)
if ("BBmisc" %in% rownames(installed.packages()) == FALSE) {install.packages("BBmisc")}
require(BBmisc)

#Read in groundtruth and Wi-Fi data
dat <- read.csv(file = "C:\\Users\\hobso\\OneDrive - Carleton University\\Working Directory\\Research Project WD\\Phase 2\\Data\\JP\\gt_wifi_phase1.csv")

#Convert date information to POSIXct
dat[,1] <- as.POSIXct(dat[,1], format = "%Y-%m-%d %H:%M", tz = "GMT")

#Limit model to hours between 6am and 8pm (avoid overfitting to low, unoccupied hours)
dat <- dat[which(hour(dat[,1]) %in% c(6:19)),]

temp_subset <- data.frame()

for (i in unique(yday(dat[,1]))[order(unique(yday(dat[,1])))]){
  
  subset <- dat[which(yday(dat[,1])==i),]
  
  subset$DC <- subset$DC-(min(subset$DC)-min(subset$Groundtruth))
  
  subset[which(subset$DC < 0),2] <- 0
  
  temp_subset <- rbind.data.frame(subset, temp_subset)
  
}

dat <- temp_subset

rm(temp_subset, subset)

set.seed(1)

#Specify 10-fold cross-validation
train_control <- trainControl(method="cv",number=10)

#Speicify that linear model has no y-intercept (i.e., intercept at origin, no devices = no occupants)
grid <- expand.grid(intercept=FALSE)

#Train linear model
model <- train(form = Groundtruth ~ DC, data = dat, trControl=train_control, method="lm", tuneGrid=grid)

#Extract model coefficient (i.e., slope or number of occupants per device)
m_coef <- as.numeric(coef(model$finalModel))

#Find the R-squared
r_squared <- round(as.numeric(model$results[3])*100, digits = 1)

#Find the normalized RMSE
nrmse <- round(as.numeric(model$results[2]/max(dat$Groundtruth))*100, digits = 1)

my_col <- rgb(0,0,0, alpha = 40, maxColorValue = 255)

should_plot <- F

if(should_plot == T){
  
  png(filename = "lm_gt_wifi_phase1.png", width = 2.75, height = 2.75, units = 'in', res = 600)
  par(mai = c(0.6,0.6,0.05,0.05), mgp = c(0,0.6,0))
  
  plot(c(0:100),c(0:100), type = "n", axes = "F", cex.lab = 0.8, 
       main = "", ylab = "", xlab = "")
  
  points(dat$Groundtruth, dat$DC, col = my_col, pch = 16, cex = 0.5)
  
  lines(y=c(0:100), x = m_coef*c(0:100), lwd = 2, col = "red")
  
  axis(side = 1, at = seq(from = 0, to = 100, by = 20), labels = seq(from = 0, to = 1, by = 0.2),cex.axis = 0.6)
  axis(side = 1, at = seq(from = 0, to = 100, by = 10), labels = rep("",length(seq(from = 0, to = 100, by = 10))), tck = -0.02)
  axis(side = 2, at = seq(from = 0, to = 100, by = 20), labels = seq(from = 0, to = 1, by = 0.2), las = 2, cex.axis = 0.6)
  axis(side = 2, at = seq(from = 0, to = 100, by = 10), labels = rep("",length(seq(from = 0, to = 100, by = 10))), tck = -0.02)
  
  title(xlab = "Normalized Occupancy", line = 1.7, cex.lab = 0.8)
  title(ylab = "Normalized Wi-Fi Count", line = 2, cex.lab = 0.8)
  
  text(x = 0, y = 95, pos = 4, labels = as.expression(bquote(R^2 == .(r_squared)*"%, "~ NRMSE == .(nrmse)*"%")), cex = 0.6)
  legend("bottomright", legend = "y = 1.2x", lwd = 2, col = "red", cex = 0.6, bty = 'n')
  
  dev.off()
  
}