dev.off()
rm(list=ls())
cat("\014")

#----Calinski-Harabasz Index----

#Finds the CH-index foreach clustering technique and a varying number of clusters

#Author(s): Brodie W. Hobson

#Set working directory
setwd("C:\\Users\\hobso\\OneDrive - Carleton University\\Working Directory\\Research Project WD\\Phase 2")

#Load or install required packages
if ("lubridate" %in% rownames(installed.packages()) == FALSE) {install.packages("lubridate")}
require(lubridate)
if ("matrixStats" %in% rownames(installed.packages()) == FALSE) {install.packages("matrixStats")}
require(matrixStats)
if ("clues" %in% rownames(installed.packages()) == FALSE) {install.packages("clues")}
require(clues)
if ("BBmisc" %in% rownames(installed.packages()) == FALSE) {install.packages("BBmisc")}
require(BBmisc)
if ("uniReg" %in% rownames(installed.packages()) == FALSE) {install.packages("uniReg")}
require(uniReg)

#Read in data
dat <- read.csv(file = "C:\\Users\\hobso\\OneDrive - Carleton University\\Working Directory\\Research Project WD\\Phase 2\\Data\\JP\\gt_day_dat.csv")

#Create date range
colnames(dat) <- as.Date(c(1:ncol(dat))-1, origin = '2018-01-27')

#Convert dates to POSIXct
colnames(dat) <- as.POSIXct(colnames(dat), format = "%Y-%m-%d", tz = "GMT")

kmeans_CH <- vector()

for (i in 2:10){
  
  #Perform k-means clustering and extract cluster indicies
  set.seed()
  ind <- as.numeric(kmeans(t(dat[c(7:21),]),i)$cluster)
  kmeans_CH[length(kmeans_CH)+1] <- get_CH(t(dat),ind,disMethod = "Euclidean")
  
}

#Perform AHC using Euclidean distance and extracts cluster indicies
euc_CH <- vector()
dist.mx <- dist(t(dat), method = "euclidean")
euc.hclust.mx <- hclust(dist.mx, members = NULL)

for (i in 2:10){
  
  ind <- cutree(euc.hclust.mx,i)
  euc_CH[length(euc_CH)+1] <- get_CH(t(dat),ind,disMethod = "Euclidean")
  
}

#Perform AHC using correlation dissimilarity and extracts cluster indicies
dist.mx <- as.dist(1-normalize(cor(dat[(7:21),]), method = "range", range = c(0,1)))
cor.hclust.mx <- hclust(dist.mx, members = NULL)
cor_CH <- vector()

for (i in 2:10){
  
  ind <- cutree(cor.hclust.mx,i)
  cor_CH[length(cor_CH)+1] <- get_CH(t(dat),ind,disMethod = "1-corr")
  
}

should_plot <- F

if(should_plot == T){
  
  #Create a plot of the Calinski-Harabasz Index
  png(filename = "CH_index.png", width = 2.75, height = 2.75, units = 'in', res = 600)
  par(mai = c(0.6,0.6,0.05,0.05), mgp = c(0,0.6,0))
  
  plot(kmeans_CH, type = "n", las = 1, bty = "n", axes = F, ylim = c(0,400), xlim = c(2,10),
       xlab = "", ylab = "", main = "", cex.lab = 0.8)
  
  lines(x=c(2:10), y = cor_CH, col = "red", lty = 3, lwd = 2)
  lines(x=c(2:10), y = euc_CH, col = "red", lty = 2, lwd = 2)
  lines(x=c(2:10), y = kmeans_CH, col = "red", lty = 1, lwd = 2)
  
  axis(side = 1, seq(from = 2, to = 10, by = 2), labels = seq(from = 2, to = 10, by = 2), cex.axis = 0.6)
  axis(side = 1, seq(from = 2, to = 10, by = 1), labels = rep("",length(seq(from = 2, to = 10, by = 1))), tck = -0.02)
  axis(side = 2, seq(from = 0, to = 400, by = 50), labels = rep("",length(seq(from = 0, to = 400, by = 50))), tck = -0.02)
  axis(side = 2, seq(from = 0, to = 400, by = 100), labels = seq(from = 0, to = 400, by = 100), las = 2, cex.axis = 0.6)
  
  title(xlab = "Number of Clusters", line = 1.7, cex.lab = 0.8)
  title(ylab = "Caliñski-Harabasz Index", line = 2, cex.lab = 0.8)
  
  legend("topright", legend = c(expression(paste("AHC, 1 - ", rho)), "AHC, d", "k-means"), bty = "n", col = rep("red",3), 
         lty = c(3,2,1), lwd = c(2,2,2), cex = 0.6)
  dev.off()
  
}