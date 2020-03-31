dev.off()
rm(list=ls())
cat("\014")

#----Agglomerative Heirarchical Clustering Using Euclidean Distance----

#Performs clustering using ACH d on artificial occupancy data

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

#Read in data
dat <- read.csv(file = "C:\\Users\\Brodie\\OneDrive - Carleton University\\Working Directory\\Research Project WD\\Phase 2\\Data\\JP\\gt_day_dat.csv")

colnames(dat) <- as.Date(c(1:ncol(dat))-1, origin = '2018-01-27')

colnames(dat) <- as.POSIXct(colnames(dat), format = "%Y-%m-%d", tz = "GMT")

dist.mx <- dist(t(dat), method = "euclidean")

#Create heirarchical clustering model
euc.hclust.mx <- hclust(dist.mx, members = NULL)
rm(dist.mx)

#Specify number of clusters based on CH-index calculated previously
nclust <- 5

ind <- cutree(euc.hclust.mx,nclust)

#Sort the cluster indicies from highest to lowest cluster
clust_ord <- vector()

for(i in 1:nclust){
  
  clust_ord[length(clust_ord)+1] <- max(rowQuantiles(as.matrix(subset(dat[,which(ind==i)])), probs = 0.95))
  
}

clust_ord <- rev(order(clust_ord))

temp_ind <- rep(0,206)

for(i in 1:nclust){
  
  temp_ind[which(ind==clust_ord[i])] <- i
  
}

ind <- temp_ind

rm(temp_ind)

#Record the number of observations of each cluster
obs <- list()

for (i in 1:nclust){
  
  obs[[length(obs)+1]] <- length(which(ind==i))
  
}

obs <- unlist(obs)

should_plot <- F

if(should_plot == T){
  
  #Create a colourful graph of days and cluster centres for visualization
  
  my_grey <- rgb(0,0,0, alpha = 40, maxColorValue = 255)
  
  my_red <- rgb(255,0,0, alpha = 40, maxColorValue = 255)
  
  my_green <- rgb(0,202,14, alpha = 40, maxColorValue = 255)
  
  my_blue <- rgb(22,93,239, alpha = 40, maxColorValue = 255)
  
  my_yellow <- rgb(255,233,0, alpha = 40, maxColorValue = 255)
  
  my_col <- c(my_red,my_yellow,my_blue,my_green)
  
  prof_letter <- c('h','m','l','L')
  
  legend.text <- vector()
  
  for(i in 1:nclust){
    
    temp <- paste0("'", prof_letter[i], "', n = ", obs[i])
    
    legend.text <- c(legend.text, temp)
    
  }
  
  png(filename = "euc_clust.png", width = 2.75, height = 2.75, units = 'in', res = 600)
  par(mai = c(0.6,0.6,0.05,0.05), mgp = c(0,0.6,0))
  
  plot(x=c(0:24), y=c(0:24), xlim = c(0,24), ylim= c(0,500), type = "n", axes = "F", cex.lab = 0.8, 
       main = "", xlab = "", ylab = "")
  
  for(i in 1:nclust){
    
    temp <- subset(dat[,which(ind==i)])
    
    for(j in 1:ncol(temp)){
      
      lines(spline(x=c(0:23), y=temp[,j]), col = my_col[i])
      
    }
    
  }
  
  my_col <- c('red','yellow','blue','green')
  
  for(i in 1:nclust){
    
    lines(spline(x=c(0:23), y=(rowQuantiles(as.matrix(subset(dat[,which(ind==i)])),probs=0.95))), col = noquote(my_col[i]), lwd = 3)
    
  }
  
  axis(side = 1, seq(from = 0, to = 23, by = 23/24), labels = rep("",25), tck = -0.02)
  axis(side = 1, seq(from = 0, to = 23, by = 6*23/24), labels = c("00:00","06:00","12:00","18:00","23:55"), cex.axis = 0.6)
  axis(side = 2, at = seq(from = 0, to = 500, by = 50), labels = rep("",length(seq(from = 0, to = 500, by = 50))), tck = -0.02)
  axis(side = 2, at = seq(from = 0, to = 500, by = 100), labels = seq(from = 0, to = 500, by = 100), cex.axis = 0.6, las = 2)
  
  title(ylab = "Estimated Occupancy", line = 2, cex.lab = 0.8)
  title(xlab = "Time of Day", line = 1.8, cex.lab = 0.8)
  
  legend("topleft", legend = legend.text, bty = "n", col = my_col, lwd = 3, cex = 0.6)
  
  text(x = 24, y = 485, pos = 2, labels = "AHC, d (b)", cex = 0.6)
  
  dev.off()
  
}

write.csv(ind, "euc_ind.csv", row.names = F)