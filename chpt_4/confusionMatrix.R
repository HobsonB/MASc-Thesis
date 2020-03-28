dev.off()
rm(list=ls())
cat("\014")

#----eSim Confusion Matrices----

#Examine confusion matricies for forecasting algorithm

#Author(s): Brodie W. Hobson

#Set working directory
setwd("C:\\Users\\hobso\\OneDrive - Carleton University\\Working Directory\\Research Project WD\\Phase 3")

#Load or install required packages
if ("lubridate" %in% rownames(installed.packages()) == FALSE) {install.packages("lubridate")}
require(lubridate)
if ("caret" %in% rownames(installed.packages()) == FALSE) {install.packages("caret")}
require(caret)

daytypes <- read.csv(file = "C:\\Users\\hobso\\OneDrive - Carleton University\\Working Directory\\Research Project WD\\Phase 3\\Data\\eSim\\daytypes.csv")

confusionMatrix(table(daytypes$Measured, daytypes$Actual))