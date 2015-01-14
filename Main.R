# Authors: Rob Maas & Wilmar van Ommeren
# Date: 13-1-2015

# Load libraries:
library(sp) 
library(raster)

# Load source scripts:
source("R/calculate.RMSE.R")
source("R/mask.trainingdata.R")
source("R/calculate.RMSE.class.R")

# Load data:
load("Data/GewataB1.rda")
load("Data/GewataB2.rda")
load("Data/GewataB3.rda")
load("Data/GewataB4.rda")
load("Data/GewataB5.rda")
load("Data/GewataB7.rda")
load("Data/vcfGewata.rda")
load("Data/trainingPoly.rda")

opar <- par(mfrow=c(1,1))

# Transform data
vcfGewata[vcfGewata > 100] <- NA
rasterbrick <- brick(GewataB1, GewataB2, GewataB3, GewataB4, GewataB5, GewataB7)
rasterbrick <- calc(rasterbrick, fun=function(x) x / 10000)
rasterbrick <- addLayer(rasterbrick, vcfGewata)
names(rasterbrick) <- c("Band1","Band2", "Band3", "Band4", "Band5", "Band7", "VCF")

# Create rasterbrick
rasterbrickData <- getValues(rasterbrick)
rasterbrickData <- as.data.frame(rasterbrickData, na.rm=T)

# Plotting correlation between VCF and different LandSat bands
plot(VCF ~ Band1, data = rasterbrickData, pch = ".", col = "orange", main="Correlation between VCF and Landsat bands", xlab="Reflectance of LandSat bands", ylab="VCF [%]", xlim=c(0,0.6), ylim=c(0,100))
points(VCF ~ Band5, data = rasterbrickData, pch = ".", col = "dark green")
points(VCF ~ Band7, data = rasterbrickData, pch = ".", col = "light blue")
legend("topright", legend=c("Band1", "Band5", "Band7"), fill=c("orange", "dark green", "light blue"), bg="white")

# Creating linear regression model
model157 <- lm(VCF ~ Band1+Band5+Band7, rasterbrickData, na.action=na.omit)
modelAll <- lm(VCF ~ Band1+Band2+Band3+Band4+Band5+Band7, rasterbrickData, na.action=na.omit)

# summary(model157)
# summary(modelAll) 

# Predicting VCF based on linear regression model
VCFpredict <- predict(rasterbrick, model = model157, na.rm=T)
names(VCFpredict) <- "VCF"
VCFpredict[VCFpredict < 0] <- NA
VCFpredict[VCFpredict > 100] <- NA

# Comparing predicted and original VCF
par(mfrow=c(1,2))
plot(rasterbrick$VCF, main="Original VCF")
plot(VCFpredict,main="Predicted VCF")
par(opar)

# Calculating RMSE
RMSEpredict <- calculate.RMSE(rasterbrick$VCF, VCFpredict)
RMSEpredict

## Compare RMSE's of different classes

# Create trainingareas of the original and predicted values, and adding their classes.
trainingOri <- mask.trainingdata(rasterbrick$VCF, trainingPoly)
trainingPred <- mask.trainingdata(VCFpredict, trainingPoly)

# Calculating RMSE per class
RMSE_crop <- calculate.RMSE.class(trainingOri, trainingPred,1)
RMSE_for <- calculate.RMSE.class(trainingOri, trainingPred,2)
RMSE_wet <- calculate.RMSE.class(trainingOri, trainingPred,3)

paste("RMSE of cropland, forest and wetland are",format(round(RMSE_crop, 2), nsmall = 2),",",
      format(round(RMSE_for, 2), nsmall = 2),"and",format(round(RMSE_wet, 2), nsmall = 2),"respectively.")
