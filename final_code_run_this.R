# Libraries ----
library("alr4")
library("GGally")
library("ggplot2")
library("knitr")
library("lmtest")
setwd("School/-School (old)-/STAT 512/project/house prices/")

# Read in data and discuss data ----
# Data can be downloaded from:
# https://www.kaggle.com/c/house-prices-advanced-regression-techniques/data
# Place data in working directory
house <- read.csv("train.csv")
dim(house)
colnames(house)

# Analysis of NAs in data ----
colMeans(is.na(house))

# Create Indicator Variables ----
# X2ndFlr ----
house$HasX2ndFlr <- as.factor(with(house, ifelse(X2ndFlrSF == 0, 0, 1)))
# HasPool ----
house$HasPool <- as.factor(with(house, ifelse(PoolArea == 0, 0, 1)))


# HasBasement ----
house$HasBasement <- as.factor(with(house, ifelse(TotalBsmtSF == 0, 0, 1)))

# HasGarage ----
house$HasGarage <- as.factor(with(house, ifelse(GarageArea == 0, 0, 1)))

# HasOutdoorSeating ----
house$HasOutdoorSeating <- as.factor(
  with(house, ifelse(WoodDeckSF != 0 | 
                       OpenPorchSF != 0 | 
                       EnclosedPorch != 0 | 
                       X3SsnPorch != 0 | 
                       ScreenPorch != 0, 1, 0)))
# HasLotFrontage ----
house$HasLotFrontage <- as.factor(with(house, ifelse(LotFrontage == 0 | is.na(LotFrontage), 0, 1)))

# HasMasVnr ----
house$HasMasVnr <- as.factor(with(
  house, ifelse(MasVnrArea == 0 | MasVnrType == "None" | 
                  is.na(MasVnrArea) | is.na(MasVnrType), 0, 1)))
house$MasVnrType[is.na(house$MasVnrType)] <- "None"

# MoSold ----
house$MoSold <- as.factor(house$MoSold)









# Make master file ----
colnames(house)
house <- house[-c(1, 4, 6, 7, 10, 20, 27, 31, 32, 33, 34, 35, 36, 37, 38, 39, 46,
                  58, 59, 60, 61, 62, 63, 64, 65, 67, 68, 69, 70, 71, 72, 73, 74, 75,
                  76)]
# Save as csv ----
write.csv(house, "Housing Data - Master.csv")
# Transformations ----
# Loading in desired data set
house <- read.csv("Housing Data - Master.csv")

# Taking only the desired columns
house <- house[-c(1,30,55,56)]
# Casting categorical variables as factors
house$MSSubClass <- as.factor(house$MSSubClass)
house$OverallQual <- as.factor(house$OverallQual)
house$OverallCond <- as.factor(house$OverallCond)
house$MoSold <- as.factor(house$MoSold)
house$HasX2ndFlr <- as.factor(house$HasX2ndFlr)
house$HasPool <- as.factor(house$HasPool)
house$HasBasement <- as.factor(house$HasBasement)
house$HasGarage <- as.factor(house$HasGarage)
house$HasOutdoorSeating <- as.factor(house$HasOutdoorSeating)
house$HasLotFrontage <- as.factor(house$HasLotFrontage)
house$HasMasVnr <- as.factor(house$HasMasVnr)
house <- na.omit(house)

# Visualizing bivariate plots, univariate plots, and correlation
ggpairs(house, c(3, 28, 29, 45))

# Looking for transformations
summary(a1<-powerTransform(cbind(LotArea,X1stFlrSF,GrLivArea) ~ 1, house))

# Double checking with marginal tests
with(house,invTranPlot(LotArea,log(SalePrice),lambda=c(-1,0,1)))
with(house,invTranPlot(X1stFlrSF,log(SalePrice),lambda=c(-1,0,1)))
with(house,invTranPlot(GrLivArea,log(SalePrice),lambda=c(-1,0,1)))

# Double checking transformation choice for response
m1 <- lm(SalePrice~log(LotArea)+log(X1stFlrSF)+log(GrLivArea),data=house)
# First, we look at an inverse response plot
inverseResponsePlot(m1)
# We see that lambda = 0.13 best fits the data 

# Now we look at the Box-Cox plot
boxCox(m1)

# This summary table from the power test confirms our thoughts from
# the previous two graphs
summary(powerTransform(m1))

# All log transform for LotArea, X1stFlrSF, GrLivArea
pairs(~log(LotArea)+log(X1stFlrSF)+log(GrLivArea)+log(SalePrice),house)
testTransform(a1,c(0,0,0))

# Testing correlations of transformed predictors with transformed response
with(house, cor.test(log(LotArea),log(SalePrice)))
with(house, cor.test(log(X1stFlrSF),log(SalePrice)))
with(house, cor.test(log(GrLivArea),log(SalePrice)))

# Visualize transformed data
house$logLotArea <- log(house$LotArea)
house$logX1stFlrSF <- log(house$X1stFlrSF)
house$logGrLivArea <- log(house$GrLivArea)
house$logSalePrice <- log(house$SalePrice)
ggpairs(house, c(53, 54, 55, 56))

# Variable selection ----
# There are 52 variables total and now we wish to do variable selection
# 40 were factors and 12 numeric of some sort
# Now performing model selection
m0 <- lm(log(SalePrice) ~ 1, house) # the base model
f <- ~MSSubClass+MSZoning+log(LotArea)+LotShape+LandContour+LotConfig+LandSlope+Neighborhood+Condition1+Condition2+BldgType+HouseStyle+OverallQual+OverallCond+YearRemodAdd+RoofStyle+RoofMatl+Exterior1st+Exterior2nd+MasVnrType+ExterQual+ExterCond+Foundation+Heating+HeatingQC+CentralAir+Electrical+log(X1stFlrSF)+log(GrLivArea)+FullBath+HalfBath+BedroomAbvGr+KitchenAbvGr+KitchenQual+TotRmsAbvGrd+Functional+Fireplaces+PavedDrive+MoSold+YrSold+SaleType+SaleCondition+HasX2ndFlr+HasPool+HasBasement+HasGarage+HasOutdoorSeating+HasLotFrontage+HasMasVnr

# Forward selection with BIC
m.forward <- step(m0, scope = f, direction = "forward", k = log(dim(house)[1]))
# Gives this model
# log(SalePrice) ~ OverallQual + log(GrLivArea) + Neighborhood + 
#  MSSubClass + OverallCond + log(LotArea) + HasBasement + RoofMatl + 
#  YearRemodAdd + HasGarage + Fireplaces + MSZoning + KitchenAbvGr + 
#  Functional + HasX2ndFlr + SaleCondition + KitchenQual + Condition2 + 
#  CentralAir + BedroomAbvGr

# Model diagnostics ----
m1 <- lm(log(SalePrice) ~ OverallQual + log(GrLivArea) + Neighborhood + 
           MSSubClass + OverallCond + log(LotArea) + HasBasement + RoofMatl + 
           YearRemodAdd + HasGarage + Fireplaces + MSZoning + KitchenAbvGr + 
           Functional + HasX2ndFlr + SaleCondition + KitchenQual + Condition2 + 
           CentralAir + BedroomAbvGr, house)
Anova(m1)
summary(m1)

# Testing for Outliers
outlierTest(m1)
# There are 10 outliers at the 0.05 Bonferroni corrected significance level
plot(m1,which = c(4))
# Examine cooks distance
cdm1 <- cooks.distance(m1)
cdm1[(cdm1) >= .35 | is.na(cdm1)]

# Model diagnostics with influential points removed ----
# Let's start with removing 524 and 826
m2 <- lm(log(SalePrice) ~ OverallQual + log(GrLivArea) + Neighborhood + 
           MSSubClass + OverallCond + log(LotArea) + HasBasement + RoofMatl + 
           YearRemodAdd + HasGarage + Fireplaces + MSZoning + KitchenAbvGr + 
           Functional + HasX2ndFlr + SaleCondition + KitchenQual + Condition2 + 
           CentralAir + BedroomAbvGr, house, subset = -c(524, 826, 121, 272, 376, 524, 534, 584,
                                                         667, 826, 1004, 1231, 1276, 1299))
summary(m2)

# Still some outliers
outlierTest(m2)
# Nothing is particularily influential
plot(m2,which = c(1,2,3,4))

# Examine cooks distance again
cdm2 <- cooks.distance(m2)
cdm2[(cdm2) >= 0.1 | is.na(cdm2)]
# The result is nothing

# Looking at Anova for m2
Anova(m2)
# Wow, Condition2 and RoofMatl is no longer significant!

# Creating model to see if can remove the above two terms
m3 <- lm(log(SalePrice) ~ OverallQual + log(GrLivArea) + Neighborhood + 
           MSSubClass + OverallCond + log(LotArea) + HasBasement + 
           YearRemodAdd + HasGarage + Fireplaces + MSZoning + KitchenAbvGr + 
           Functional + HasX2ndFlr + SaleCondition + KitchenQual + Condition2+
           CentralAir + BedroomAbvGr, house, subset = -c(524, 826, 121, 272, 376, 524, 534, 584,
                                                         667, 826, 1004, 1231, 1276, 1299))
# Performing F test
anova(m3,m2)
# Good to remove RoofMatl
Anova(m3)
# Looks like Condition2 is still not significant in the model
m4 <- lm(log(SalePrice) ~ OverallQual + log(GrLivArea) + Neighborhood + 
           MSSubClass + OverallCond + log(LotArea) + HasBasement + 
           YearRemodAdd + HasGarage + Fireplaces + MSZoning + KitchenAbvGr + 
           Functional + HasX2ndFlr + SaleCondition + KitchenQual +
           CentralAir + BedroomAbvGr, house, subset = -c(524, 826, 121, 272, 376, 524, 534, 584,
                                                         667, 826, 1004, 1231, 1276, 1299))


# Performing F test
anova(m4,m3)
# We are good to remove Condition2 from model as well

# Examining this reduced model
Anova(m4)
# Looking at residual plot for possibly misspecified mean function
residualPlots(m4)
# There is curvature in the log(GrLivArea) residual plot, while everything else
# looks fine. We acknowledge this shortcoming in our model, perhaps this suggests
# that the model fit is not the best

# Now testing for outliers
outlierTest(m4)
# Still a few outliers

# Looking for influential points
plot(m4,c(4))
# No single point appears to have a Cook's distance that is particularily larger
# than all the other points, so we have no additional points to remove

# Testing for nonconstant variance
ncvTest(m4)
# Very strong evidence against constant variance.
# We decided to use a sandwich estimator to accomdate this misspecified variance
# due to the large number of variables in our model that could possibly be
# contributing to nonconstant variance

# Checking normality assumption
plot(m4,c(2))
# Residuals do not appear to be normally distributed, since there is some
# strong curvature for the left end of the normality plot. However, our sample
# size is very large, and in general regression is robust toward violations of
# normality, so we simply acknowledge this pitfall in this model.

# Here is the regression output
summary(m4)


# Thus, we display the results of our final model
Anova(m4,vcov. = hccm)
coeftest(m4, vcov. =hccm)
coefci(m4,vcov. = hccm)

# Making effects plot for interpretation
efffects <- allEffects(m4, vcov. = hccm)
plot(efffects, ask=FALSE, multiline=TRUE, rug=FALSE, grid=TRUE,
     ci.style="bars", key.arg=list(corner=c(.975, .025)))

?subset
has2ndflr <- subset(house, HasX2ndFlr == 1)
mean(has2ndflr$logSalePrice)
no2ndflr <- subset(house, HasX2ndFlr == 0)
mean(no2ndflr$logSalePrice)

