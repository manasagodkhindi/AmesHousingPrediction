library(dplyr)
library(ggplot2)
library(mice)
library(VIM)
library(randomForest)
library(tree)
library(lattice)



#.....................Author: Lalith Sugavanam.........................
#LOAD TRAINING DATA
houseTrain = read.csv("C:/NYCDSProject/MachineLearning/Data/train.csv", 
                        header = TRUE,
                        stringsAsFactors = FALSE)


dim(houseTrain)
str(houseTrain)
colnames(houseTrain)
class(houseTrain)
summary(houseTrain)
#drop salesprice to combine data
#houseTrain = within(houseTrain,rm())
cHouse=within(cHouse,rm(All))

#LOAD TEST DATA
houseTest = read.csv("C:/NYCDSProject/MachineLearning/Data/test.csv", 
                       header = TRUE,
                       stringsAsFactors  = FALSE)
dim(houseTest)
str(houseTest)
colnames(houseTest)
class(houseTest)
summary(houseTest)
#ADDING DUMMY SALEPRICE COLUMN TO TEST DATASET
houseTest$SalePrice = 0

#COMBINE DATASETS TO GET A CONSISTENCY
cg = rbind(houseTrain,houseTest)

md.pattern(cg)
cHouse = rbind(houseTrain,houseTest)
dim(cHouse)
summary(cHouse)

#............MISSINGNESS & IMPUTATIONS...................
#CHECKING THE DISTRIBUTION OF MISSING OBSERVATIONs
nullCols = which(colSums(is.na(houseTrain))>0)
sort(colSums(sapply(houseTrain[nullCols],is.na)),decreasing=TRUE)
#THE VARIABLES WITH THE MISSING VALUES IN THE ORDER OF SIZE

#PoolQC  MiscFeature        Alley        Fence    SalePrice  FireplaceQu  LotFrontage 
#2909         2814         2721         2348         1459         1420          486 
#GarageYrBlt GarageFinish   GarageQual   GarageCond   GarageType     BsmtCond BsmtExposure 
#159          159          159          159          157           82           82 
#BsmtQual BsmtFinType2 BsmtFinType1   MasVnrType   MasVnrArea     MSZoning    Utilities 
#81           80           79           24           23            4            2 
#BsmtFullBath BsmtHalfBath   Functional  Exterior1st  Exterior2nd   BsmtFinSF1   BsmtFinSF2 
#2            2            2            1            1            1            1 
#BsmtUnfSF  TotalBsmtSF   Electrical  KitchenQual   GarageCars   GarageArea     SaleType 
#1            1            1            1            1            1            1

#Missing Data Pattern
md.pattern(cHouse)

#VISUALISATION OF MISSING VALUES
mice_plot = aggr(cHouse,col=c('blue','yellow'),
                 numbers=TRUE,softVars=TRUE,
                 labels=names(cHouse),cex.axis=.7, gap=1, ylab=c("Histogram of missing data","Pattern"))

#class(impData)
#REFERENCE - https://www.r-bloggers.com/missing-value-treatment/

#THERE ARE 13 HOUSES WITH SWIMMING POOLS AND ONLY 10 HAVE POOL QUALITY DATA AVAILABLE
library(rpart)
table(cHouse$PoolArea>0,cHouse$PoolQC, useNA="ifany")
#FOR POOL AREA REASSIGN 'None' FOR NAs - AS IT IT MEANS NO POOL
cHouse[cHouse$PoolArea == 0 & is.na(cHouse$PoolQC),]$PoolQC = 'NP'
sum(sapply(cHouse$PoolQC,is.na))
#FOR THE 3 MISSING POOLQC VALUES WHICH CAN BE PREDICTED USING RPART
colsForPrediction = c("PoolQC","YearBuilt","PoolArea","SaleCondition","ExterCond","ExterQual","YrSold","SaleType","SaleCondition")
poolQltyRpart = rpart(as.factor(PoolQC)~.,
                      data=cHouse[!is.na(cHouse$PoolQC),colsForPrediction],
                      method="class",na.action = na.omit)
cHouse[is.na(cHouse$PoolQC),"PoolQC"] = predict(poolQltyRpart,
                                       cHouse[is.na(cHouse$PoolQC),colsForPrediction],
                                       type="class")
sum(sapply(cHouse$PoolQC,is.na))

#.........THE GARAGE VARIABLES COLELCTIVELY HAVE A LARGE NUMBER OF NAs
table(is.na(cHouse$GarageYrBlt) & is.na(cHouse$GarageFinish) & 
        is.na(cHouse$GarageQual) & 
        is.na(cHouse$GarageCond) & 
        is.na(cHouse$GarageType) &
        is.na(cHouse$GarageCars) &
        is.na(cHouse$GarageArea)
      )
table(is.na(cHouse$GarageYrBlt) & is.na(cHouse$GarageFinish) & 
        is.na(cHouse$GarageQual) & 
        is.na(cHouse$GarageCond) & 
        is.na(cHouse$GarageType)
)
#NA IN GARAGE TYPE MEAN NO GARAGE - THERE ARE 157 SUCH GARAGE TYPES, SET THEM TO NG 
cHouse[is.na(cHouse$GarageType),]$GarageType = 'NG'
table(cHouse$GarageArea == 0 & cHouse$GarageCars == 0 & cHouse$GarageType != "NG")
#THERE ARE 157 HOUSES WITH NOGARAGEAREA AND TYPE MENTIONED
howMany = length(cHouse[cHouse$GarageType == 'NG',]$GarageType)
colsGarage = c("GarageFinish","GarageQual","GarageCond") 
cHouse[cHouse$GarageArea==0 & cHouse$GarageCars==0 & cHouse$GarageType == 'NG', colsGarage] =
  apply(cHouse[cHouse$GarageArea == 0 & cHouse$GarageCars==0 & cHouse$GarageType == 'NG',colsGarage],2,function(x) x = rep("None",howMany))
cHouse[cHouse$GarageType == 'NG',]$GarageYrBlt = 0 #157 ROWS

#THAT LEAVES US WITH THE 1 GarageArea, GarageCars, 2 GarageYrBlt, GarageFinish, GarageQual, GarageCond MISSING GARAGE VALUES
table(is.na(cHouse$GarageYrBlt) & is.na(cHouse$GarageFinish) & 
        is.na(cHouse$GarageQual) & 
        is.na(cHouse$GarageCond))
colsGaragePredict =  c("GarageType","GarageYrBlt","GarageFinish","GarageQual","GarageCond","YearBuilt","GarageCars","GarageArea") 
#GARAGEAREA
garageAreaRpart = rpart(GarageArea ~ ., data=cHouse[!is.na(cHouse$GarageArea),colsGaragePredict],method="anova",na.action = na.omit)
cHouse$GarageArea[is.na(cHouse$GarageArea)] = round(predict(garageAreaRpart,cHouse[is.na(cHouse$GarageArea),colsGaragePredict]))
sum(sapply(cHouse$GarageArea,is.na))
#GARAGECARS
garageCarsRpart = rpart(GarageCars ~ ., data=cHouse[!is.na(cHouse$GarageCars),colsGaragePredict],method="anova",na.action = na.omit)
cHouse$GarageCars[is.na(cHouse$GarageCars)] = round(predict(garageCarsRpart,cHouse[is.na(cHouse$GarageCars),colsGaragePredict]))
#GARAGEYEAR
#garageYrBltRpart=rpart(as.factor(GarageYrBlt) ~ ., data=cHouse[!is.na(cHouse$GarageYrBlt),colsGaragePredict],
#                         method="anova",
#                         na.action = na.omit)
#cHouse$GarageYrBlt[is.na(cHouse$GarageYrBlt)]= as.numeric(as.character(predict(garageYrBltRpart,cHouse[is.na(cHouse$GarageYrBlt),colsGaragePredict])))
cHouse$GarageYrBlt[is.na(cHouse$GarageYrBlt)]= 0

#GARAGEFINISH, GARAGEQUAL, GARAGECOND
table(cHouse$GarageFinish) #unf isthe most common type
cHouse[is.na(cHouse$GarageFinish),"GarageFinish"] = "Unf"

table(cHouse$GarageQual) #TA isthe most common type
cHouse[is.na(cHouse$GarageQual),"GarageQual"] = "TA"
table(cHouse$GarageCond) #TA isthe most common type
cHouse[is.na(cHouse$GarageCond),"GarageCond"] = "TA"

cHouse[is.na(cHouse$GarageFinish) & is.na(cHouse$GarageQual) & is.na(cHouse$GarageCond),
       c(colsGarage,c("GarageCars","GarageArea"))]

#GarageType      GarageYrBlt GarageFinish GarageQual GarageCond GarageCars GarageArea
#2127     Detchd 40.5829875518672         <NA>       <NA>       <NA>          1        360
#2577     Detchd 40.5829875518672         <NA>       <NA>       <NA>          1        300
#SEPARATE CHECKS SHOW THAT ALL THE DETACHED GARAGES IN THAT YEAR ARE UNFINISHED AND 'TA' CONDITION

nullCols = which(colSums(is.na(cHouse))>0)
sort(colSums(sapply(cHouse[nullCols],is.na)),decreasing=TRUE)

#MiscFeature - NA means None
cHouse[is.na(cHouse$MiscFeature),"MiscFeature"] = "None"

#Alley - NA no Alley access
cHouse[is.na(cHouse$Alley),"Alley"] = "None"

#Fence - FENCES ARE MISSING MAYBE BECAUSE THERE ARE NO FENCE, SO SET TO NONE
cHouse[is.na(cHouse$Fence),"Fence"] = "None"
#FirePlaceQu-  ARE MISSING MAYBE BECAUSE THERE ARE NO FENCE, SO SET TO NONE
cHouse[is.na(cHouse$FireplaceQu),"FireplaceQu"] = "None"

#BsmtCond, BsmtExposure, BsmtQual,  BsmtFinType2, BsmtFinType1 - NAs COLECTIVELY MEAN BASEMENT
table(is.na(cHouse$BsmtCond) & is.na(cHouse$BsmtExposure) & 
        is.na(cHouse$BsmtQual) & 
        is.na(cHouse$BsmtFinType2) & 
        is.na(cHouse$BsmtFinType1)
)
cHouse[is.na(cHouse$BsmtCond) & is.na(cHouse$BsmtExposure) & 
  is.na(cHouse$BsmtQual) & 
  is.na(cHouse$BsmtFinType2) & 
  is.na(cHouse$BsmtFinType1),
  c("BsmtCond","BsmtExposure","BsmtQual","BsmtFinType2","BsmtFinType1")] = "None"

#LotFrontage - USE rpart AND predict, LotFrontage WILL BE DEPENDENT ON OTHER VARIABLES OR PREDICTORS
#Rpart - Recursive Partitioning and Regression Trees
lotFrontageCols = c("LotFrontage","MSSubClass","MSZoning","LotArea","HouseStyle","LotArea","Street","Alley",
                    "BldgType","LandContour","LotConfig","LandSlope")
lotFrontRpart = rpart(LotFrontage ~ ., data=cHouse[!is.na(cHouse$LotFrontage),lotFrontageCols],method="anova",na.action = na.omit)
cHouse$LotFrontage[is.na(cHouse$LotFrontage)] = ceiling(predict(lotFrontRpart,cHouse[is.na(cHouse$LotFrontage),lotFrontageCols]))

#THERE ARE STILL FEW NAS - MICE SHOULD HANDLE THEM
#imputedLastNAs = mice(cHouse[,startsWith(names(cHouse),"Bsmt")],m = 5,method ="cart")

cHouse[is.na(cHouse$BsmtFullBath),"BsmtFullBath"] = 0
cHouse[is.na(cHouse$BsmtHalfBath),"BsmtHalfBath"] = 0
cHouse[is.na(cHouse$BsmtFinSF1),"BsmtFinSF1"] = 0
cHouse[is.na(cHouse$BsmtFinSF2),"BsmtFinSF2"] = 0
cHouse[is.na(cHouse$BsmtUnfSF),"BsmtUnfSF"] = 0
cHouse[is.na(cHouse$TotalBsmtSF),"TotalBsmtSF"] = 0
#cHouse = complete(imputedLastNAs)
cHouse[is.na(cHouse$BsmtFinType2),"BsmtFinType2"] = "None"
cHouse[is.na(cHouse$BsmtCond),"BsmtCond"] = "None"
cHouse[is.na(cHouse$BsmtExposure),"BsmtExposure"] = "None"
cHouse[is.na(cHouse$BsmtQual),"BsmtQual"] ="None"
cHouse[is.na(cHouse$MasVnrType),"MasVnrType"] = "None"
cHouse[is.na(cHouse$MasVnrArea),"MasVnrArea"] = 0
cHouse[is.na(cHouse$SaleType),"SaleType"] = "Oth"
cHouse[is.na(cHouse$Functional),"Functional"] = "Typ"
cHouse[is.na(cHouse$Functional),"Exterior1st"] = "Other"
cHouse[is.na(cHouse$Functional),"Exterior2nd"] = "Other"
cHouse[is.na(cHouse$KitchenQual),"KitchenQual"] = "TA"
cHouse[is.na(cHouse$Utilities),"Utilities"] = "AllPub"
cHouse[is.na(cHouse$Electrical),"Electrical"] = "AllPub"
cHouse[is.na(cHouse$Exterior1st),"Exterior1st"] = "Other"
cHouse[is.na(cHouse$Exterior2nd),"Exterior2nd"] = "Other"

#MSZONING
colsToPredict = c("Neighborhood", "Condition1", "Condition2", "MSZoning")
theRpart =rpart(as.factor(MSZoning) ~ .,
                   data = cHouse[!is.na(cHouse$MSZoning),colsToPredict],
                   method = "class",
                   na.action=na.omit)

cHouse$MSZoning[is.na(cHouse$MSZoning)] = as.character(predict(theRpart, cHouse[is.na(cHouse$MSZoning),colsToPredict], 
                                                                type = "class"))
aggr(cHouse, prop = F, numbers = T)

#SAVING DATA IN EXCEL FILE
write.csv(cHouse,"C:/NYCDSProject/MachineLearning/Data/Imputed.csv",sep = ",")

#............EXPLORATORY DATA ANALYSIS.............
#CONVERT CHARACTER VARIABLES TO FACTORS AFTER IMPUTING CATEGORICAL DATA
cHouse[,categoryVariables] = lapply(cHouse[,categoryVariables],as.factor)

#NOTES : SIGNIFICANCE TESTS
#FOR CONTINUOUS VS. NOMINAL VARIABLES - ANOVA (EFFECT SIZE - STRENGTH OF ASSOCIATION) ICC (PSYCH PACKAGE)
#FOR NOMINAL VS. NOMINAL - CHI-SQUARED (CALCULATE CRAMER'S V USING ASSOCSTATS IN VCD PACKAGE)

#...............PREPPING DATA.............
numericVars = names(wineDF)[sapply(wineDF, is.numeric)]
normalize <- function(x) {
  return((x - mean(x))/sd(x))
}
wineDF[,numericVars] = lapply(wineDF[,numericVars],normalize)

#................CORRELATION..............
## correlation between continuous variables in training dataset - pearson
numericFeatures = names(wineDF)[sapply(wineDF, is.numeric)]
library(corrplot)
correlations = cor(cHouse[,],use="everything")
corrplot(correlations, type="lower",order="hclust", t1.col="black,t1.srt=45") 

#SPLIT DATA
train = cHouse[cHouse$SalePrice!=0,]
test = cHouse[cHouse$SalePrice==0,]
library(caret)
trainIdx <- createDataPartition(train$SalePrice, 
                                p = .8,
                                list = FALSE,
                                times = 1)
subTrain <- train[trainIdx,]
subTest <- train[-trainIdx,]
print(head(sTrain))

#...................EDA.....................
#NEIGHBORHOOD VS SALEPRICE
ggplot(data = train, aes(group = Neighborhood, x= Neighborhood, y = SalePrice)) +
  geom_violin() +
  geom_boxplot(aes(fill = Neighborhood))

#EXTERNAL QUALITY AND SALE PRICE
ggplot(data = train, aes(x= ExterQual, y = SalePrice)) +
  geom_boxplot(aes(fill = ExterQual))

#BATHROOM AND SALE PRICE
ggplot(data = train, aes(x= MSZoning, y = SalePrice)) +
  geom_boxplot(aes(fill = MSZoning))

#OverallQual AND SALE PRICE
ggplot(data = train, aes(group= OverallQual, x= OverallQual, y = SalePrice)) +
  geom_boxplot(aes(fill = OverallQual))

#OverallQual AND SALE PRICE
ggplot(data = train, aes(group= HouseStyle, x= HouseStyle, y = SalePrice)) +
  geom_boxplot(aes(fill = HouseStyle))


#c("20","30","40","45","50","60","70","75","80","85","90","120","150","160","180","190")
#............FEATURES ENGINEERING...................
nearZeroVar()
#           "MSSubClass"    "MSZoning"      "LotFrontage"   "LotArea"       "Street"        "Alley"         "LotShape"      "LandContour"   "Utilities"     "LotConfig"     "LandSlope"    
# "Neighborhood"  "Condition1"    "Condition2"    "BldgType"      "HouseStyle"    "OverallQual"  
# "OverallCond"   "YearBuilt"     "YearRemodAdd"  "RoofStyle"     "RoofMatl"      "Exterior1st"  
# "Exterior2nd"   "MasVnrType"    "MasVnrArea"    "ExterQual"     "ExterCond"     "Foundation"   
# "BsmtQual"      "BsmtCond"      "BsmtExposure"  "BsmtFinType1"  "BsmtFinSF1"    "BsmtFinType2" 
# "BsmtFinSF2"    "BsmtUnfSF"     "TotalBsmtSF"   "Heating"       "HeatingQC"     "CentralAir"   
# "Electrical"    "X1stFlrSF"     "X2ndFlrSF"     "LowQualFinSF"  "GrLivArea"     "BsmtFullBath" 
# "BsmtHalfBath"  "FullBath"      "HalfBath"      "BedroomAbvGr"  "KitchenAbvGr"  "KitchenQual"   "TotRmsAbvGrd"  "Functional"    "Fireplaces"    "FireplaceQu"   "GarageType"    "GarageYrBlt"  
# "GarageFinish"  "GarageCars"    "GarageArea"    "GarageQual"    "GarageCond"    "PavedDrive"   
# "WoodDeckSF"    "OpenPorchSF"   "EnclosedPorch" "X3SsnPorch"    "ScreenPorch"   "PoolArea"     
# "PoolQC"        "Fence"         "MiscFeature"   "MiscVal"       "MoSold"        "YrSold"       
# "SaleType"      "SaleCondition" "SalePrice"

library(caret)
#.....................MLR A Linear Model (USES ABSOLUTE T-STATIC)........
#Evaluate variable importance
library(caret)
set.seed(0)
#ALL VARIABLES
lmHouse = train(SalePrice ~ . , data = subTrain, method="lm")

#FEW VARIABLES
lmHouse = train(SalePrice ~ MSSubClass+OverallCond+KitchenQual+SaleCondition+PoolQC+GarageQual+BldgType+YearBuilt+BsmtFinSF1,data = subTrain, method="lm")

featureImp = varImp(lmHouse,scale = TRUE)
plot(featureImp)
#PERFORMANCE MEASURES
summary(lmHouse)
#Residual standard error: 22260 on 827 degrees of freedom
#Multiple R-squared:  0.9452,	Adjusted R-squared:  0.9226
#F-statistic: 41.81 on 341 and 827 DF,  p-value: < 2.2e-16
#Predict on test dataset with predict
prediction = predict(lmHouse, subTest)
#Error in model.frame.default(Terms, newdata, na.action = na.action, xlev = object$xlevels): 
#factor GarageYrBlt has new levels 1908, 1933, 1942
#Measure performance
RMSE(pred = prediction, obs = subTest$SalePrice)
#.....................RANDOM FORESTS...........
set.seed(0)
rfHouse = randomForest(SalePrice ~ .-Id, data = subTrain, importance = TRUE)
oob.err = numeric(80)
for (mtry in 1:80) {
  fit = randomForest(SalePrice ~ ., data = subTrain, mtry = mtry)
  oob.err[mtry] = fit$mse[500]
}
plot(1:80, oob.err, pch = 16, type = "b",
     xlab = "Variables Considered at Each Split",
     ylab = "OOB Error Rate",
     main = "Random Forest OOB Error Rates\nby # of Variables")
importance(rfHouse, type=1)
varImpPlot(rfHouse)
#....................REGULARIZATION..................
library(ISLR)
library(glmnet)
#WITH THIS 1) THE ESTIMATED VARIANCE IS REDUCED, 2)VARIABLE SELECTION IS PERFORMED.
x = model.matrix(SalePrice ~ ., subTrain)[, -1] #Dropping the intercept column.
dim(x)
#1169  263
y = subTrain$SalePrice

#Fitting the lasso regression. Alpha = 1 for lasso regression.
grid = 10^seq(5, -2, length = 100)
lasso.models = glmnet(x, y, alpha = 1, lambda = grid)
dim(coef(lasso.models)) #80 different coefficients, estimated 100 times --
#once each per lambda value.
coef(lasso.models)

#What do the estimates look like for a smaller value of lambda?
lasso.models$lambda[80] #Lambda = 0.2595.
coef(lasso.models)[, 80] #Most estimates not close to 0.
sum(abs(coef(lasso.models)[-1, 80])) #4196349
#What do the estimates look like for a larger value of lambda?
lasso.models$lambda[15] #Lambda = 10,235.31.
coef(lasso.models)[, 15] #Estimates NOT all 0.
sum(abs(coef(lasso.models)[-1, 15])) #31250.36

#Visualizing the lasso regression shrinkage.
plot(lasso.models, xvar = "lambda", label = TRUE, main = "Lasso Regression")
#Can use the predict() function to obtain lasso regression coefficients for a
#new value of lambda, not necessarily one that was within our grid:
predict(lasso.models, s = 50, type = "coefficients")
#......................Elastic Net....................
fitCtrl <- trainControl(method = "cv",
                        number = 5,
                        verboseIter = TRUE,
                        summaryFunction=defaultSummary)
#This function sets up a grid of tuning parameters for a number of classification
#and regression routines, fits each model and calculates a resampling based
#performance measure.
enetGrid <- expand.grid(alpha = seq(0, 1, .1),
                        lambda = seq(0, .6, .01))
set.seed(1234)  # for reproducibility
enetFit <- train(SalePrice ~ .,
                 data = subTrain,
                 method="glmnet",
                 metric="RMSE",
                 trControl=fitCtrl,
                 tuneGrid=enetGrid)
print(enetFit$bestTune)
#   alpha lambda
#61     0    0.6
#...........................................................................................

