library(dplyr)
library(readr)
library(ggplot2)
library(doMC)
library(dplyr)
library(purrr)
library(caret)
library(corrplot)
library(knitr)

#Open training data from working directory as csv
train <- read.csv("~/Desktop/Kaggle/train.csv", stringsAsFactors = FALSE)
# train$SalePrice<-log(train$SalePrice)

#preliminary data inspection for train
str(train)
dim(train)
head(train)
class(train)

#Open test data from working directory as csv
test <- read.csv("~/Desktop/Kaggle/test.csv", stringsAsFactors = FALSE)

#preliminary data inspection for test
dim(test)
str(test)
class(test)
head(test)

#_____COMBINING THE TWO DATA SETS, TEST AND TRAIN______

#Identify disimilarities in column names
setdiff(colnames(test), colnames(train))
#results, "X1stFlrSF"  "X2ndFlrSF"  "X3SsnPorch"

#Correct the disimilarities
names(test)[names(test) == 'X1stFlrSF'] <- '1stFlrSF'
names(test)[names(test) == 'X2ndFlrSF'] <- '2ndFlrSF'
names(test)[names(test) == 'X3SsnPorch'] <- '3SsnPorch'

#Create a proxy column for SalePrice in test, assign NA
test$SalePrice <- NA

#Create an additional identifier column to distinguish between test, and training data
#1 = true (it is training data), 0 = false (it is test data)
train$isTrain <- 1
test$isTrain <- 0

#Finally join the two data sets
combined <-rbind(train, test)

dim(combined)

#___________IMPUTATIONS AND MISSINGNESS____________

##Assess total number of NA's
NA_TOTALS <- colSums(apply(X = combined, MARGIN = 2, is.na))
#Quick view of output
NA_TOTALS

#Select column and subsequent count of NA's in each column
NA_TOTALS[which(NA_TOTALS > 0)]

#Total number of variables that contain NA's
length(NA_TOTALS[which(NA_TOTALS > 0)])

#Total number of NA's
sum(NA_TOTALS)

#Based on totals, appears that Garage variables have a high concentration of NA's

#####CLEANING THE GARAGE######

colSums(apply(X = combined[, c("GarageCond","GarageQual", "GarageType","GarageFinish",
                               "GarageArea","GarageCars","GarageYrBlt")], MARGIN = 2, is.na))

#VISUAL REPRESENTATION OF GARAGE CONDITION VS SALES PRICE), and IMPUTATION of "NONE" for 0 value

boxplot(SalePrice ~ ifelse(is.na(GarageCond), "None", GarageCond), data = train, xlab = 
          "Garage Condition", ylab = "log(SalePrice)", main = "GarageCond")

#In order to consolidate the number of categories, use T-TESTs to determine if the mean SalesPrice
#of each certain category pairs are the same. From the boxplot, you can see Fair ("Fa") and Poor("Po") seem
#to align, just the same as Good ("Gd") and Excellent("Ex"). Will need t-test to comfirm:

t.test(with(train, SalePrice[with(train, which(GarageCond == "Ex"))]), with(train, SalePrice[with(train,
                                                                            which(GarageCond == "Gd"))]))

t.test(with(train, SalePrice[with(train, which(GarageCond == "Po"))]), with(train, SalePrice[with(train, 
                                                                            which(GarageCond == "Fa"))]))


## P-value for first pair (Good and Excellent, "Go" and "Ex") ~ .018, rejects Null hypothesis, confirms AH,
## so we won't merge those categories.

## P-value for second pair(Poor and Fair, "Po" and "FA") ~ .82, can confirm Null hypothesis and merge categores.

# We can replace Poor ("Po") with Fair ("Fa")

combined$GarageCond <- with(combined, ifelse(GarageCond == "Po", "Fa", GarageCond))

#Next we replace the rest of the NA's with the category "None", and apply dummy coding... and choose "None" as a
#baseline... means that linear regression coefficients of other values of this category will fill in for average
#SalesPrice of a house with no garage vs. a house with a garage with respectiv quality levels.

combined$GarageCond<-as.factor(with(combined, ifelse(is.na(GarageCond), "None", GarageCond)))
contrasts(combined$GarageCond)<-contr.treatment(levels(combined$GarageCond), base = 4)
with(combined, attr(GarageCond, "contrasts"))


#We can apply the same methods to Garage Quality (GarageQual)

t.test(with(train, SalePrice[with(train, which(GarageQual == "Ex"))]), with(train, SalePrice[with(train,
                                                                            which(GarageQual == "Gd"))]))
t.test(with(train, SalePrice[with(train, which(GarageQual == "Po"))]), with(train, SalePrice[with(train, 
                                                                            which(GarageQual == "Fa"))]))

#In this case, the p-values for each test respective, .93 (Excellent and Good) and .43 (Poor and Fair) "confirm
#the"fails to reject"

#Null hypothesis so we can merge those categories.

combined$GarageQual <- with(combined, ifelse(GarageQual == "Ex", "Gd", GarageQual)) 
combined$GarageQual <- with(combined, ifelse(GarageQual == "Po", "Fa", GarageQual))

#As was previouslt the case with Garage Condition we impute the remaining NA's with "None"

combined$GarageQual<-factor(with(combined, ifelse(is.na(GarageQual), "None", GarageQual)))

#...and apply dummy coding 

contrasts(combined$GarageQual) <- contr.treatment(unique(combined$GarageQual), base = 3)

#### SWEEPING THE REST OF THE GARAGE #####

# Replace remaining categorical values to "None" and all quantitative variables to "0" (ie in GarageType) in 
#order not to compensate for actual missing values

combined$GarageType<-factor(with(combined, ifelse(is.na(GarageType),"None", GarageType)))
combined$GarageFinish<-factor(with(combined, ifelse(is.na(GarageFinish), "None", GarageFinish)))

# apply dummy coding with baseline "None"

contrasts(combined$GarageType)<-contr.treatment(levels(combined$GarageType), base = 7)
contrasts(combined$GarageFinish)<-contr.treatment(levels(combined$GarageFinish), base = 2)

# replace numerical values

combined$GarageArea <- with(combined, ifelse(GarageType == "None", 0, GarageArea))
combined$GarageCars <- with(combined, ifelse(GarageType == "None", 0, GarageCars))
combined$GarageYrBlt<- with(combined, ifelse(GarageType == "None", 0, GarageYrBlt))

# account for the treatment/dummy variable, and create hypothetical categorical variables that indicates
# that a house has a garage -> HouseWithGarage

combined<-cbind(combined, with(combined, ifelse(GarageType == "None", 0,1)))
names(combined)[length(combined)]<-"HouseWithGarage"
combined$HouseWithGarage <- with(combined, factor(HouseWithGarage))

# create a summary of remaining NA within all Garage variables
sum(!complete.cases(combined[, c("GarageYrBlt","GarageCars","GarageArea","GarageQual",
                                 "GarageCond", "GarageType", "GarageFinish")]))

###### I THOUGHT I TOLD YOU TO CLEAN THE BASEMENT #######

#Summarize basement NA's and distribution amongst basement categories

colSums(apply(X = combined[, c("BsmtQual","BsmtCond", "BsmtUnfSF","BsmtFinSF1","BsmtFinSF2",
                               "BsmtHalfBath","BsmtFullBath","TotalBsmtSF", "BsmtFinType1","BsmtFinSF1",
                               "BsmtFinType2","BsmtFinSF2", "BsmtExposure", "TotalBsmtSF")], MARGIN = 2, is.na))

#A bit of a mess -> BsmtQual, BsmtCond, BsmtFinType1, BsmtFinType2, BsmtExposure each have ~ low 80's
# but BsmtQual and BsmtCond NA's suggest they are missing values

#Same as with Garage, replace with "None" and utilize it as baseline for dummy coding
combined$BsmtQual <- as.factor(with(combined, ifelse(is.na(BsmtQual), "None", BsmtQual)))
contrasts(combined$BsmtQual) <- contr.treatment(unique(combined$BsmtQual), base = 4)

combined$BsmtCond <- as.factor(with(combined, ifelse(is.na(BsmtCond), "None", BsmtCond)))
contrasts(combined$BsmtCond) <- contr.treatment(unique(combined$BsmtCond), base = 3)

#Run a check to determine if in fact those that have been imputed as "None" reflect consistently 
#across across Basement variables

with(combined, table(BsmtCond, BsmtQual))

#nope, there are a couple instances where there is a value for BaseQual that corresponds to "None" on the same
#observation for BsmtCond... suggesting we need to revert these occurences back to NA and surrender to the 
#imputation procedure that we described earlier

combined$BsmtQual[with(combined, which(BsmtCond == "Fa" & BsmtQual == "None"))]<-NA
combined$BsmtQual[with(combined, which(BsmtCond == "TA" & BsmtQual == "None"))]<-NA
combined$BsmtCond[with(combined, which(BsmtCond == "None" & BsmtQual == "Gd"))]<-NA
combined$BsmtCond[with(combined, which(BsmtCond == "None" & BsmtQual == "TA"))]<-NA
with(combined, table(BsmtCond, BsmtQual))

#Can do the same dummy coding with BsmtFinType1 & BsmtFinType2

combined$BsmtFinType1 <- as.factor(with(combined, ifelse(is.na(BsmtFinType1), "None", BsmtFinType1)))
contrasts(combined$BsmtFinType1) <- contr.treatment(unique(combined$BsmtFinType1), base = 5)

combined$BsmtFinType2 <- factor(with(combined, ifelse(is.na(BsmtFinType2), "None", BsmtFinType2)))
contrasts(combined$BsmtFinType2) <- contr.treatment(unique(combined$BsmtFinType2), base = 5)

#...And impute a single inconsistency on a resulting observation
combined[333, "BsmtFinType2"] <- NA

###For BsmtExposure, can convert the NA's as "None" and constrast to BsmtQual to ensure consistency
combined$BsmtExposure <- as.factor(with(combined, ifelse(is.na(BsmtExposure), "None", BsmtExposure)))
contrasts(combined$BsmtExposure) <- contr.treatment(unique(combined$BsmtExposure), base = 5)
with(combined, table(BsmtExposure, BsmtQual))

## The rest of the "Basement" variables are numerical, so recode to 0. We can compare to BsmtQual "None" to
#screen for values to replace.  

combined$BsmtUnfSF <- with(combined, ifelse(BsmtQual == "None", 0, BsmtUnfSF))
combined$BsmtFinSF1 <- with(combined, ifelse(BsmtQual == "None", 0, BsmtFinSF1))
combined$BsmtFinSF2 <- with(combined, ifelse(BsmtQual == "None", 0, BsmtFinSF2))
combined$BsmtHalfBath <- with(combined, ifelse(BsmtQual == "None", 0, BsmtHalfBath))
combined$BsmtFullBath <- with(combined, ifelse(BsmtQual == "None", 0, BsmtFullBath))

## Affix dummy variable to notate that the house has a basement

combined<-cbind(combined, with(combined, ifelse(BsmtQual == "None", 0,1)))
names(combined)[length(combined)]<-"HouseWithBsmt"
combined$HouseWithBsmt <- with(combined, factor(HouseWithBsmt))

###### Dealing with missing values in ELECTRICAL variable ######

# Electrical has 5 levels (SBrkr, Mix, FuseA, FuseF, FuseP), reasonable to believe that NA's are a result of missing values because
# *all homes have electrical systems

# Box plot to visualize electrical categories vs SalePrice

boxplot(SalePrice ~ Electrical, data = train, main = "Boxplot of Electrical", xlab = "Electrical", 
        ylab = "log(SalePrice)")

#Electrical categorical "level Mix" only has one observation so its not deemed statisticall relevant to
#our study

#Can performn an ANOVA test on Electrical to see if it is plausible to combine observations in the category
#can apply dummy coding with FuseA as baseline.

combined$Electrical<-as.factor(combined$Electrical)
contrasts(combined$Electrical)<-contr.treatment(levels(combined$Electrical), base = 1)

one_way_anova<-lm(SalePrice ~ Electrical, data = train)
summary(one_way_anova)

## P-value < .05 in relationship between SBrkr and FuseA, however the remaining categories don't suggest
# statistically significant correlations so we can combine the remaining levels into a single category "N_SBrKr",
#..and treatment/dummy code N_SBrKr as baseline

combined$Electrical<-as.character(combined$Electrical)
combined$Electrical[which(combined$Electrical %in% c("FuseA", "FuseF","FuseP","Mix"))]<-"N_SBrkr"
combined$Electrical<-as.factor(combined$Electrical)
contrasts(combined$Electrical)<-contr.treatment(levels(combined$Electrical), base = 1)


#####__________KITCHEN_Quality_________#######

#The Kitchen Quality variable presents an interesting discretionary situation.  


#Box plot to visualize the scale of the ordering of the levels according to its original categorical scale

boxplot(SalePrice ~ KitchenQual, data = train, main = "KitchenQual")

#Boxplot shows ordering to be approximately in scale, so we can proceed to convert the categorical scale into
# an ordinal integer scale ("Fa", "TA", "Gd", "Ex") -> (O, 1, 2, 3)

# create a recoding function using a 0-3 scale

KitchenQualCode<-function(val)
{
  if (is.na(val)){              
    new.val<-NA
  } else if (val == "Ex"){
    new.val<-3
  } else if (val == "Gd"){
    new.val<-2
  } else if (val == "TA"){
    new.val<-1
  } else if (val == "Fa"){
    new.val<- 0
  }
  return(new.val)
}
# Apply new function to KitchenQual

combined$KitchenQual <- sapply(X = combined$KitchenQual, FUN = KitchenQualCode)

##### MSSubClass #####
# MSSubClass has only one observation (categirical), and its within the test set (so we can't predict).
#Replace with NA, and address in imputation process.

table(train$MSSubClass)
table(test$MSSubClass)

combined[with(combined, which(MSSubClass == "150")), "MSSubClass"] <- NA

##### The remaining variables------- "MSSubClass","MSZoning","Street", "Alley","LotShape","LandContour",
#"Utilities","LotConfig","LandSlope", "Neighborhood","Condition1","Condition2","BldgType","HouseStyle","RoofStyle",
#"RoofMatl","ExterCond","Exterior1st","Exterior2nd","MasVnrType", "Foundation","HeatingQC","Heating",
#"CentralAir","Electrical","Functional","PavedDrive", "Fence", "SaleType","SaleCondition", "BsmtFullBath",
#"BsmtHalfBath","KitchenQual"_____ are list, so NA's coded to "None"

# Alley
combined$Alley <- as.factor(with(combined, ifelse(is.na(Alley), "None", Alley)))
contrasts(combined$Alley)<-contr.treatment(levels(combined$Alley), base = 2)
# Fence
combined$Fence <- as.factor(with(combined, ifelse(is.na(Fence), "None", Fence)))
contrasts(combined$Fence)<-contr.treatment(levels(combined$Fence), base = 5)

# MasVnrArea
combined$MasVnrArea <- with(combined, ifelse(MasVnrType == "None", 0, MasVnrArea))

# PoolQC
combined$PoolQC <- factor(with(combined, ifelse(is.na(PoolQC), "None", PoolQC)))
contrasts(combined$PoolQC) <- contr.treatment(unique(combined$PoolQC), base = 4)

# MiscFeature
combined$MiscFeature <- as.factor(with(combined, ifelse(is.na(MiscFeature), "None", MiscFeature)))
contrasts(combined$MiscFeature) <- contr.treatment(unique(combined$MiscFeature), base = 2)

# FireplaceQu
combined$FireplaceQu <- factor(with(combined, ifelse(Fireplaces == 0, "None", FireplaceQu)))
contrasts(combined$FireplaceQu) <- contr.treatment(unique(combined$FireplaceQu), base = 4)

# ExterQual
combined$ExterQual <- factor(combined$ExterQual)
contrasts(combined$ExterQual) <- contr.treatment(unique(combined$ExterQual), base = 2)  

# converting all categorical variables to factors
cat.vars<-c("MSSubClass","MSZoning","Street", "Alley","LotShape","LandContour","Utilities","LotConfig","LandSlope", "Neighborhood","Condition1","Condition2","BldgType","HouseStyle","RoofStyle","RoofMatl","ExterCond","Exterior1st","Exterior2nd","MasVnrType", "Foundation","HeatingQC","Heating","CentralAir","Electrical","Functional","PavedDrive", "Fence", "SaleType","SaleCondition", "BsmtFullBath", "BsmtHalfBath","KitchenQual")
for (i in 1:length(cat.vars))
{
  j<-which(colnames(combined) == cat.vars[i])
  combined[, j]<-as.factor(combined[,j])
}

missing_ct<-colSums(apply(X = combined, MARGIN = 2, is.na))
missing_ct[which(missing_ct > 0)]

# index.missing:  indices of missing values in jth feature
# j: index of feature to be imputed
# data: data frame, features in rows, observations in columns

random.impute <- function(index.missing, j, data)
{
  obs<-data[!index.missing, j]                              # get observed data for feature j
  n.missing<-sum(index.missing)                             # number of missing values
  rand.mis<-sample(obs, size = n.missing, replace = TRUE)   # sample with replacement from non-missing values
  return(rand.mis)                                                   
}

rf.impute <- function(index.missing, j, data)
{
  # fit random forest on observed values of feature j using the remaining features
  model<-randomForest(data[!index.missing, j] ~. , data = data[!index.missing, -j]) 
  # predict missing values
  pred.mis <- predict(model, newdata = data[index.missing, -j])
  return(pred.mis)
}

###############
############### IMPUTATION PROCESS ###################

# index.missing:  indices of missing values in jth feature
# j: index of feature to be imputed
# data: data frame, features in rows, observations in columns

#We first randomly initialize the imputed values using a sample from the non-missing values and 
#then we iterate over all  j with missing values to fill in the missing data. To do this, we will 
#use random forests.

#We'll create two functions to do the imputation. random.impute() returns random imputations sampled
#from the observed data and rf.impute() imputes missing values using random forests.

random.impute <- function(index.missing, j, data)
{
  obs<-data[!index.missing, j]                              # get observed data for feature j
  n.missing<-sum(index.missing)                             # number of missing values
  rand.mis<-sample(obs, size = n.missing, replace = TRUE)   # sample with replacement from non-missing values
  return(rand.mis)                                                   
}

rf.impute <- function(index.missing, j, data)
{
  # fit random forest on observed values of feature j using the remaining features
  model<-randomForest(data[!index.missing, j] ~. , data = data[!index.missing, -j]) 
  # predict missing values
  pred.mis <- predict(model, newdata = data[index.missing, -j])
  return(pred.mis)
}

#Now we will impute the missing values using random forests. We do this by initializing the missing values using 
#random samples from the non-missing data and then we iterate over the variables with missing values, sequentially 
#fitting a random forest model then predicting the imputed values.

# get indices of features that have NAs
na.index <- apply(X = combined, FUN = is.na, MARGIN = 2)
has.na<-colSums(na.index)
has.na<-which(has.na > 0)

# initialize with random values
for (k in 1:length(has.na))
{
  j<-has.na[k]
  i.mis <- na.index[, j]
  combined[i.mis, j]<-random.impute(i.mis, j, combined)
}
In [28]:
library(randomForest)
# set predictor type pattern for random forests
type.pattern<-c("classification", # MSSubClass
                "classification",  # MSZoning
                "regression",      # LotFrontage
                "classification",  # Utilities
                "classification",  # Exterior1st
                "classification",  # Exterior2nd
                "classification",  # MasVnrType
                "regression",      # MasVnrArea
                "classification",  # BsmtQual
                "classification",  # BsmtCond
                "classification",  # BsmtExposure
                "regression",      # BsmtFinSF1
                "classification",  # BsmtFinType2
                "regression",      # BsmtFinSF2
                "regression",      # BsmtUnfSF
                "classification",  # Electrical
                "classification",  # BsmtFullBath    
                "classification",  # BsmtHalfBath
                "classification",  # KitchenQual
                "classification",  # Functional
                "regression",      # GarageYrBlt
                "classification",  # GarageCars
                "regression",      # GarageArea
                "classification")  # SaleType


iter<-1 # set number of iterations (usually > 1)

for (t in 1:iter)
{
  for (k in 1:length(has.na))
  {
    j<-has.na[k]
    i.mis <- na.index[, j]
    combined[i.mis, j]<- rf.impute(i.mis, j ,combined)
  }
}

# make sure there are no missing values
colSums(apply(X = combined, FUN = is.na, MARGIN = 2))

# split back into training and test sets
imp.train<-cbind(train$SalePrice, combined[1:1460, ])
imp.test<-combined[1461:2919, ]

imp.train1 <- imp.train[, -c(83:85)]
imp.train2 <- imp.train[, -c(1, 83:85)]

########### Feature Engineering ############
# 
# # Low Variance Predictors
# 
# nzvar <- function(x) {
#   degenerate_check <- list()
#   tbl <- sort(table(x), decreasing = TRUE)
#   degenerate_check[["unique_to_samp"]] <- length(tbl) / sum(tbl)
#   degenerate_check[["most_prev_to_2nd_prev"]] <- (tbl[[1]] / tbl[[2]])
#   degenerate_check
# }
# 
# degenerate_vectors <- function(x){
#   degenerate_variables <- vector("character", ncol(x))
#   i = 1
#   for (nme in names(x) ) {
#     degen <- nzvar(x[[nme]])
#     if (degen[[1]] <= 0.1 & degen[[2]] >= 20) {
#       degenerate_variables[i] <- nme
#       i <- i + 1
#     }
#   }
#   degenerate_variables[degenerate_variables != ""]
# }
# 
# degen_vec <- degenerate_vectors(combined)
# combined1 <- select(combined, one_of(setdiff(names(combined), degen_vec)))
# 
# print(degen_vec)
# 
# #### Multicollinearity #####
# 
# # get_collinear <- function(x) {
# #   # Expects data dataframe
# #   num_cols <- ncol(x)
# #   collinear_vec <- vector("character", num_cols)
# #   index <- 1
# #   for (i in seq(1:num_cols)) {
# #     corMat <- cor(x)
# #     diag(corMat) <- 0
# #     df_cols <- names(x)
# #     AB <- which(corMat == max(abs(corMat)), arr.ind = TRUE)
# #     if (corMat[AB][[1]] > 0.75) {
# #       names_AB <- rownames(AB)
# #       if (sum(abs(corMat[names_AB[1], ])) > sum(abs(corMat[names_AB[2], ]))) {
# #         collinear_vec[index] = names_AB[1]
# #         index <- index + 1
# #       } else {collinear_vec[index] = names_AB[2]
# #       index <- index + 1}
# #
# #       x <- select(x, one_of(setdiff(df_cols, collinear_vec[index - 1])))
# #     } else{break}
# #   }
# #   collinear_vec[collinear_vec != ""]
# # }
# # pl <- get_collinear(combined1)
# 
# ####### Model Selection #######
# 
# flds <- createFolds(y = combined1$SalePrice, k = 10)
# # Set up custom trainControl
# my_contrl <- trainControl(
#   verboseIter = TRUE,
#   savePredictions = TRUE,
#   index = flds
# )
# data_cols <- ncol(combined1)
# glmnet_grid <- expand.grid(alpha = 0:1, lambda = seq(0.0001, 1, length = 20))
# rrf_grid <- expand.grid(coefReg = seq(0.5, 0.9, 0.1), mtry = seq(round(data_cols/6), round(data_cols/2), 2 )  )
# enet_grid <- expand.grid(lambda = seq(0, 0.1, 0.02), fraction = seq(0.05, 1, 0.19))


######## Models #########

# model1 <- train(x = imp.train1, y = imp.train1$SalePrice, method = "glm", trControl = my_contrl,
#                 preProcess = c("center", "scale"))
# model2 <- train(x = combined1, y = combined1$SalePrice, tuneGrid = glmnet_grid, method = "glmnet",
#                 trControl = my_contrl)
# model3 <- train(x = combined1, y = combined1$SalePrice, method = "ranger", trControl = my_contrl)
# model4 <- train(x = imp.train2, y = imp.train2$SalePrice, tuneGrid = enet_grid, method = "enet",
#                 trControl = my_contrl)
# model5 <- train(x = combined1, y = combined1$SalePrice, method = "leapBackward", trControl = my_contrl,
#                 preProcess = c("center", "scale"))
# model6 <- train(x = combined1, y = combined1$SalePrice, tuneGrid = rrf_grid, method = "RRFglobal",
#                 trControl = my_contrl)
# model7 <- train(x = combined1, y = combined1$SalePrice, method = "svmLinear2", trControl = my_contrl,
#                 preProcess = c("center", "scale"))
# model8 <- train(x = combined1, y = combined1$SalePrice, method = "gbm", trControl = my_contrl)
# model9 <- train(x = combined1, y = combined1$SalePrice, method = "brnn", trControl = my_contrl)

control <- trainControl(method="cv", number=10)
metric <- "Accuracy"
imp.train2$SalePrice <- as.factor(imp.train2$SalePrice)

# a) linear algorithms
set.seed(7)
fit.lda <- train(imp.train2$SalePrice~., data=imp.train2, method="lda", metric=metric, trControl=control)
# b) nonlinear algorithms
# CART
set.seed(7)
fit.cart <- train(imp.train2$SalePrice~., data=imp.train2, method="rpart", metric=metric, trControl=control)
# kNN
set.seed(7)
fit.knn <- train(imp.train2$SalePrice~., data=imp.train2, method="knn", metric=metric, trControl=control)
# c) advanced algorithms
# SVM
set.seed(7)
fit.svm <- train(imp.train2$SalePrice~., data=imp.train2, method="svmRadial", metric=metric, trControl=control)
# Random Forest
set.seed(7)
fit.rf <- train(imp.train2$SalePrice~., data=imp.train2, method="rf", metric=metric, trControl=control)
