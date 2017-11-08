## Load the training data set

library(dplyr)
library(VIM)
library(mice)
library(missForest)
library(ggplot2)
library(corrplot)
library (vcd)
library(e1071)
library(randomForest)
library(car)
library(glmnet)
library(xts)
library(DAAG)

Ames_train= read.csv('C:\\Data\\Bootcamp\\RML\\Machine Learning Project\\Data\\train\\train.csv',
                     header = TRUE, stringsAsFactor = FALSE)

View(Ames_train)
str(Ames_train)

sum(rowSums(is.na(ames_sub))) 

missingnum_col=colSums(is.na(ames_sub))

aggr(ames_sub)
missingnum_col
md.pattern(ames_sub)

colnames(Ames_train)[colSums(is.na(Ames_train))>0]

Ames_train$Alley[is.na(Ames_train$Alley)]= "NAl"
Ames_train$PoolQC[is.na(Ames_train$PoolQC)]= "NP"
Ames_train$Fence[is.na(Ames_train$Fence)]= "NF"
Ames_train$BsmtQual[is.na(Ames_train$BsmtQual)]= "NB"
Ames_train$BsmtCond[is.na(Ames_train$BsmtCond)]= "NB"
Ames_train$BsmtExposure[is.na(Ames_train$BsmtExposure)]= "NB"
Ames_train$BsmtFinType1[is.na(Ames_train$BsmtFinType1)]= "NB"
Ames_train$BsmtFinType2[is.na(Ames_train$BsmtFinType2)]= "NB"
Ames_train$FireplaceQu[is.na(Ames_train$FireplaceQu)]= "NFp"
Ames_train$GarageType[is.na(Ames_train$GarageType)]= "NG"
Ames_train$GarageFinish[is.na(Ames_train$GarageFinish)]= "NG"
Ames_train$GarageQual[is.na(Ames_train$GarageQual)]= "NG"
Ames_train$GarageCond[is.na(Ames_train$GarageCond)]= "NG"
Ames_train$MiscFeature[is.na(Ames_train$MiscFeature)]= "None"
Ames_test$MasVnrArea[is.na(Ames_train$MasVnrArea)]= 0
Ames_train$SalePrice = log(Ames_train$SalePrice)

#function to convert the datatypes of columns
converttypes = function(df,columns= names(df), type){
  df[cols] = lapply(df[cols],type)
  df
}

cols= c("Alley","PoolQC","Fence", "BsmtQual","BsmtCond","BsmtExposure","FireplaceQu","GarageType",
        "GarageFinish","GarageQual","GarageCond", "SaleCondition", "KitchenQual","BldgType","LandSlope",
        "Street")
Ames_train = converttypes(Ames_train, cols, 'factor')

str(Ames_train)

#impute missing data using mice
set.seed(0)
imputed_data = mice(Ames_train, m=5, maxit=10, method = 'cart')


#get the data imputed
Ames_train <- complete(imputed_data,2)
#impute data using missForest
#imputed_rf = missForest(ames_sub)

#plot the graps to see the relationships
plot(Ames_train$MasVnrArea, Ames_train$SalePrice)

#subset all the numeric varaibles for plotting
Ames_train_numeric = Ames_train[,sapply(Ames_train, is.numeric)]
summary(Ames_train_numeric)
sapply(Ames_train_numeric,sd)

#correlation of numeric variables
Ames_cor = cor(Ames_train_numeric[,-1], use="complete.obs")
row_indic <- apply(correlations, 1, function(x) sum(x > 0.3 | x < -0.3) > 1)

corrplot(Ames_cor, method ='color',type='lower')
#Sales price has strong corelation with 
#overall quality,TotalBsmtArea, x1stFlrSE, GrliviArea,Fullbath,TotalRoom,CarGarage,GarageArea,
#second level corelation with year built, year remodeled,MasVnrArea,GarageYrbuilt,Fireplace
#SalesPrice negatively corelated with overallCondition, kitchenAbvGrd, enclosedPorch

#subset all the categorical (factor and character )
Ames_train_categorical = Ames_train[,sapply(Ames_train, class) %in% c('character','factor')]
names(Ames_train_categorical)
Ames_train_categorical = sapply(Ames_train_categorical,factor)


#plot relationship between categorical variables
mosaic(Ames_train_categorical, shade = TRUE)

#plot histogram of saleprice
ggplot(Ames_train, aes(x=log(SalePrice)))+ geom_histogram(bins = 25 )

#plot the saleprice by neighborhood
ggplot(Ames_train, aes(x=Neighborhood, y = SalePrice))+ geom_col() + coord_flip()
#neighrborhoods NoRidge has the highest price 

#plot to find relation between GrlivArea and SalePrice
ggplot(Ames_train, aes(x=GrLivArea, y = SalePrice))+ geom_point() + geom_smooth()

#plot density plots for all numeric variables
Ames_melt_train= melt(Ames_train_numeric[,-c(1)])
ggplot(Ames_melt_train, aes(x=value))+ geom_histogram(bins=50) +facet_wrap(~variable, scales="free_x")

#install.packages('psych')
library(psych)
#multi.hist(Ames_train_numeric[,-c(1)]) # to use psych library and plot histograms, big number .of variables 


# find skew for each numerical variables using e1071 library
skew_measure = sapply(Ames_train_numeric[-1],skewness)

#function to normalize data 
normalize = function(x){
  return(x-mean(x)/sd(x))
}
# scale all the variables with skewness above 0.5
skew_cols = skew_measure[skew_measure>0.75 | skew_measure < (-0.75)]
Ames_train_numeric[,names(skew_cols)] = sapply(Ames_train_numeric[,names(skew_cols)], normalize)
#Ames_train_numeric[,names(skew_cols)] = sapply(Ames_train_numeric[,names(skew_cols)], log)
#chi -sq test
mapply(function(x, y) chisq.test(x, y)$p.value, Ames_train_categorical[,-42] , MoreArgs=list(Ames_train_categorical[,42]))

#combine numerical and categorical variables
Ames_train1 = cbind( Ames_train_categorical, Ames_train_numeric)
Ames_train1 = Ames_train1[complete.cases(Ames_train1),]


# build a randomForest tree

#separate train and test sets
set.seed(0)
train = sample(1:nrow(Ames_train1), nrow(Ames_train1)*0.8)
test = Ames_train1[-train,]
SalePrice.test = test$SalePrice

# fit randomForest 
set.seed(0)
rf_Ames = randomForest(SalePrice ~ . ,data = Ames_train1[train,], importance = TRUE)
rf_Ames

#Mean of squared residuals: 0.01843413
#% Var explained: 88.67


importance(rf_Ames)
varImpPlot(rf_Ames)



#vary the number of variables at each step and save the oOB error
set.seed(0)
oob.err = numeric(80)
for (mtry in 1:80) {
  fit = randomForest(SalePrice ~ ., data = Ames_train1[train, ], mtry = mtry)
  oob.err[mtry] = fit$mse[500]
  cat("We're performing iteration", mtry, "\n")
}

plot(1:80, oob.err, pch = 16, type = "b",
     xlab = "Variables Considered at Each Split",
     ylab = "OOB Error Rate",
     main = "Random Forest OOB Error Rates\nby # of Variables")

#Alternative way to find best number of variables
#tuneRF(Ames_train1[train, -1],Ames_train1$SalePrice, 26, ntreeTry=50, stepFactor=2, improve=0.05, trace=TRUE, plot=TRUE, doBest=FALSE)

set.seed(0)
best_fit = randomForest(SalePrice ~ ., data = Ames_train1[train, ], mtry = 9)
best_fit
# Mean of squared residuals: 0.01959631
#% Var explained: 87.96

predict_test = predict(best_fit, test)

#calculate mean squared error
plot(predict_test,SalePrice.test)
abline(0,1)

mse = mean((SalePrice.test - predict_test)^2)
#test error - 0.02102575
Rmse = sqrt(mean((SalePrice.test - predict_test)^2))
#Rmse - [1] 0.1450026
mean(best_fit$rsq)

# model lasso regression for variable selection and prediction

 #use model matrix to dummify the categorical variables
Ames_dummy = model.matrix(SalePrice ~ . ,  data = Ames_train1)[,-1]
 
grid = 10^seq(5, -2, length = 100)
lasso_model = glmnet(Ames_dummy[train,], Ames_train1[train,]$SalePrice, alpha = 1 , 
                     lambda = grid)
coef(lasso_model)
dim(coef(lasso_model))
plot(lasso_model, xvar = "lambda", label = TRUE)

set.seed(0)
lasso_Ames = cv.glmnet(Ames_dummy[train,], Ames_train1[train,]$SalePrice, alpha = 1 , 
                       lambda = grid , nfolds= 10)
summary(lasso_Ames)

dim(coef(lasso_Ames)) # 260 coefficients estimated 1 time

# variable selection - get the variables whose  coefficients are non zero 
 tmp_coeffs = coef(lasso_Ames, s = "lambda.min") #this lists the variables and their coefficients
 #colnames(Ames_dummy)[which(coef(lasso_Ames, s = "lambda.min"))!=0]
lasso_variable_selected = data.frame(name = tmp_coeffs@Dimnames[[1]][tmp_coeffs@i + 1], coefficient = tmp_coeffs@x)
lasso_variable_selected 
# 53 variables have non zero coefficients

#get the lambda where MSE is minimum.
best_shrinkage = lasso_Ames$lambda.min
log(best_shrinkage) 
# 0.01 is the lambda whee MSE is low. plot the lambdas to confirm this.
# log lambda is -4.60517.
plot(lasso_Ames)

# predict on the test set
set.seed(0)
predict_test_lasso = predict(lasso_Ames,  s= best_shrinkage, newx= Ames_dummy[-train,] )
predict_test_lasso

#calculate the mse and rmse
mse_lasso = mean(( SalePrice.test - predict_test_lasso )^2)
#0.04049319
Rmse_lasso = sqrt(mean(( SalePrice.test - predict_test_lasso )^2))
#0.2012292

#fit model  on entire dataset set with best lambda
Ames_lasso_full  = glmnet(Ames_dummy, Ames_train1$SalePrice, alpha =1, lambda= best_shrinkage)
Ames_lasso_full


# model  using Multiple linear regression

ml_Ames = lm(SalePrice ~  Neighborhood + OverallQual + TotalBsmtSF + 
               GrLivArea + KitchenQual + GarageArea + BsmtFinSF1 +BsmtFinType1 + OverallCond
               + LotArea + X1stFlrSF
              , data = Ames_train1[train,], x = TRUE)

summary(ml_Ames)
View(head(ml_Ames$x)) #factor variables are dummified.

#check for assumptions
plot(ml_Ames)

# find the default contrasts used in the model.
# default contrasts used in the model is contr.treatment
ml_Ames$contrasts

#residual vs fitted plots
plot(ml_Ames$residuals, ml_Ames$fitted.values)
ml_Ames$effects


#covariance for model parameters
vcov(ml_Ames)
influence(ml_Ames)


#look for linearly dependent variables
ld.vars <- attributes(alias(ml_Ames)$Complete)$dimnames[[1]]
ld.vars


car::vif(ml_Ames)

#5 folds cross validation on linear model
ml_Ames_cv =cv.lm(lm=ml_Ames, m=5, seed = 0)


predict_ml_test = predict(ml_Ames, test)
mse_ml = mean((predict_ml_test - SalePrice.test)^2) #0.04071098
Rmse_ml = sqrt(mean((predict_ml_test - SalePrice.test)^2)) #0.2017696







### NOT USED #########################################################

# replaceNA =function(df, cols){
#   for(col in cols){
#     if (is.na(col))
#     {
#      df$col[is.na(df$col)]= "No"
#      df
#     }
#     }
#   }
#ames_sub = Ames_train %>% select(LotFrontage,MasVnrType,MasVnrArea ,Electrical,GarageYrBlt) 
###############################################





