###########Principal Component Analysis################
library(psych)
houseTrain = read.csv("C:/NYCDSProject/MachineLearning/Data/MG_Ames_train_imputed.csv", 
                      header = TRUE,
                      stringsAsFactors = FALSE)

numericFeatures = names(houseTrain)[sapply(houseTrain, is.numeric)]
train.Cov = cov(houseTrain[,numericFeatures[1:37]])

#####Choosing K#####
fa.parallel(houseTrain[,numericFeatures[1:37]], #The data in question.
            fa = "pc", #Display the eigenvalues for PCA.
            n.iter = 100) #Number of simulated analyses to perform.
abline(h = 1) #Adding a horizontal line at 1.

#1. Kaiser-Harris criterion suggests retaining PCs with eigenvalues > 1; PCs with
#   eigenvalues < 1 explain less varaince than contained in a single variable.
#2. Cattell Scree test visually inspects the elbow graph for diminishing return;
#   retain PCs before a drastic drop-off.
#3. Run simulations and extract eigenvalues from random data matrices of the same
#   dimension as your data; find where the parallel analysis overshadows real data.

#####Performing PCA#####
pc_Houses = principal(houseTrain[,numericFeatures[1:37]], #The data in question.
                      nfactors = 5, #The number of PCs to extract.
                      rotate = "none")

#GIVES RELATION BETWEEN NEW VARIABLE AND OLD VARIABLES, NEGATIVE MEANS A NEGATIVE CORRELATION BETWEEN 
#PC1 AND THE VARIABLE
pc_Houses

#-PC columns contain loadings; correlations of the observed variables with the PCs.
#-h2 column displays the component comunalities; amount of variance explained by
# the components.
#-u2 column is the uniqueness (1 - h2); amount of varaince NOT explained by the
# components.
#-SS loadings row shows the eigenvalues of the PCs; the standardized varaince.
#-Proportion/Cumulative Var row shows the variance explained by each PC.
#-Proportion Explained/Cumulative Proportion row considers only the selected PCs.
#
#####Visualizing & Interpreting PCA#####

plot(pc_Houses,
            labels = colnames(train.Cov)) #Add variable names to the plot.
#-PC1 correlates highly positively with length-related variables (height, arm
# span, forearm, and lower leg). This is a "length" dimension.
#-PC2 correlates highly positively with volume-related variables (weight, bitro
# diameter, chest girth, and chest width). This is a "volume" dimension.
plot(pc_Houses$scores)
### Preprocess with PCA with caret

library(caret)
### The number of principal components kept can be decided by
### 1. thres: set the threshold for cumulative variance
### 2. pcaComp: set explicitlythe amount to be kept
### If pcaComp is set, thres would be ignored

ctrl <- trainControl(preProcOptions = list(thres = 0.90,
                                           pcaComp = 5))
md = train(SalePrice ~ ., data = houseTrain,
           method = 'glmnet',
           preProc = 'pca',
           family = 'multinomial',
           trControl = ctrl)

### The predictors included in the final model
md$finalModel$xNames
