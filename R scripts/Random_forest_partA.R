library(data.table)

library(Metrics)
library(e1071)
library(kernlab)

suppressMessages(require(plotly, quietly = T))
library(nptest)
library(snow)
#install.packages("matrixStats")
library(matrixStats)


#IMPORT ORIGINAL DATASETS
traindf <- fread('statistical-learning-hw03-2022/train_hw03.csv') 
testdf <- fread('statistical-learning-hw03-2022/test_hw03.csv') 


#IMPORT NEW CONNECTOME DF
train_new <- fread('Train_Connectome_df.csv',header = TRUE)
test_new <- fread('Test_Connectome_df.csv',header = TRUE)



### add Features as age,sex from original dataset
#dataset must be named traindf and testdf

#train
train_new <- data.frame(train_new, age=traindf$age, sex= ((traindf$sex=='male')*1))
#test
test_new <- data.frame(test_new, age=testdf$age, sex = ((testdf$sex=='male')*1))


# ABSOLUTE VALUE DATASET
train_new_abs <- abs(train_new)
test_new_abs <- abs(test_new)

#binding together the data to perform kpca
total_new_abs <- rbind(train_new_abs, test_new_abs)




################# KPCA -> SVM PIPELINE
dt2<-as.matrix(total_new_abs)       
rbf2<-rbfdot(sigma=0.01)   
km2<-kernelMatrix(rbf2,dt2)
N_feat=50
kpc2 <- kpca(km2,data=total_new_abs,kernel="rbfdot",kpar=list(sigma=0.2),features=N_feat)

# CREATE DATAFRAMES FROM KERNEL COMPONENTS
kern_comp_test <- pcv(kpc2)
kern_comp_test2 <- data.frame(kern_comp_test)

#SPLIT IN TRAIN AND TEST
train_kern_kaggle <- data.frame(kern_comp_test2[1:600,], y=as.factor(traindf$y))
test_kern_kaggle <- kern_comp_test2[601:799,]


#install.packages('randomForest')
library(randomForest)


rf.model <- randomForest(formula = y ~ ., mtry = sqrt(ncol(train_kern_kaggle)),
                         nodesize = 10,
                         sampsize = 100,
                         data = train_kern_kaggle)

rf.model

library(caret)

bestmtry <- tuneRF(train_kern_kaggle[-51],train_kern_kaggle[51],stepFactor = 1.2, improve = 0.01, trace=T, plot= T) 

###############
control <- trainControl(method='repeatedcv', 
                        number=10, 
                        repeats=3)
#Metric compare model is Accuracy
metric <- "Accuracy"

mtry <- sqrt(ncol(train_kern_kaggle))
tunegrid <- expand.grid(.mtry=mtry)

rf_default <- train(y~., 
                    data=train_kern_kaggle,
                    method='rf', 
                    metric='Accuracy', 
                    tuneGrid=tunegrid, 
                    trControl=control)
print(rf_default)



mtry <- sqrt(ncol(train_kern_kaggle))
#ntree: Number of trees to grow.
ntree <- 3


control <- trainControl(method='repeatedcv', 
                        number=10, 
                        repeats=3,
                        search = 'random')

#Random generate 15 mtry values with tuneLength = 15
set.seed(1)
rf_random <- train(y ~ .,
                   data=train_kern_kaggle,
                   method = 'rf',
                   metric = 'Accuracy',
                   tuneLength  = 15, 
                   trControl = control)
print(rf_random)





