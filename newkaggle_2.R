library(Metrics)
library(e1071)
library(data.table)
library(kernlab)

#IMPORT ORIGINAL DATASETS
traindf <- fread('statistical-learning-hw03-2022/train_hw03.csv') 
testdf <- fread('statistical-learning-hw03-2022/test_hw03.csv') 


#IMPORT NEW CONNECTOME DF
train_new <- fread('Train_Connectome_df.csv',header = TRUE)
test_new <- fread('Test_Connectome_df.csv',header = TRUE)



### add Features as age,sex from originl dataset
#dataset must be named traindf and testdf

#train
train_new <- data.frame(train_new, age=traindf$age, sex= ((traindf$sex=='male')*1))
#test
test_new <- data.frame(test_new, age=testdf$age, sex = ((testdf$sex=='male')*1))






#binding together the data to perform kpca
total_new <- rbind(train_new, test_new)


################# KPCA -> SVM PIPELINE
dt2<-as.matrix(total_new)       
rbf2<-rbfdot(sigma=0.01)   
km2<-kernelMatrix(rbf2,dt2)
N_feat=50
kpc2 <- kpca(km2,data=total_new,kernel="rbfdot",kpar=list(sigma=0.2),features=N_feat)

# CREATE DATAFRAMES FROM KERNEL COMPONENTS
kern_comp_test <- pcv(kpc2)
kern_comp_test2 <- data.frame(kern_comp_test)

#SPLIT IN TRAIN AND TEST
train_kern_kaggle <- data.frame(kern_comp_test2[1:600,], y=as.factor(traindf$y))
test_kern_kaggle <- kern_comp_test2[601:799,]



##ACCURACY 
acc_list <- c()
n=100
N=50 #N. of test 
for (i in 1:n){
  
  if(i/n==0.5) print('halfway')
  
  #randomply split data
  idx <- sample(nrow(train_kern_kaggle),N,replace = FALSE)
  kern_train_tr <- train_kern_kaggle[-idx,]
  kern_train_ts <-train_kern_kaggle[idx,]
  
  #train
  svmfit_kern_4 = svm(y ~ ., data = kern_train_tr, kernel = "radial", gamma=0.001, cost = 100, scale = TRUE)
  
  #predict
  pred4_train <- predict(svmfit_kern_4,kern_train_ts[-51])
  #save accuracy
  acc_list <- append(acc_list,accuracy(kern_train_ts$y,pred4_train))
}

hist(acc_list, breaks = 20)
summary(acc_list) ## 0.64 mean



#FIT WITH TRAIN
svmfit_kern4 = svm(y ~ ., data = train_kern_kaggle, kernel = "radial", gamma=0.001, cost = 100, scale = TRUE)
summary(svmfit_kern4)


##ATTEMPT 4 - kaggle 0.611
#cost = 100
pred_kern_test4 <- predict(svmfit_kern4,test_kern_kaggle)
df <- data.frame(testdf$id,pred_kern_test4)
colnames(df) <- c('id','target')
df
write.csv(df,file = 'Prediction_kern4.csv',row.names = F )



################## ABSOLUTE CONNECTOME DATASET ECC------------------------------ 
#### 
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



##ACCURACY 
acc_list <- c()
n=100
N=50 #N. of test 
for (i in 1:n){
  
  if(i/n==0.5) print('halfway')
  
  #randomply split data
  idx <- sample(nrow(train_kern_kaggle),N,replace = FALSE)
  kern_train_tr <- train_kern_kaggle[-idx,]
  kern_train_ts <-train_kern_kaggle[idx,]
  
  #train
  svmfit_kern_5 = svm(y ~ ., data = kern_train_tr, kernel = "radial", gamma=0.01, cost = 1000, scale = TRUE)
  
  #predict
  pred5_train <- predict(svmfit_kern_5,kern_train_ts[-(N_feat+1)])
  #save accuracy
  acc_list <- append(acc_list,accuracy(kern_train_ts$y,pred5_train))
}

hist(acc_list, breaks = 20)
length(acc_list)
summary(acc_list) ## 0.60 mean



svmfit_kern_5 = svm(y ~ ., data = train_kern_kaggle, kernel = "radial", gamma=0.01, cost = 1000, scale = TRUE)

##ATTEMPT 5 - kaggle 0.611
#cost = 100
pred_kern_test5 <- predict(svmfit_kern_5,test_kern_kaggle)

table(pred_kern_test4,pred_kern_test5)
df <- data.frame(testdf$id,pred_kern_test5)
colnames(df) <- c('id','target')
df
write.csv(df,file = 'Prediction_kern5.csv',row.names = F )



## tuning
tune_out <- 
  tune.svm(x = train_kern_kaggle[, -51], y = train_kern_kaggle[, 51], 
           type = "C-classification", 
           kernel = "radial", cost = 10^(1:5), 
           gamma = c(0.1,0.01,0.001,0.0001))


summary(tune_out)
1 - tune_out$best.performance



############BEST ATTEMPT!!! ABSOLUTE VALUE ACCURACY 0.64 -----------------------

svmfit_kern_6 = svm(y ~ ., data = train_kern_kaggle, kernel = "radial", gamma=10^(-4), cost = 100, scale = TRUE)
pred_kern_test6 <- predict(svmfit_kern_6,test_kern_kaggle)
table(pred_kern_test5,pred_kern_test6)
df <- data.frame(testdf$id,pred_kern_test6)
colnames(df) <- c('id','target')
df
write.csv(df,file = 'Prediction_kern6.csv',row.names = F )

## -----------------------------------------------------------------------------