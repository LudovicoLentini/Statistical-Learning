library(Metrics)
library(e1071)
library(data.table)
library(kernlab)
library(ggplot2)
library(caret)
#install.packages('klaR')
library(klaR)



traindf <- fread('statistical-learning-hw03-2022/train_hw03.csv') 


# ROI 116, time slots 115
nroi=116
ntime=115


## CREATE LIST OF DATASETS OF A SINGLE OBSERVATION -------------------------------
#MATRIX ROIXTIME for every obs

train_OBS_l = list()
for (i in 1:nrow(traindf)){   #scan every observation 
  print(i) 
  
  #each time, create empty matrix of nroixntime
  df_obs <- matrix(NA,nrow = nroi, ncol = ntime)
  for (j in 1:nroi){
    ##fill the matrix with slices of the obs 
    df_obs[j,] <- as.numeric(traindf[i,(5+(j-1)*ntime):(5+(ntime-1) +(j-1)*ntime)])
  }
  #save in the list of datasets
  train_OBS_l <- append(train_OBS_l,list(df_obs))
}
remove(df_obs)

## Now we have every observation in order in a df list
train_OBS_l[[1]] 


### CONNECTOME DATASET ---------------------------------------------------------
## DEFINE FUNCTION TO CREATE CONNECTOME

Create_Connectdf <- function(df_list, nroi1){
  OBS=length(df_list)
  #Empty matrix to fill, dimension reduced
  Connect_diag_mat1 <- matrix(NA,nrow = OBS, ncol = nroi1*(nroi1-1)/2)
  
  for ( i in 1:OBS){
    #1. scan all observation
    #2. make corr matrix
    cor_mat_i <- cor(t(df_list[[i]]))
    ## 2.1  HANDLE THE NAS!
    cor_mat_i[is.na(cor_mat_i)] <- 0
    #3. take only upper triangular matrix without the diagonal, and flatten 
    cor_arr_i <- cor_mat_i[upper.tri(cor_mat_i, diag = F)]
    remove(cor_mat_i)
    #4. save it in new matrix
    Connect_diag_mat1[i,] <- cor_arr_i
  }
  return(Connect_diag_mat1)
}

Connect_diag_mat_ <- Create_Connectdf(train_OBS_l,nroi)

Connect_diag_mat2 <- data.frame(Connect_diag_mat_,
                                traindf$age,traindf$sex,y=as.factor(traindf$y))

hist(Connect_diag_mat_, breaks = 100)


###############  KPCA + SVM TRY ------------------------------------------------------

dt<-as.matrix(Connect_diag_mat2[,-c(6672,6673)])  
rbf<-rbfdot(sigma=0.01)   
km<-kernelMatrix(rbf,dt)
kpc <- kpca(km,data=Connect_diag_mat2[,-c(6672,6673)],kernel="rbfdot",kpar=list(sigma=0.01),features=50)

#KERNEL COMPONENTS
kern_comp <- pcv(kpc)
kern_comp2 <- data.frame(kern_comp, y=as.factor(traindf$y))

# PLOT WITH Y COLORED
plot(kern_comp2$X1,kern_comp2$X2,col= kern_comp2$y,lwd=2)


# TRY SVM TESTS (Accuracy smoothed with 10 tries)
accur_list <-c()
for (i in 1:10){
  svmfit3 = svm(y ~ ., data = kern_comp2, kernel = "radial", gamma=0.001, cost = 100, scale = TRUE,cross=10)
  accur_list <-append(accur_list,svmfit3$tot.accuracy)
}

summary(accur_list)
mean(accur_list)
svmfit3$tot.accuracy

#BEST ACCURACY 63.6
#FEATURES 50, kernel sigma 0.01
# GAMMA 0.001, COST 100
#svmfit3 = svm(y ~ ., data = kern_comp2, kernel = "radial", gamma=0.001, cost = 100, scale = TRUE,cross=10)


#### IS SVM WIT CROSS=10 A REAL CV?? ------------------------------------------

#try personal CV

acc_list <- c()
n=200
N=50
for (i in 1:n){
  
  if(i/n==0.5) print('halfway')
  
  #randomply split data
  idx <- sample(nrow(kern_comp2),N,replace = FALSE)
  kern_train_tr <- kern_comp2[-idx,]
  kern_train_ts <-kern_comp2[idx,]
  
  #train
  svmfit_4 = svm(y ~ ., data = kern_train_tr, kernel = "radial", gamma=0.001, cost = 100, scale = TRUE)
  
  #predict
  pred4_train <- predict(svmfit_4,kern_train_ts[-51])
  #save accuracy
  acc_list <- append(acc_list,accuracy(kern_train_ts$y,pred4_train))
}

boxplot(acc_list)
hist(acc_list,breaks=20)
summary(acc_list)


##train final model
svmfit_4 = svm(y ~ ., data = kern_comp2, kernel = "radial", gamma=0.001, cost = 100, scale = TRUE)



#specify the cross-validation method
ctrl <- trainControl(method = "cv", number = 5)




### ------------------KAGGLE WITH COnnect diag -> KPCA -> SVM: svmfit3 ---------

### TRANSFORM THE TEST DATA AS BEFORE

testdf <- fread('statistical-learning-hw03-2022/test_hw03.csv') 

## CREATE LIST OF DATASETS OF A SINGLE OBSERVATION -------------------------------
#MATRIX ROIXTIME for every obs

## sligtly modify the code


test_OBS_l = list()
for (i in 1:nrow(testdf)){   #scan every observation 
  print(i) 
  
  #each time, create empty matrix of nroixntime
  df_obs <- matrix(NA,nrow = nroi, ncol = ntime)
  for (j in 1:nroi){
    ##fill the matrix with slices of the obs 
    df_obs[j,] <- as.numeric(testdf[i,(4+(j-1)*ntime):(4+(ntime-1) +(j-1)*ntime)])
  }
  #save in the list of datasets
  test_OBS_l <- append(test_OBS_l,list(df_obs))
}
remove(df_obs)

## Now we have every observation in order in a df list

##check if code works well
heatmap(test_OBS_l[[100]],Rowv = NA,Colv = NA)
testdf[1,4:(4+ntime)]

i=100
j=100
testdf[i,(4+(j-1)*ntime):(4+(ntime-1) +(j-1)*ntime)]
test_OBS_l[[1]][j,]



### CREATE NEW CONNECTOME DATASET
Connect_test <- Create_Connectdf(test_OBS_l,nroi)


#CHECK IF CORRELATIONS ARE CORRECT
Connect_test[1,1:20]
testdf[1,(4+(1-1)*ntime):(4+(ntime-1) +(1-1)*ntime)]
testdf[1,(4+(2-1)*ntime):(4+(ntime-1) +(2-1)*ntime)]

Connect_test[i,1:20]
i=1
j=1
j2=6
as.numeric(Connect_test[i,j2-1])
cor(as.numeric(testdf[i,(4+(j-1)*ntime):(4+(ntime-1) +(j-1)*ntime)]),as.numeric(testdf[i,(4+(j2-1)*ntime):(4+(ntime-1) +(j2-1)*ntime)]))

###????
i=1
j=1
j2=6
cor(t(test_OBS_l[[i]]))[1:10,1:10]
cor(as.numeric(testdf[i,(4+(j-1)*ntime):(4+(ntime-1) +(j-1)*ntime)]),as.numeric(testdf[i,(4+(j2-1)*ntime):(4+(ntime-1) +(j2-1)*ntime)]))

Connect_test[i,1:10]

###### ok the correlations seems to be in right place

## does this test dataset have similar features?
heatmap(Connect_test[,1:100],Rowv = NA,Colv = NA)

heatmap(Connect_diag_mat_[,1:100],Rowv = NA,Colv = NA)

##feature N18 seems similar in both datasets, train and test
par(mfrow=c(2,1))
plot(Connect_test[,18])
plot(Connect_diag_mat_[,18])

#also feature 90
par(mfrow=c(2,1))
plot(Connect_test[,90])
plot(Connect_diag_mat_[,90])


# .... SO the test dataset seems to be pretty similar, no error in the data manipulation
par(mfrow=c(1,1))
hist(Connect_test, breaks=100)

##is different from the dataset before?
length(Connect_test[1,])
length(Connect_diag_mat_[1,])
mean(Connect_test[10,]==Connect_diag_mat_[10,])





#### SOOOO LETS TRY 

Connect_test2 <- data.frame(Connect_test,
                                testdf$age)

#### KPCA -> SVM 
dt2<-as.matrix(Connect_test2)       
rbf2<-rbfdot(sigma=0.01)   
km2<-kernelMatrix(rbf2,dt2)
kpc2 <- kpca(km2,data=Connect_test2,kernel="rbfdot",kpar=list(sigma=0.2),features=50)

#KERNEL COMPONENTS
kern_comp_test <- pcv(kpc2)
kern_comp_test2 <- data.frame(kern_comp_test)


plot(kern_comp_test2$X1,kern_comp_test2$X2)
##seems quite similar

###COMPARE THE KPCA DATASETS
plot(kern_comp2$X2)
plot(kern_comp_test2$X2)


##USE THE CORRECT SVM 
#svmfit3
summary(svmfit3)

pred <- predict(svmfit3, kern_comp_test2)
table(pred)



df <- data.frame(testdf$id,pred)
colnames(df) <- c('id','target')
#df
write.csv(df,file = 'Prediction3_Connectome.csv',row.names = F )

#### CONFRONT WITH svmfit_4
pred4 <- predict(svmfit_4, kern_comp_test2)

table(pred,pred4)

