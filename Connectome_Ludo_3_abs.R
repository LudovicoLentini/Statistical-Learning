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

Create_Connect_abs_df <- function(df_list, nroi1){
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
    Connect_diag_mat1[i,] <- abs(cor_arr_i)
  }
  return(Connect_diag_mat1)
}

Connect_diag_mat_abs_ <- Create_Connect_abs_df(train_OBS_l,nroi)

Connect_diag_mat_abs2 <- data.frame(Connect_diag_mat_abs_,
                                traindf$age,traindf$sex,y=as.factor(traindf$y))

hist(Connect_diag_mat_abs_, breaks = 100)





dt<-as.matrix(Connect_diag_mat_abs2[,-c(6672,6673)])  
rbf<-rbfdot(sigma=0.01)   
km<-kernelMatrix(rbf,dt)
kpc <- kpca(km,data=Connect_diag_mat_abs2[,-c(6672,6673)],kernel="rbfdot",kpar=list(sigma=0.2),features=50)

#KERNEL COMPONENTS
kern_comp <- pcv(kpc)
kern_comp2 <- data.frame(kern_comp, y=as.factor(traindf$y))



acc_list <- c()
n=100
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
summary(acc_list)

