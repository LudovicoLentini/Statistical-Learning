##Part B LOCO 
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




##rename for PART B LOCO  
train <- data.frame(train_kern_kaggle[,1:10], y=train_kern_kaggle$y)
train$y
train_num <- train

#Create second train for part B LOCO
train_num$y <- as.numeric(train$y=='autism')
train_num$y

###########LOCO ALGORITM ------------------------------------------------------
#1. Randomly split the data in two
Proportion <- 10
n1 <- floor(nrow(train)/Proportion)
n1
idx <- sample(nrow(train),n1,replace = FALSE)
D1 <- train[-idx,]
D2 <-train[idx,]

Ncols <- ncol(D1)

#2. Compute estimate f_n1_hat
svmfit_LOCO = svm(y ~ ., data = D1, kernel = "radial", gamma=0.01, cost = 100, scale = TRUE)


#3. select A variable j and compute estimate f_n1_wj_hat
Var_loco <- 1

seq_cols <- seq(1,Ncols,1)
#columns without j-th var
seq_cols_w = seq_cols[! seq_cols %in% Var_loco]


D1_wj <- D1[,seq_cols_w]
D2_wj <- D2[,seq_cols_w]
#new dataset D1_wj and D2_wj without variable

svmfit_LOCO_wj = svm(y ~ ., data = D1_wj, kernel = "radial", gamma=0.01, cost = 100, scale = TRUE)


#4. Predict the f_n1_hat and f_n1_wj_hat with the D2 dataset 
#Then calculate difference between the accuracy, delta_acuracy
f_n1_hat <- predict(svmfit_LOCO,D2[-(Ncols)])
f_n1_wj_hat <- predict(svmfit_LOCO_wj,D2_wj[-(Ncols-1)])

delta_acc <- accuracy(D2$y,f_n1_hat) - accuracy(D2_wj$y,f_n1_wj_hat)
delta_acc


#CREATE FUNCTION THAT DOES DELTA ACCURACY 
# Param:
      # df, dataframe to split
      # Proportion, how much is the D2 test part has to be divided
      # Var_loco: the Variable we want to evaluate the LOCO
      #method: can be 'diff' or 'frac'

Create_LOCO_Delta_Acc <- function(df = train,Proportion=10, Var_loco, method='frac'){
  
  #1. Randomly split the data in two 
  n1 <- floor(nrow(df)/Proportion)
  
  idx <- sample(nrow(df),n1,replace = FALSE)
  D1 <- df[-idx,]
  D2 <-df[idx,]
  Ncols <- ncol(D1)
  
  #2. Compute estimate f_n1_hat
  svmfit_LOCO = svm(y ~ ., data = D1, kernel = "radial", gamma=0.01, cost = 100, scale = TRUE)
  
  #3. select A variable j and compute estimate f_n1_wj_hat
  
  seq_cols <- seq(1,Ncols,1)
  #columns without j-th var
  seq_cols_w = seq_cols[! seq_cols %in% Var_loco]
  D1_wj <- D1[,seq_cols_w]
  D2_wj <- D2[,seq_cols_w]
  
  #new dataset D1_wj and D2_wj without variable
  
  svmfit_LOCO_wj = svm(y ~ ., data = D1_wj, kernel = "radial", gamma=0.01, cost = 100, scale = TRUE)
  

  #4. Predict the f_n1_hat and f_n1_wj_hat with the D2 dataset 
  #Then calculate difference between the accuracy, delta_acuracy
  f_n1_hat <- predict(svmfit_LOCO,D2[-(Ncols)])
  f_n1_wj_hat <- predict(svmfit_LOCO_wj,D2_wj[-(Ncols-1)])
  
  if (method=='diff') {
    delta_acc <- accuracy(D2$y,f_n1_hat) - accuracy(D2_wj$y,f_n1_wj_hat)
    return(delta_acc)
  }
  else if (method == 'frac') {
    delta_acc <- accuracy(D2$y,f_n1_hat)/accuracy(D2_wj$y,f_n1_wj_hat)
    return(delta_acc)
  }
  
}

Create_LOCO_Delta_Acc(Var_loco = 1)


##REPEAT THE PROCESS TO HAVE A BOOTSTRAPPED VALUE:

N <- 10
##Choose variable to exclude
Var_j=1

Proportion <- 10
delta_acc_list <-c()
for(k in 1:N){
  print(k)
  delta_acc <- Create_LOCO_Delta_Acc(train,Proportion,Var_j)
  delta_acc_list <- append(delta_acc_list, delta_acc)
}

hist(delta_acc_list, breaks=30)
Theta_j <- median(delta_acc_list)
Theta_j

###NON parametric bootstrap 

npbs <- np.boot(x = delta_acc_list, R=10000,statistic = median, level = 0.95)
npbs$t0
npbs$percent
as.numeric(npbs$percent[1])
hist(npbs$boot.dist, breaks = 30)


###FUNCTION TO MAKE N times

Create_LOCO_list <- function(N,df,Proportion,Var_j,method='frac'){
  
  delta_acc_list <-c()
  for(k in 1:N){
    #print(k)
    delta_acc <- Create_LOCO_Delta_Acc(df,Proportion,Var_j,method)
    delta_acc_list <- append(delta_acc_list, delta_acc)
  }
  return(delta_acc_list)
}

Create_LOCO_list(50,train,5,1)
system.time(
  L_l <-Create_LOCO_list(100,train,5,1)
)


###PARALLELIZE PROCESS

Create_LOCO_for_parall <- function(i,df = train,Proportion=Prop, Var_loco=Var_j){
  #1. Randomly split the data in two 
  n1 <- floor(nrow(df)/Proportion)
  
  idx <- sample(nrow(df),n1,replace = FALSE)
  D1 <- df[-idx,]
  D2 <-df[idx,]
  Ncols <- ncol(D1)
  
  #2. Compute estimate f_n1_hat
  svmfit_LOCO = svm(y ~ ., data = D1, kernel = "radial", gamma=0.01, cost = 100, scale = TRUE)
  
  #3. select A variable j and compute estimate f_n1_wj_hat
  seq_cols <- seq(1,Ncols,1)
  #columns without j-th var
  seq_cols_w = seq_cols[! seq_cols %in% Var_loco]
  D1_wj <- D1[,seq_cols_w]
  D2_wj <- D2[,seq_cols_w]
  
  #new dataset D1_wj and D2_wj without variable
  
  svmfit_LOCO_wj = svm(y ~ ., data = D1_wj, kernel = "radial", gamma=0.01, cost = 100, scale = TRUE)
  
  
  #4. Predict the f_n1_hat and f_n1_wj_hat with the D2 dataset 
  #Then calculate difference between the accuracy, delta_acuracy
  f_n1_hat <- predict(svmfit_LOCO,D2[-(Ncols)])
  f_n1_wj_hat <- predict(svmfit_LOCO_wj,D2_wj[-(Ncols-1)])
  
  
  
  # if (method=='diff') {
  #  delta_acc <- accuracy(D2$y,f_n1_hat) - accuracy(D2_wj$y,f_n1_wj_hat)
  #  return(delta_acc)
  # }
  # else if (method == 'frac') {
  #  delta_acc <- accuracy(D2$y,f_n1_hat)/accuracy(D2_wj$y,f_n1_wj_hat)
  #  return(delta_acc)
  #}
  
  delta_acc_diff <- accuracy(D2$y,f_n1_hat) - accuracy(D2_wj$y,f_n1_wj_hat)
  delta_acc_frac <- accuracy(D2$y,f_n1_hat) / accuracy(D2_wj$y,f_n1_wj_hat)
  return(c(delta_acc_diff,delta_acc_frac))
  
}


Var_j=1
Prop = 4
Create_LOCO_for_parall(1)

### Parallelization try

  N=100 
  Fake_N_list <- seq(1,N,1)
  Prop = 4
  metodo='diff'
  
  cl<-makeCluster(4,type="SOCK")
  
  clusterExport(cl, "train")
  clusterExport(cl, "Var_j")
  clusterExport(cl, "Prop")
  clusterExport(cl, "metodo")
  clusterEvalQ(cl, library("e1071"))
  clusterEvalQ(cl, library("Metrics"))
  
  system.time(  
  LOCO_list<-parLapply(cl,Fake_N_list,Create_LOCO_for_parall)
)
stopCluster(cl)

a <-Unpack_list(LOCO_list)
LOCO_list2 <- a[[1]]
LOCO_list2
LOCO_list3 <- a[[2]]
hist(LOCO_list2)
hist(LOCO_list3)

Unpack_list <- function(LIST){
  arr1 <- c()
  arr2<- c()
  for(i in 1:length(LIST)){
    #print(LOCO_list[[i]][1])
    arr1 <- append(arr1,LIST[[i]][1])
    arr2 <- append(arr2,LIST[[i]][2])
  }
  return(list(arr1,arr2))
}

### EVALUATE VAR IMPORTANCE OF ALL VARS WITH CONF INT AND PLOT: -------------------
#PARALLELIZATION METHOD 2 ------------------------------------------------
N=600
Prop <- 4
Variables <- seq(1,10,1)
Fake_N_list <- seq(1,N,1)

Theta_j_list <- c()
Lower_bund_list <- c()
Upper_bund_list <- c()

Theta_j_list2 <- c()
Lower_bund_list2 <- c()
Upper_bund_list2 <- c()

system.time(
  for (Var_j in Variables){
    print(Var_j)
    
    #PARALLELIZATION
    cl<-makeCluster(detectCores(),type="SOCK")
    
    #IMPORTING LIBRARIES TO COMPUTE 
    clusterEvalQ(cl, library("e1071"))
    clusterEvalQ(cl, library("Metrics"))
    
    clusterExport(cl, "train")
    clusterExport(cl, "Var_j")
    clusterExport(cl, "Prop")

    LOCO_list<-parLapply(cl,Fake_N_list,Create_LOCO_for_parall)
    
    stopCluster(cl)
    a <- Unpack_list(LOCO_list)
    delta_acc_diff <- a[[1]]
    delta_acc_frac <- a[[2]]
    
    
    
    #delta_acc_list <- unlist(LOCO_list)
    
    
    #NON PARAMETRIC BOOTSTRAP CI
    #hist(delta_acc_list)
    
    npbs <- np.boot(x = delta_acc_diff, R=50000,statistic = median, level = 0.95)
    hist(npbs$boot.dist,breaks=20)
    #Save results
    Theta_j_list<-append(Theta_j_list,npbs$t0)
    Lower_bund_list <- append(Lower_bund_list,as.numeric(npbs$percent[1])) 
    Upper_bund_list <- append(Upper_bund_list,as.numeric(npbs$percent[2])) 
    
    npbs2 <- np.boot(x = delta_acc_frac, R=50000,statistic = median, level = 0.95)
    hist(npbs2$boot.dist,breaks=20)
    #Save results
    Theta_j_list2<-append(Theta_j_list2,npbs2$t0)
    Lower_bund_list2<- append(Lower_bund_list2,as.numeric(npbs2$percent[1])) 
    Upper_bund_list2 <- append(Upper_bund_list2,as.numeric(npbs2$percent[2])) 
  }

)




df_CI_perc <- data.frame(Theta_j_list2,Lower_bund_list2,Upper_bund_list2)
df_CI_perc <- round((df_CI_perc -1)*100,4) 
df_CI_perc

df_CI_diff <- round(data.frame(Theta_j_list,Lower_bund_list,Upper_bund_list),7)
df_CI_diff 

par(mfrow=c(2,1))
Plot_CI('frac',df_CI_perc[,1],df_CI_perc[,2],df_CI_perc[,3])
Plot_CI('diff',df_CI_diff[,1],df_CI_diff[,2],df_CI_diff[,3])




###                 PLOT------------------------------------------------------- 

Plot_CI <- function(metodo, Theta, Lower,Upper){
  
  if (metodo=='diff'){
    space <- 0.003
    y_label = 'Accuracy change'
  }
  else if(metodo=='frac'){
    space <- 0.3
    y_label = '% Accuracy change'
  }
  else if (metodo == 'loco'){
    space <- 0.003
    y_label = ' value change'
    
  }

  h_value=0
  Variables <- seq(1,10,1)
  y_lim_1 <- c(min(Lower)-space,max(Upper)+space)
  
  plot(Variables,Theta, ylim=y_lim_1,
       ylab= y_label, xlab='Variables', 
       main= paste('Confidence intervals of median J-Variable',y_label) ,
       pch=19, lwd=3, col='black')
  abline(h=h_value)
  
  for (idx in Variables){
    color='blue'
    if (Lower[idx]>h_value){
      color = "green"
    }
    else if (Upper[idx]<h_value){
      color = "red"
    }
    points(c(idx,idx),c(Lower[idx]-abs(Lower[idx]*0.05), Upper[idx]+abs(Upper[idx]*0.05)), type='l',
           lwd=6, col=color)
  }
  grid(10,10, lwd = 2) 
  axis(1, at=Variables,labels=Variables, col.axis="black", las=1)
}



###### METHOD WITH DIFFERENCE OF VALUES ----------------------------------------
#1. Randomly split the data in two



Create_LOCO_2 <- function(df,Proportion){
  n1 <- floor(nrow(df)/Proportion)
  idx <- sample(nrow(df),n1,replace = FALSE)
  D1 <- train[-idx,]
  D2 <-train[idx,]
  
  Ncols <- ncol(D1)
  
  #2. Compute estimate f_n1_hat
  svmfit_LOCO = svm(y ~ ., data = D1, kernel = "radial", gamma=0.01, cost = 100, scale = TRUE)
  f_n1_hat <- predict(svmfit_LOCO,D2[-(Ncols)])
  
  Delta_j_list <- c()
  
  Variables <- seq(1,10,1)
  #3. select A variable j and compute estimate f_n1_wj_hat
  for (Var_loco in Variables){
    seq_cols <- seq(1,Ncols,1)
    #columns without j-th var
    seq_cols_w = seq_cols[! seq_cols %in% Var_loco]
    
    
    D1_wj <- D1[,seq_cols_w]
    D2_wj <- D2[,seq_cols_w]
    #new dataset D1_wj and D2_wj without variable
    
    svmfit_LOCO_wj = svm(y ~ ., data = D1_wj, kernel = "radial", gamma=0.01, cost = 100, scale = TRUE)
    
    
    #4. Predict the f_n1_hat and f_n1_wj_hat with the D2 dataset 
    #Then calculate difference between the accuracy, delta_acuracy
 
    f_n1_wj_hat <- predict(svmfit_LOCO_wj,D2_wj[-(Ncols-1)])
    
    real_y <- as.numeric(D2$y=="autism")
    f_hat <- as.numeric(f_n1_hat =="autism")
    f_wj_hat <- as.numeric(f_n1_wj_hat =="autism")
    
    delta_j <- abs(real_y-f_wj_hat) - abs(real_y - f_hat)
    Delta_j_list <- append(Delta_j_list, delta_j)
    
  }
  
  return(Delta_j_list)
  
}





par(mfrow=c(1,1))

Delta_list <- Create_LOCO_2(train,Proportion=4)

alpha=0.9

npbt_method2 <-  np.boot(Delta_list, statistic = mean,level = alpha)
npbt_method2$t0
npbt_method2$percent
hist(npbt_method2$boot.dist)






Create_LOCO_3 <- function(df,Proportion,alpha=0.9){
  n1 <- floor(nrow(df)/Proportion)
  idx <- sample(nrow(df),n1,replace = FALSE)
  D1 <- df[-idx,]
  D2 <-df[idx,]
  
  Ncols <- ncol(D1)
  
  #2. Compute estimate f_n1_hat
  svmfit_LOCO = svm(y ~ ., data = D1, kernel = "radial", gamma=0.01, cost = 100, scale = TRUE)
  f_n1_hat <- predict(svmfit_LOCO,D2[-(Ncols)])
  
  Delta_j_list <- c()
  Lower <- c()
  Upper <- c()
  
  
  Variables <- seq(1,10,1)
  #3. select A variable j and compute estimate f_n1_wj_hat
  for (Var_loco in Variables){
    seq_cols <- seq(1,Ncols,1)
    #columns without j-th var
    seq_cols_w = seq_cols[! seq_cols %in% Var_loco]
    
    
    D1_wj <- D1[,seq_cols_w]
    D2_wj <- D2[,seq_cols_w]
    #new dataset D1_wj and D2_wj without variable
    
    svmfit_LOCO_wj = svm(y ~ ., data = D1_wj, kernel = "radial", gamma=0.01, cost = 100, scale = TRUE)
    
    
    #4. Predict the f_n1_hat and f_n1_wj_hat with the D2 dataset 
    #Then calculate difference between the accuracy, delta_acuracy
    
    f_n1_wj_hat <- predict(svmfit_LOCO_wj,D2_wj[-(Ncols-1)])
    
    real_y <- as.numeric(D2$y=="autism")
    f_hat <- as.numeric(f_n1_hat =="autism")
    f_wj_hat <- as.numeric(f_n1_wj_hat =="autism")
    
    delta_j <- abs(real_y-f_wj_hat) - abs(real_y - f_hat)
    
    npbt_method2 <-  np.boot(delta_j, statistic = mean,level = alpha)
    Delta_j_list <- append(Delta_j_list, npbt_method2$t0)
    Lower <- append(Lower,as.numeric(npbt_method2$percent[1]))
    Upper <- append(Upper,as.numeric(npbt_method2$percent[2]) )
    
  }
  
  Df_CI <- data.frame(Delta_j_list,Lower,Upper)
  
  return(Df_CI)
}

Df_CI_new <- Create_LOCO_3(train_num,4,0.95)
Plot_CI('loco',Df_CI_new[,1],Df_CI_new[,2],Df_CI_new[,3])


Create_LOCO_4 <- function(N,df,Proportion,alpha=0.9){
  
  Delta_mat <- matrix(NA,10,N)
  Upper_mat <- matrix(NA,10,N)
  Lower_mat <- matrix(NA,10,N)
  
  for (i in 1:N){
    
    print(i)
    n1 <- floor(nrow(df)/Proportion)
    idx <- sample(nrow(df),n1,replace = FALSE)
    D1 <- df[-idx,]
    D2 <-df[idx,]
    
    Ncols <- ncol(D1)
    
    #2. Compute estimate f_n1_hat
    svmfit_LOCO = svm(y ~ ., data = D1, kernel = "radial", gamma=0.01, cost = 100, scale = TRUE)
    f_n1_hat <- predict(svmfit_LOCO,D2[-(Ncols)])
    
    Delta_j_list <- c()
    Lower <- c()
    Upper <- c()
    
    
    Variables <- seq(1,10,1)
    #3. select A variable j and compute estimate f_n1_wj_hat
    for (Var_loco in Variables){
      seq_cols <- seq(1,Ncols,1)
      #columns without j-th var
      seq_cols_w = seq_cols[! seq_cols %in% Var_loco]
      
      
      D1_wj <- D1[,seq_cols_w]
      D2_wj <- D2[,seq_cols_w]
      #new dataset D1_wj and D2_wj without variable
      
      svmfit_LOCO_wj = svm(y ~ ., data = D1_wj, kernel = "radial", gamma=0.01, cost = 100, scale = TRUE)
      
      
      #4. Predict the f_n1_hat and f_n1_wj_hat with the D2 dataset 
      #Then calculate difference between the accuracy, delta_acuracy
      
      f_n1_wj_hat <- predict(svmfit_LOCO_wj,D2_wj[-(Ncols-1)])
      
      real_y <- as.numeric(D2$y=="autism")
      f_hat <- as.numeric(f_n1_hat =="autism")
      f_wj_hat <- as.numeric(f_n1_wj_hat =="autism")
      
      delta_j <- abs(real_y-f_wj_hat) - abs(real_y - f_hat)
      
      npbt_method2 <-  np.boot(delta_j, statistic = mean,level = alpha)
      Delta_j_list <- append(Delta_j_list, npbt_method2$t0)
      Lower <- append(Lower,as.numeric(npbt_method2$percent[1]))
      Upper <- append(Upper,as.numeric(npbt_method2$percent[2]) )
      
    }
    Delta_mat[,i] <- Delta_j_list
    Upper_mat[,i] <- Upper
    Lower_mat[,i] <- Lower
    
  }
  
  Delta_j_m <- rowMedians(Delta_mat)
  Lower_m <- rowMedians(Lower_mat)
  Upper_m <- rowMedians(Upper_mat)
  
  Df_CI <- data.frame(Delta_j_m,Lower_m,Upper_m)
  
  return(Df_CI)
}

Df_CI_new2 <- Create_LOCO_4(1,train,5,0.95)
Plot_CI('loco',Df_CI_new2[,1],Df_CI_new2[,2],Df_CI_new2[,3])

Df_CI_new2 <- Create_LOCO_4(1,train,5,0.95)

################ LOCO FOR NUMERIC SVM REGRESSION -------------------------------

Create_LOCO_num <- function(df,Proportion,alpha=0.9){
  n1 <- floor(nrow(df)/Proportion)
  idx <- sample(nrow(df),n1,replace = FALSE)
  D1 <- df[-idx,]
  D2 <-df[idx,]
  
  Ncols <- ncol(D1)
  
  #2. Compute estimate f_n1_hat
  svmfit_LOCO = svm(y ~ ., data = D1, kernel = "radial", gamma=0.01, cost = 100, scale = TRUE)
  f_n1_hat <- predict(svmfit_LOCO,D2[-(Ncols)])
  
  Delta_j_list <- c()
  Lower <- c()
  Upper <- c()
  
  
  Variables <- seq(1,10,1)
  #3. select A variable j and compute estimate f_n1_wj_hat
  for (Var_loco in Variables){
    seq_cols <- seq(1,Ncols,1)
    #columns without j-th var
    seq_cols_w = seq_cols[! seq_cols %in% Var_loco]
    
    
    D1_wj <- D1[,seq_cols_w]
    D2_wj <- D2[,seq_cols_w]
    #new dataset D1_wj and D2_wj without variable
    
    svmfit_LOCO_wj = svm(y ~ ., data = D1_wj, kernel = "radial", gamma=0.01, cost = 100, scale = TRUE)
    
    #4. Predict the f_n1_hat and f_n1_wj_hat with the D2 dataset 
    #Then calculate difference between the accuracy, delta_acuracy
    
    f_n1_wj_hat <- predict(svmfit_LOCO_wj,D2_wj[-(Ncols-1)])
    
    real_y <- D2$y
    f_hat <- f_n1_hat
    f_wj_hat <- f_n1_wj_hat
    
    delta_j <- abs(real_y-f_wj_hat) - abs(real_y - f_hat)
    hist(delta_j, main = paste("hist of Delta j, var",Var_loco), breaks = 30)
    #print(delta_j)
    npbt_method2 <-  np.boot(delta_j,R=50000, statistic = median,level = alpha)
    Delta_j_list <- append(Delta_j_list, npbt_method2$t0)
    Lower <- append(Lower,as.numeric(npbt_method2$percent[1]))
    Upper <- append(Upper,as.numeric(npbt_method2$percent[2]) )
  }
  
  Df_CI <- data.frame(Delta_j_list,Lower,Upper)
  
  return(Df_CI)
}

Df_CI_num <- round(Create_LOCO_num(train_num,5,0.90),5)
Df_CI_num
Plot_CI('loco',Df_CI_num[,1],Df_CI_num[,2],Df_CI_num[,3])



Create_LOCO_num_2 <- function(N,df,Proportion,alpha=0.9){
  
  Delta_mat <- matrix(NA,10,N)
  Upper_mat <- matrix(NA,10,N)
  Lower_mat <- matrix(NA,10,N)
  
  for (i in 1:N){
    
    print(i)
    n1 <- floor(nrow(df)/Proportion)
    idx <- sample(nrow(df),n1,replace = FALSE)
    D1 <- df[-idx,]
    D2 <-df[idx,]
    
    Ncols <- ncol(D1)
    
    #2. Compute estimate f_n1_hat
    svmfit_LOCO = svm(y ~ ., data = D1, kernel = "radial", gamma=0.01, cost = 100, scale = TRUE)
    f_n1_hat <- predict(svmfit_LOCO,D2[-(Ncols)])
    
    Delta_j_list <- c()
    Lower <- c()
    Upper <- c()
    
    
    Variables <- seq(1,10,1)
    #3. select A variable j and compute estimate f_n1_wj_hat
    for (Var_loco in Variables){
      seq_cols <- seq(1,Ncols,1)
      #columns without j-th var
      seq_cols_w = seq_cols[! seq_cols %in% Var_loco]
      
      
      D1_wj <- D1[,seq_cols_w]
      D2_wj <- D2[,seq_cols_w]
      #new dataset D1_wj and D2_wj without variable
      
      svmfit_LOCO_wj = svm(y ~ ., data = D1_wj, kernel = "radial", gamma=0.01, cost = 100, scale = TRUE)
      
      
      #4. Predict the f_n1_hat and f_n1_wj_hat with the D2 dataset 
      #Then calculate difference between the accuracy, delta_acuracy
      
      f_n1_wj_hat <- predict(svmfit_LOCO_wj,D2_wj[-(Ncols-1)])
      
      real_y <- D2$y
      f_hat <- f_n1_hat
      f_wj_hat <- f_n1_wj_hat
      
      delta_j <- abs(real_y-f_wj_hat) - abs(real_y - f_hat)
      
      npbt_method2 <-  np.boot(delta_j, statistic = median,level = alpha)
      Delta_j_list <- append(Delta_j_list, npbt_method2$t0)
      Lower <- append(Lower,as.numeric(npbt_method2$percent[1]))
      Upper <- append(Upper,as.numeric(npbt_method2$percent[2]) )
      
    }
    Delta_mat[,i] <- Delta_j_list
    Upper_mat[,i] <- Upper
    Lower_mat[,i] <- Lower
    
  }
  
  Delta_j_m <- rowMedians(Delta_mat)
  Lower_m <- rowMedians(Lower_mat)
  Upper_m <- rowMedians(Upper_mat)
  
  Df_CI <- data.frame(Delta_j_m,Lower_m,Upper_m)
  
  return(Df_CI)
}



Df_CI_num2 <- round(Create_LOCO_num_2(25,train_num,4,0.95),5)
Df_CI_num2
Plot_CI('loco',Df_CI_num2[,1],Df_CI_num2[,2],Df_CI_num2[,3])



