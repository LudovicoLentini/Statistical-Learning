library(Metrics)
library(e1071)
library(data.table)
library(kernlab)
library(ggplot2)
library(caret)
#install.packages('klaR')
library(klaR)
library(igraph)
library(snow)


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


plot(train_OBS_l[[1]][1,],train_OBS_l[[1]][3,])
cor(train_OBS_l[[1]][1,],train_OBS_l[[1]][3,])


####### DELAYED CORRELATION METHOD


d=3
plot(as.vector(train_OBS_l[[2]]))

xj <- train_OBS_l[[2]][5,]
xk <- train_OBS_l[[2]][10,]

xj


N=length(xj)
xj_s <- xj[d:N]
xk_s <- xk[1:(N-d +1)]

xjkprod <- xj_s*xk_s
rjk_d <- 1/N*(sum(xjkprod))


##Create function that perform cross correlation
r_jk_new <- function(xj,xk,d){
  
  N=length(xj)
  xj_s <- xj[(d+1):N]
  xk_s <- xk[1:(N-(d))]
  
  
  
  abs_corr = abs(cor(xj_s,xk_s))
  
  adj_corr <- abs_corr/sqrt(d+1)
  return(adj_corr)
}

r_jk_new(xj,xk,6)


Strength_maker_new <- function(xk,xj){
  N=length(xj)
  fract = 6
  list_r <- c()
  ##iterate over d from negative to positive
  for (d in 1:floor(N/fract)){
    d=abs(d)
    list_r <- append(list_r,r_jk_new(xk,xj,d))
  }
  for (d in 0:floor(N/fract)){
    list_r <- append(list_r,r_jk_new(xj,xk,d))
  }
  
  #seqplot <- seq(-floor(length(xj)/5),floor(length(xj)/5),1)
  #plot(x=seqplot,y=list_r)
  #print(list_r)
  
  strength_ <- max(list_r)
  return(strength_)
}

Strength_maker_new(xk,xj)



############## exploration of scaling functions 

a1 <- function(x) 1/log(abs(x)+2.73)
a2 <- function(x) 1/sqrt(abs(x)+1)
a3 <- function(x) ((abs(x)+1)^(-1/3))
a4 <- function(x) ((abs(x)+1)^(-1/4))
a5 <- function(x) 2^(-abs(x))
a6 <- function(x) 1/(abs(x)+1)

curve(a1, xlim = c(-30,30),ylim =c(0,1),col=2, main="Red=log, Green=sqrt")
curve(a2, xlim = c(-30,30),ylim =c(0,1), col=3, add=T)
curve(a3, xlim = c(-30,30),ylim =c(0,1), col=4, add=T)
curve(a4, xlim = c(-30,30),ylim =c(0,1), col=5, add=T)
curve(a5, xlim = c(-30,30),ylim =c(0,1), col=6, add=T)
curve(a6, xlim = c(-30,30),ylim =c(0,1), col=7, add=T)

curve(dnorm(x,1),xlim = c(-30,30),ylim =c(0,1), col=7, add=T)

curve(dnorm(0,1),xlim = c(-30,30),ylim =c(0,1), col=7)


x <- seq(-15, 15, by = .01)

y <- dnorm(x, mean = 0, sd = 4)
y <- y*(1/max(y))
plot(x,y)



############# try 
xj <- train_OBS_l[[2]][1,]
xk <- train_OBS_l[[2]][3,]

Strength_maker_new(xk,xj)
abs(cor(xk,xj))

Strength_maker_new(xk,xj) - abs(cor(xk,xj))




New_corr <- function (df){
  nrows <-nrow(df)
  Delayed_corr <- matrix(NA, nrow = nrows, ncol = nrows)
  
  for (i in 1:nrows){
    xj <- df[i,]
    for (j in 1:nrows){
      xk <- df[j,]
      
      if (i<j){
        Delayed_corr[i,j] <- Strength_maker_new(xj,xk)
        
      }
    }
  }
  return(Delayed_corr)
}


#A <- New_corr(train_OBS_l[[OBS]]) ##OBS 2,  with level N/5 analysis
#A


OBS=2
start_time <- Sys.time()
A2 <- New_corr(train_OBS_l[[OBS]])
end_time <- Sys.time()
end_time - start_time 

A2[1:10,1:10]



B <- abs(cor(t(train_OBS_l[[OBS]])))

D <- A - A2
summary(as.numeric(D))
boxplot(as.numeric(D))

heatmap(D, Rowv = NA,Colv = NA)

A[1:10,1:10]
B[1:10,1:10]


Diff_matrix <- A-B
Diff_matrix[1:10,1:10]
summary(as.numeric(Diff_matrix))
hist(Diff_matrix, breaks = 30)

heatmap(Diff_matrix,Rowv = NA,Colv = NA)

#########à New corr flat


New_corr_flat <- function(df){
  nrows <-nrow(df)
  Delayed_corr <- matrix(NA, nrow = nrows, ncol = nrows)
  
  for (i in 1:nrows){
    xj <- df[i,]
    for (j in 1:nrows){
      xk <- df[j,]
      
      if (i<j){
        Delayed_corr[i,j] <- Strength_maker_new(xj,xk)
        
      }
    }
  }
  Flat_delayed <- Delayed_corr[upper.tri(Delayed_corr, diag = F)]
  
  return(Flat_delayed)
}

######## NewCorr flatt fo parallel 


New_corr_flat_parall <- function(df){
  r_jk_new <- function(xj,xk,d){
    
    N=length(xj)
    xj_s <- xj[(d+1):N]
    xk_s <- xk[1:(N-(d))]
    
    
    adj_corr <- 0.0
    abs_corr = abs(cor(xj_s,xk_s))
    
    if (!is.na(abs_corr)){
      adj_corr <- abs_corr/sqrt(d+1)
    }
    return(adj_corr)
  }
  
  
  
  Strength_maker_new <- function(xj,xk){
    N=length(xj)
    fract = 6
    #list_r <- c()
    
    strength2 <- 0.0
    ##iterate over d from negative to positive
    for (d in 1:floor(N/fract)){
      d <- abs(d)
      
      x <- r_jk_new(xk,xj,d)
      if (x > strength2) {
        strength2 <- x
      }
      
      #list_r <- append(list_r,r_jk_new(xk,xj,d))
    }
    for (d in 0:floor(N/fract)){
      
      x <- r_jk_new(xj,xk,d)
      if (x>strength2) {
        strength2 <- x
      }
      #list_r <- append(list_r,r_jk_new(xj,xk,d))
    }
    
    #seqplot <- seq(-floor(length(xj)/5),floor(length(xj)/5),1)
    #plot(x=seqplot,y=list_r)
    #print(list_r)
    
    #strength_ <- max(list_r)
    #return(strength_)
    
    return(strength2)
  }
  
  
  
  
  
  nrows <-nrow(df)
  Delayed_corr <- matrix(NA, nrow = nrows, ncol = nrows)
  
  for (i in 1:nrows){
    xj <- df[i,]
    for (j in 1:nrows){
      xk <- df[j,]
      
      if (i<j){
        Delayed_corr[i,j] <- Strength_maker_new(xj,xk)
        
      }
    }
  }
  Flat_delayed <- Delayed_corr[upper.tri(Delayed_corr, diag = F)]
  print("V")
  return(Flat_delayed)
}


## PARALLELIZE PROCESS!


system.time(
a <- lapply(train_OBS_l[1:10],New_corr_flat)
)


cl<-makeCluster(detectCores(),type="SOCK")
clusterExport(cl, "train_OBS_l")
system.time(
  a2<-parLapply(cl,train_OBS_l[1:10],New_corr_flat_parall)
)
stopCluster(cl)

#MUCH BETTER..
library(parallel)
detectCores()



## DO the whole dataset! (20 mins...)
cl<-makeCluster(4,type="SOCK")
clusterExport(cl, "train_OBS_l")

system.time(
  Delayed_cor_list<-parLapply(cl,train_OBS_l,New_corr_flat_parall)
)
stopCluster(cl)

#### ERRORS GIVEN,,, MISSING TRUE/FLSE VALUE.. 
#-> ERROR IN THE FUNCTION, SOMETIMES CORRELATION IS NA AND NEEDS TO BE HANDLED


##TRY THE DETECTCORE
cl<-makeCluster(detectCores(),type="SOCK")
clusterExport(cl, "train_OBS_l")
system.time(
  Delayed_cor_list<-parLapply(cl,train_OBS_l,New_corr_flat_parall)
)
stopCluster(cl)


length(Delayed_cor_list)
length(Delayed_cor_list[[1]])

Delayed_cor_df <- matrix(NA, length(Delayed_cor_list),length(Delayed_cor_list[[1]]))

for (i in 1:length(Delayed_cor_list)){
  Delayed_cor_df[i,] <- Delayed_cor_list[[i]]
}  

hist(Delayed_cor_df,breaks=100)

Delayed_cor_df <- data.frame(Delayed_cor_df)


Delayed_cor_df
write.csv2(Delayed_cor_df, file = "Delayed_Cor_new.csv")


##################### PERFORM KPCA + SVM CV -> FUNCTION? TO BE CONTINUED

Delayed_df <- data.frame(Delayed_cor_df, age=traindf$age, sex= ((traindf$sex=='male')*1))


################# KPCA -> SVM PIPELINE 

KPCA_SVM_PIPL <- function(df,sigma=0.01, N_feat=50,gam, costo, n=100){
  
  
  dt2<-as.matrix(df)       
  rbf2<-rbfdot(sigma=0.01)   
  km2<-kernelMatrix(rbf2,dt2)
  kpc2 <- kpca(km2,data=df,kernel="rbfdot",kpar=list(sigma=0.2),features=N_feat)
  
  # CREATE DATAFRAMES FROM KERNEL COMPONENTS
  kern_comp_test <- pcv(kpc2)
  kern_comp_test2 <- data.frame(kern_comp_test)
  train_tot <- data.frame(kern_comp_test2, y=as.factor(traindf$y))
  
  ##ACCURACY 
  acc_list <- c()
  ##define n 
  N=50 #N. of test 
  for (i in 1:n){
    
    if(i/n==0.5) print('halfway')
    
    #randomply split data
    idx <- sample(nrow(train_tot),N,replace = FALSE)
    kern_train_tr <- train_tot[-idx,]
    kern_train_ts <-train_tot[idx,]
    
    #train
    svmfit_kern_f = svm(y ~ ., data = kern_train_tr, kernel = "radial", gamma=gam, cost = costo, scale = TRUE)
    
    #predict
    predf_train <- predict(svmfit_kern_f,kern_train_ts[-(N_feat+1)])
    #save accuracy
    acc_list <- append(acc_list,accuracy(kern_train_ts$y,predf_train))
  }
  
  return(acc_list)
}


acc <- KPCA_SVM_PIPL(Delayed_df,sigma = 10,N_feat = 50,gam=0.0001, costo=100, n=200)
hist(acc, breaks=20)
mean(acc)
sd(acc)

summary(acc)







Delayed_df2 <- data.frame(Delayed_df, y=as.factor(traindf$y))

dt2<-as.matrix(Delayed_df)       
rbf2<-rbfdot(sigma=0.01)   
km2<-kernelMatrix(rbf2,dt2)
kpc2 <- kpca(km2,data=df,kernel="rbfdot",kpar=list(sigma=0.2),features=50)

# CREATE DATAFRAMES FROM KERNEL COMPONENTS
kern_comp_test <- pcv(kpc2)
kern_comp_test2 <- data.frame(kern_comp_test)
train_tot <- data.frame(kern_comp_test2, y=as.factor(traindf$y))





ksvm_01a <- ksvm(y ~ ., data = train_tot,
                type   = "C-svc", 
                kernel = "rbfdot", 
                C     = 100,
                kpar  = list(sigma = 0.001),
                cross=10)
ksvm_01a

plot(ksvm_01a,data=train_tot)

