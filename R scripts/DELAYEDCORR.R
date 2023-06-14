library(Metrics)
library(e1071)
library(data.table)
library(kernlab)
library(ggplot2)
library(caret)
#install.packages('klaR')
library(klaR)
library(igraph)



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

xj <- train_OBS_l[[2]][3,]
xk <- train_OBS_l[[2]][10,]

xj


N=length(xj)
xj_s <- xj[d:N]
xk_s <- xk[1:(N-d +1)]

xjkprod <- xj_s*xk_s
rjk_d <- 1/N*(sum(xjkprod))


##Create function that perform cross correlation
r_jk <- function(xj,xk,d){
  d=abs(d)
  
  N=length(xj)
  xj_s <- xj[d:N]
  xk_s <- xk[1:(N-d +1)]
  #print(length(xj_s))
  #print(cor(xj_s,xk_s))
  #plot(xj_s,xk_s)
  
  xjkprod <- xj_s*xk_s
  rjk_d <- (1/N)*(sum(xjkprod))
  
  return(abs(rjk_d))
}

r_jk(xj,xk,9)

#vectorize? no
vr_jk <- Vectorize(r_jk)
vr_jk(xj,xk,1:20)


List_maker <- function(xk,xj){
  N=length(xj)
  list_r <- c()
  for (d in 1:floor(N/4)){
    list_r <- append(list_r,r_jk(xj,xk,d))
  }
  return(list_r)
}

list_r <- List_maker(xj,xk)


seqplot <- seq(1,floor(length(xj)/4),1)
plot(x=seqplot,y=list_r)

idxmax <- which(list_r == max(list_r))
idxmax

r_jk(xj,xk,idxmax)


Strength_maker <- function(xj,xk){
  list_rjk <- List_maker(xj,xk)
  list_rkj <- List_maker(xk,xj)
  
  idxmaxjk <- which(list_rjk == max(list_rjk))
  idxmaxkj <- which(list_rkj == max(list_rkj))
  
  Strength <- 1/(min(idxmaxjk,idxmaxkj))
  return(Strength)
}


Strength_maker(xj,xk)


########## TRY BUILD ENTIRE MATRIX OF DELAYEDCORR------------------------
nrows <-nrow(train_OBS_l[[1]])
Delayed_corr <- matrix(NA, nrow = nrows, ncol = nrows)

for (i in 1:nrows){
  print(i)
  for (j in 1:nrows){
    
    xj <- train_OBS_l[[1]][i,]
    xk <- train_OBS_l[[1]][j,]
    Delayed_corr[i,j] <- Strength_maker(xj,xk)
  }
}

Delayed_corr

hist(Delayed_corr)
cor_mat_i[upper.tri(cor_mat_i, diag = F)]

Delayed_corr_tri <- Delayed_corr[upper.tri(Delayed_corr,diag=F)]

##GRAPH 
n=115
c <- Delayed_corr[1:n,1:n]
diag(c) <- 0

c
qx=0.75
c[c<quantile(c, qx)] <- 0
c[c>=quantile(c,qx)] <- 1
hist(c)
g<- graph_from_adjacency_matrix(c, mode="undirected", weighted = T)
par(mfrow=c(1,2))
plot(g,edge.width = E(g)$weight)
#tkplot(g,edge.width = E(g)$weight)
plot(colMeans(c), lwd=3)
grid(n, NA, lwd = 2)


###MEASURES IN GRAPH THEORY
average.path.length(g)
global_efficiency(g)
average_local_efficiency(g)
local_efficiency(g)

plot(local_efficiency(g))

plot(local_efficiency(g), colMeans(c))
cor(local_efficiency(g), colMeans(c))



## ----------------------------------
###################delayed correlation 

Create_Delayed_corr <- function (df){
  nrows <-nrow(df)
  Delayed_corr <- matrix(NA, nrow = nrows, ncol = nrows)
  
  for (i in 1:nrows){
    xj <- df[i,]
    for (j in 1:nrows){
      xk <- df[j,]
      
      if (i<j){
        Delayed_corr[i,j] <- Strength_maker(xj,xk)
      }
    }
  }
  return(Delayed_corr)
}

Create_Delayed_flat_corr <- function (df){
  nrows <-nrow(df)
  Delayed_corr <- matrix(NA, nrow = nrows, ncol = nrows)
  
  for (i in 1:nrows){
    xj <- df[i,]
    for (j in 1:nrows){
      xk <- df[j,]
      
      if (i<j){
        Delayed_corr[i,j] <- Strength_maker(xj,xk)
      }
    }
  }
  Flat_delayed <- Delayed_corr[upper.tri(Delayed_corr, diag = F)]
  
  return(Flat_delayed)
}



start_time <- Sys.time()
c <- Create_Delayed_corr(train_OBS_l[[3]])
end_time <- Sys.time()

end_time - start_time 

start_time <- Sys.time()
c <- Create_Delayed_flat_corr(train_OBS_l[[3]])
end_time <- Sys.time()

end_time - start_time 

start_time <- Sys.time()
end_time <- Sys.time()

end_time - start_time 



c[is.na(c)] <- 0 
c

qx=0.75
c[c<quantile(c, qx)] <- 0
c[c>=quantile(c,qx)] <- 1
hist(c)
g<- graph_from_adjacency_matrix(c, mode="undirected", weighted = T)
par(mfrow=c(1,2))
plot(g,edge.width = E(g)$weight)
#tkplot(g,edge.width = E(g)$weight)
plot(local_efficiency(g))
grid(n, 10, lwd = 1)



##############------ CONNECTOME DELAYED CORR BINARY 

Create_Connect_delay <- function(df_list, nroi1){
  OBS=length(df_list)
  #Empty matrix to fill, dimension reduced
  Connect_diag_mat1 <- matrix(NA,nrow = OBS, ncol = nroi1*(nroi1-1)/2)
  
  for ( i in 1:OBS){
    print(i)
    #start_time <- Sys.time()
    Connect_diag_mat1[i,] <- Create_Delayed_flat_corr(train_OBS_l[[i]])
    #end_time <- Sys.time()
    
    #print(end_time - start_time) 
  }
  
  return(Connect_diag_mat1)
}


Connect_delay_df <- Create_Connect_delay(train_OBS_l,nroi)

write.csv2(Connect_delay_df,file = 'Train_Connectome_delay.csv')



heatmap(Connect_delay_df[,1:200], Rowv = NA, Colv = NA)
hist(Connect_delay_df[,1])
hist(Connect_delay_df[,2])


heatmap(cor(Connect_delay_df[,1:200]))


Connect_delay_df2 <-data.frame(Connect_delay_df,age=traindf$age,sex=traindf$sex, y=as.factor(traindf$y))
write.csv2(Connect_delay_df2,file = 'Train_Connectome_delay.csv')



svmfit_delay = svm(y ~ ., data = Connect_delay_df2, kernel = "radial", gamma=0.01, cost = 100, scale = TRUE,cross=10)
summary(svmfit_delay)



##########kpca -> svm

dt<-as.matrix(Connect_delay_df2[,-6672])  
rbf<-rbfdot(sigma=0.01)   
km<-kernelMatrix(rbf,dt)
kpc <- kpca(km,data=Connect_delay_df2[,-6672],kernel="rbfdot",kpar=list(sigma=0.01),features=50)

#KERNEL COMPONENTS
kern_comp <- pcv(kpc)
kern_comp2 <- data.frame(kern_comp, y=as.factor(traindf$y))

plot(kern_comp2$X1,kern_comp2$X2,col=kern_comp2$y)

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
  svmfit_del2 = svm(y ~ ., data = kern_train_tr, kernel = "radial", gamma=0.001, cost = 100, scale = TRUE)
  
  #predict
  pred4_train <- predict(svmfit_del2,kern_train_ts[-51])
  #save accuracy
  acc_list <- append(acc_list,accuracy(kern_train_ts$y,pred4_train))
}

boxplot(acc_list)
summary(acc_list)


