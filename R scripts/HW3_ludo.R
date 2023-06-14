
library(Metrics)
library(e1071)
library(data.table)
library(kernlab)


'''
library(ggplot2) # ggplot() for plotting
library('dplyr') # data reformatting
library(dplyr) # data reformatting
library(tidyr) # data reformatting
install.packages("xlsx")
library("xlsx")
'''


esa_df <-read.csv('esa_scheme_only.csv')

traindf <- fread('statistical-learning-hw03-2022/train_hw03.csv') 

#### EXPLORE THE FEATURES. IN WHAT ORDER SHOULD WE LOOT AT IT?------------------

# ROI 116, time slots 115
nroi=116
ntime=115


#CREATE df of the first info, id-age-sex-y
df_info <- traindf[,1:4]

# CREATE a list of Df, each one containing a single ROI, with all time slots
train_ROI_l = list()
for (i in 1:nroi){
  dfs <- list(traindf[,(5+(i-1)*ntime):(5+(i-1)*ntime + (ntime-1))])
  train_ROI_l <- append(train_ROI_l,dfs)
}
remove(dfs)

## list of 116 df: dataframes packed by Roi, all time 
train_ROI_l[[116]]

## plot of: 5 df ( so 5 ROI), ROW 2 ( observation patient 2). Timeline plot 
plot(as.numeric(train_ROI_l[[5]][2,]))


## heatmap of the first df:
df_ROI1_no_aut <- as.matrix(train_ROI_l[[1]][-idx_autistic,])
df_ROI1_aut <- as.matrix(train_ROI_l[[1]][idx_autistic,])

par(mfrow = c(2, 1))
heatmap(df_ROI1_aut, Rowv = NA, Colv = NA)
heatmap(df_ROI1_no_aut, Rowv = NA, Colv = NA)

idx_autistic <- which(df_info$y=='autism')

#write.csv2(df_ROI1, file='df_ROI1.csv')
#write.csv2(df_info,file="df_info.csv")     

# CREATE a list of Df, each one containing the same time slot, with all ROI
train_time_l = list()
for (i in 1:ntime){
  seq_time_i <- seq(5+(i-1),13230+(i-1),ntime)
  dfs <- list(traindf[,seq_time_i])
  train_time_l <- append(train_time_l,dfs)
}
remove(dfs)

## list of 115 df: dataframes packed by time, all roi 
train_time_l[[115]]


## plot of: 1 df ( so 1 time), ROW 3 ( observation patient 3). roi plot 
par(mfrow = c(1, 1))
length(as.numeric(train_time_l[[1]][3,]))
plot(as.numeric(train_time_l[[1]][3,]))


#plot of time 1 of all rois and all the patients
df_time1 <- as.matrix(train_time_l[[1]])
heatmap(df_time1, Rowv = NA, Colv = NA)


####-------------summarize the dataframes -------------------------------------
par(mfrow = c(1, 1))
heatmap(as.matrix(train_ROI_l[[3]][1:25,]),Rowv = NA,Colv = NA)

heatmap(t(as.matrix(train_ROI_l[[1]][,])),Rowv = NA,Colv = NA)



##testing
a<-as.matrix(train_ROI_l[[1]])

min(a)
as.numeric(quantile(a,0.25))
median(a)
mean(a)
as.numeric(quantile(a,0.75))
max(a)




##deletes outliers
a<-as.matrix(train_ROI_l[[1]])
Summary<-boxplot(t(a))$stats
Summary
t_summary <- t(Summary)
t_summary
length(t(a)[1,])

i=2
plot(a[i,])
mean(a[i,])
sd(a[i,])

##function 
Create_summary_info <- function(df){
  a<-as.matrix(df)
  Summary<-boxplot(t(a))$stats
  #Summary
  t_summary <- t(Summary)
  #t_summary
  sd_l <- c()
  mean_l <- c()
  for (i in 1:nrow(df)){
    sd_l <- append(sd_l,sd(a[i,]))
    mean_l <- append(mean_l,mean(a[i,]))
    
  }
  
  result <- matrix(NA,nrow = nrow(df),ncol = ncol(t_summary)+2)
  result[,1:5] <- t_summary
  result[,6] <-mean_l
  result[,7] <-sd_l
  colnames(result) <- c('min','1Q','median','3Q','max','mean','sd')
  return(round(result[,c(3,6,7)],4))
}

s1 <- Create_summary_info(train_ROI_l[[1]])
cor(s1)
heatmap(cor(s1))
ncolumns <-ncol(s1)

Components_df <- matrix(NA,nrow = nrow(train_ROI_l[[1]]), ncol = ncolumns*nroi)
train_ROI_l[[116]]
k=0
for (i in 1:nroi){
  print(i)
  s <- Create_summary_info(train_ROI_l[[i]])
  Components_df[,(1+k):(ncolumns+k)]<- s
  k=k+ncolumns
}
#### create new dataset with summary values (eliminating time effect) of the different rois
#Components_df

#### correlation exploration
heatmap(cor(Components_df))

median_seq <- seq(1,346,3)
mean_seq <- seq(2,347,3)
sd_seq <- seq(3,348,3)
heatmap(cor(Components_df[,median_seq]),Rowv = NA,Colv = NA)
heatmap(cor(Components_df[,mean_seq]),Rowv = NA,Colv = NA)
heatmap(cor(Components_df[,sd_seq]),Rowv = NA,Colv = NA)

mean(cor(Components_df[,median_seq]))
hist(cor(Components_df[,median_seq]),breaks=100)
hist(cor(Components_df[,mean_seq]),breaks=100)
hist(cor(Components_df[,sd_seq]),breaks=100)



#1. in we want all the summary components 
Components_df <- data.frame(Components_df)
Components_df_train <- data.frame(scale(Components_df),traindf$sex,y=as.factor(traindf$y))


#2. If we only want mean and median since standard deviation is highly correlated 
#meanandmedian_seq <- c(median_seq,mean_seq, 3)
#Components_df_train <- data.frame(scale(Components_df[,meanandmedian_seq],traindf$age),traindf$sex,y=as.factor(traindf$y))


plot(Components_df_train[,1:2])
plot(Components_df_train$X1,Components_df_train$X2,col=Components_df_train$y, xlim=c(-0.05,0.15), ylim=c(-0.15,0.1), lwd=2)



svmfit = svm(y ~ ., data = Components_df_train, kernel = "linear",degree = 5, gamma=0.1, cost = 100, scale = TRUE,cross=15)
#summary(svmfit)
svmfit$tot.accuracy
table(svmfit$fitted)

###-----------------------------------try kpca??
dt<-as.matrix(Components_df)       
rbf<-rbfdot(sigma=0.0001)   
km<-kernelMatrix(rbf,dt)
kpc <- kpca(km,data=Components_df,kernel="rbfdot",kpar=list(sigma=0.2),features=200)

kern_comp <- pcv(kpc)


kern_comp2 <- data.frame(kern_comp, y=as.factor(traindf$y))


plot(kern_comp2[,1:5])
plot(kern_comp2$X1,kern_comp2$X5,col= kern_comp2$y,lwd=2)


svmfit = svm(y ~ ., data = kern_comp2, kernel = "radial",degree = 4, gamma=0.001, cost = 10, scale = TRUE,cross=10)
svmfit$tot.accuracy
table(svmfit$fitted)

###à try simple svm with only age and sex

traindf_simple <- traindf[,2:4]
traindf_simple$y <- as.factor(traindf_simple$y)
table(traindf_simple[,2:3])

plot(sex, age,data=traindf_simple)

hist(traindf_simple$sex)
table(traindf_simple$sex)
sort(traindf_simple$sex)

svmfit_simple = svm(y ~ ., data = traindf_simple, kernel = "linear",,degree = 4, gamma=0.001, cost = 10, scale = TRUE,cross=10)
svmfit_simple$tot.accuracy





######## ----------------try distance between people?--------------------------


b<-sqrt(sum((as.numeric(traindf[1,5:13344] - traindf[2,5:13344]))^2))

Distance_func <- function(i,j){
  b<-sqrt(sum((as.numeric(traindf[i,5:13344] - traindf[j,5:13344]))^2))
  return(b)
}
Distance_func(1,2)
install.packages('lsa')
library(lsa)
cosine(as.numeric(traindf[1,5:13344]),as.numeric(traindf[2,5:13344]))

N=100
Dist_mat <- matrix(NA,N,N)
for (i in 1:N){
  for (j in 1:N){
    num = cosine(as.numeric(traindf[i,5:13344]),as.numeric(traindf[j,5:13344]))
    Dist_mat[i,j] <- num
  }
}
heatmap(Dist_mat)
mean(Dist_mat)


## separate dataset in single person dataframe/matrix..
nroi
df_obs <- matrix(NA,nrow = nroi, ncol = ntime)
for (i in 1:nroi){
  df_obs[i,] <- as.numeric(traindf[1,(5+(i-1)*ntime):(5+(ntime-1) +(i-1)*ntime)])
}

heatmap(df_obs,Rowv = NA,Colv = NA)

train_OBS_l = list()
for (i in 1:nrow(traindf)){
  print(i)
  df_obs <- matrix(NA,nrow = nroi, ncol = ntime)
  for (j in 1:nroi){
    df_obs[j,] <- as.numeric(traindf[i,(5+(j-1)*ntime):(5+(ntime-1) +(j-1)*ntime)])
  }
  
  train_OBS_l <- append(train_OBS_l,list(df_obs))
}
remove(dfs)
remove(df_obs)
##explore the visual representation of a singular person
htraindf$y[1:15]
OBS=4
heatmap(train_OBS_l[[OBS]],Rowv = NA,Colv = NA,scale="none", main=paste("OBS num",OBS,traindf$y[OBS],traindf$age[OBS],traindf$sex[OBS]))
heatmap(cor(t(train_OBS_l[[OBS]])))
plot(train_OBS_l[[OBS]][1,],train_OBS_l[[OBS]][4,])


boxplot(train_OBS_l[[OBS]])
boxplot(t(train_OBS_l[[OBS]]))

mean(train_OBS_l[[OBS]])
sd(train_OBS_l[[OBS]])

##observe some row (specific roi varying time) lines 
plot(train_OBS_l[[OBS]][1,],ylim=c(min(train_OBS_l[[OBS]]),max(train_OBS_l[[OBS]])))
lines(train_OBS_l[[OBS]][1,])
lines(train_OBS_l[[OBS]][2,],col="red")
lines(train_OBS_l[[OBS]][3,],col="blue")
lines(train_OBS_l[[OBS]][4,],col="orange")
lines(train_OBS_l[[OBS]][108,],col="pink")
mean(train_OBS_l[[OBS]][108,])
sd(train_OBS_l[[OBS]][108,])
mean(train_OBS_l[[OBS]][1,])
sd(train_OBS_l[[OBS]][1,])


##observe some row (specific time varying roi) lines
plot(train_OBS_l[[OBS]][,1],ylim=c(min(train_OBS_l[[OBS]]),max(train_OBS_l[[OBS]])))
lines(train_OBS_l[[OBS]][,1])
lines(train_OBS_l[[OBS]][,2],col="red")
lines(train_OBS_l[[OBS]][,3],col="blue")
lines(train_OBS_l[[OBS]][,4],col="orange")
lines(train_OBS_l[[OBS]][,33],col="pink")



######combined dataframe of mean and sd over time and over roi -------------------
heatmap(as.matrix(train_ROI_l[[1]]),Colv = NA,Rowv = NA)

Create_mean_sd_info <- function(df){
  a<-as.matrix(df)
  sd_l <- c()
  mean_l <- c()
  for (i in 1:nrow(df)){
    sd_l <- append(sd_l,sd(a[i,]))
    mean_l <- append(mean_l,mean(a[i,]))
  }
  result <- matrix(NA,nrow = nrow(df),ncol = 2)
  result[,1] <-mean_l
  result[,2] <-sd_l
  return(round(result,4))
}





Mean_sd_df <- matrix(NA,nrow = nrow(traindf),ncol = 2*nroi+2*ntime)
k=0
for(i in 1:nroi){
  print(k)
  s <- Create_mean_sd_info(train_ROI_l[[i]])
  Mean_sd_df[,(1+k):(2+k)]<- s
  k=k+2
}
for(i in 1:ntime){
  print(k)
  s <- Create_mean_sd_info(train_time_l[[i]])
  Mean_sd_df[,(1+k):(2+k)]<- s
  k=k+2
}

#heatmap(cor(Mean_sd_df))
hist(cor(Mean_sd_df))

Mean_sd_df2 <- data.frame(scale(Mean_sd_df), y=as.factor(traindf$y))

plot(Mean_sd_df[,13],Mean_sd_df[,10],col=Mean_sd_df2$y)


svmfit = svm(y ~ ., data =Mean_sd_df2, kernel = "linear", gamma=1, cost = 10000, scale = TRUE,cross=15)
summary(svmfit)
table(svmfit$fitted)



library(caret)
Optm <- tune.svm(x = Mean_sd_df2[,-463], y = Mean_sd_df2[, 463], 
                 type = "C-classification", 
                 kernel = c("linear"), cost = c(100,1000,10000,100000), 
                 gamma = c(1,0.1,0.01,0.001))
summary(Optm)



##### CONNECTOME! CORRELATION MATRIX OF SINGLE OBS?-----------------------------
OBS=4
heatmap(train_OBS_l[[OBS]],Rowv = NA,Colv = NA,scale="none", main=paste("OBS num",OBS,traindf$y[OBS],traindf$age[OBS],traindf$sex[OBS]))
heatmap(cor(t(train_OBS_l[[OBS]])))

cor(t(train_OBS_l[[OBS]]))
cor(train_OBS_l[[OBS]][1,],train_OBS_l[[OBS]][3,])
plot(train_OBS_l[[OBS]][1,],train_OBS_l[[OBS]][3,])

## ways to visualize this graph?

#install.packages("igraph")
library(igraph)
cor_mat1 <- cor(t(train_OBS_l[[OBS]]))

cor_mat1[cor_mat1 == 1] <- 0
cor_mat1[cor_mat1 == -1] <- 0

hist(cor_mat1)
heatmap(cor_mat1,Rowv = NA,Colv = NA)
sort(cor_mat1)[1]
sort(cor_mat1)[length(sort(cor_mat1))]

cor_mat1[21:30,21:30]
g<- graph_from_adjacency_matrix(cor_mat1[1:30,1:30], mode="undirected", weighted = TRUE)
E(g)$width <- E(g)$weight/min(E(g)$weight) + 0.1 # offset=1
plot(g)


cor_mat1 <- cor(t(train_OBS_l[[OBS]]))
cor_mat1[cor_mat1 == 1] <- NA
cor_mat1[cor_mat1 == -1] <- NA
cor_arr <- as.vector(cor_mat1)
cor_arr <- cor_arr[!is.na(cor_arr)]

##### ALGO !!
# scan all the observation
# make the correlation matrix
# flatten in array
# save it in matrix row 


Connections_mat <- matrix(NA,nrow = nrow(traindf), ncol = 13340)


for ( i in 1:nrow(traindf)){
  #1. scan all observation
  #2. make corr matrix
  cor_mat_i <- cor(t(train_OBS_l[[i]]))
  print(sd(cor_mat_i))
  #print(cor_mat_i[1:20,1:20])
  #2.1 remove the diagonal of 1s
  cor_mat_i[cor_mat_i == 1] <- NA
  #3. flatten in array
  cor_arr_i <- as.vector(cor_mat_i)
  remove(cor_mat_i)
  cor_arr_i <- cor_arr_i[!is.na(cor_arr_i)]
  
  #4. save it in new matrix
  Connections_mat[i,] <- cor_arr_i
  
  print(i)
}


### check if all observation have only 116 (diagonal) correlation ones 
OBS=24
cor(t(train_OBS_l[[OBS]]))
train_OBS_l[[OBS]][107,]


traindf[24,]


heatmap(cor(t(train_OBS_l[[OBS]])))
hist(cor(t(train_OBS_l[[OBS]])))
length(which(cor(t(train_OBS_l[[OBS]])) ==1))

mean(is.na(cor(t(train_OBS_l[[OBS]]))))

l <- c()
nas <- c()
for (OBS in 1:nrow(traindf)){
  nas <- append(nas, mean(is.na(cor(t(train_OBS_l[[OBS]])))))
  l <- append(l,length(which(cor(t(train_OBS_l[[OBS]])) ==1)))
}
mean(nas)
mean(l)