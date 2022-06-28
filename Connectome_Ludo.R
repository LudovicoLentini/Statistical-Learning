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
remove(dfs)
remove(df_obs)

## Now we have every observation in order in a df list
train_OBS_l[[1]] 

#### VISUAL EXPLORATION ---------------------------------------------------------
##explore the visual representation of a singular Observation
##chose the numb of obs
OBS=1
heatmap(train_OBS_l[[OBS]],Rowv = NA,Colv = NA,scale="none",
        main=paste("OBS num",OBS,traindf$y[OBS],traindf$age[OBS],traindf$sex[OBS]))

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


##observe some col (specific time varying roi) lines
plot(train_OBS_l[[OBS]][,1],ylim=c(min(train_OBS_l[[OBS]]),max(train_OBS_l[[OBS]])))
lines(train_OBS_l[[OBS]][,1])
lines(train_OBS_l[[OBS]][,2],col="red")
lines(train_OBS_l[[OBS]][,3],col="blue")
lines(train_OBS_l[[OBS]][,4],col="orange")
lines(train_OBS_l[[OBS]][,33],col="pink")



##### CREATE CONNECTOME! IS IT CORRELATION MATRIX OF SINGLE OBS?---------------------

##first tryout with obs 1
OBS=1
## correlation matrix is a column based function, so it must be transposed first
# if we want correlation between the Rows (ROI)
cor_mat1 <- cor(t(train_OBS_l[[OBS]]))


#visualiz
heatmap(cor_mat1,Rowv = NA,Colv = NA,scale="none",
        main=paste('Connectome of OBS n:',OBS,', ',traindf$y[OBS],traindf$age[OBS],traindf$sex[OBS]))


## can we visualize it also with graphs?
#install.packages("igraph")
library(igraph)

#for graph visualization, i dont want self-connection
cor_mat1_graph <- cor_mat1
cor_mat1_graph[cor_mat1_graph == 1] <- 0
cor_mat1_graph[cor_mat1_graph == -1] <- 0

n=40
c <- cor_mat1_graph[1:n,1:n]*cor_mat1_graph[1:n,1:n]
#c <- abs(cor_mat1_graph[1:n,1:n])
c <- c*10
hist(c,breaks=50)
median(c)
quantile(c, 0.90)
c[c<quantile(c, 0.75)] <- 0
g<- graph_from_adjacency_matrix(c, mode="undirected", weighted = T )
#E(g)$weight <- edge.betweenness(g)
plot(g,edge.width = E(g)$weight)
tkplot(g,edge.width = E(g)$weight)

#E(g)$weight <- E(g)$weight*10
#E(g)$weight
#E(g)$weight <- edge.betweenness(g)
#E(g)$weight
#E(g)$width <- (E(g)$weight) + min(E(g)$weight) + 0.5 # offset=1

tk_coords(g)


### move on, not useful but cool



# The Connectome matrix must be "flattened" so can be used in a dataframe

#The self connection of the diagonal is going to be all 1's for every observation,
#so it is useless in a dataframe

#first, lets substitute the 1 diagonal correlation with NAs, more fast to delete
cor_mat1[cor_mat1 == 1] <- NA

# Matrix as vector
cor_arr <- as.vector(cor_mat1)
# Delete the NAs
cor_arr <- cor_arr[!is.na(cor_arr)]

#
cor_arr
# corr_arr is an array of (116x116 - 116) representing the "connectivity" between 
# every ROI, excluded the connectivity with self.




############### CONNECTOME DATASET --------------------------------------------- 
#Do this "Connectome" flattened matrix for every OBS in the dataset and create a new dataset

##### ALGORITM

#1. scan all the observation
#2. make the correlation matrix
#3. flatten in array
#4. save it in matrix row of new dataset

#Empty matrix to fill
Connections_mat <- matrix(NA,nrow = nrow(traindf), ncol = 13340)

for ( i in 1:nrow(traindf)){
  #1. scan all observation
  #2. make corr matrix
  cor_mat_i <- cor(t(train_OBS_l[[i]]))
  #print(sd(cor_mat_i))
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

#Error qualche corr non viene calcolata e si impalla tuttooooo

#explore where is the error
l <- c()
nas <- c()

for (OBS in 1:nrow(traindf)){
  nas <- append(nas, mean(is.na(cor(t(train_OBS_l[[OBS]])))))
  l <- append(l,length(which(cor(t(train_OBS_l[[OBS]])) ==1)))
}

mean(l)
mean(nas)  
# Un po' di osservazioni hanno dei problemi di NAS.. cioè ci sono delle variabili
#che sono "costanti" al suo interno e quindi non calcola la correlazione 
### check if all observation have only 116 (diagonal) correlation ones 
OBS=24
cor(t(train_OBS_l[[OBS]]))
#plot heatmap of obs 24
heatmap(train_OBS_l[[OBS]],Rowv = NA,Colv = NA,scale="none",
        main=paste("OBS num",OBS,traindf$y[OBS],traindf$age[OBS],traindf$sex[OBS]))

heatmap(train_OBS_l[[OBS]][100:115,],Rowv = NA,Colv = NA,scale="none",
        main=paste("OBS num",OBS,traindf$y[OBS],traindf$age[OBS],traindf$sex[OBS]))

#102, 107 are NA
train_OBS_l[[OBS]][107,]

#is it in the original dataset too?
i=24
j=107
traindf[i,(5+(j-1)*ntime):(5+(ntime-1) +(j-1)*ntime)]
plot(as.numeric(traindf[i,(5+(j-1)*ntime):(5+(ntime-1) +(j-1)*ntime)]))
plot(as.numeric(traindf[OBS,10000:13000]))
# yes...

train_OBS_l[[OBS]][102,]
i=24
j=107
mean(as.numeric(traindf[i,(5+(j-1)*ntime):(5+(ntime-1) +(j-1)*ntime)]))
plot(as.numeric(traindf[i,(5+(j-1)*ntime):(5+(ntime-1) +(j-1)*ntime)]), ylim=c(-10,10))
#plot(as.numeric(traindf[i,(5+(j-1)*ntime):(5+(j-1)*ntime+300)]))



## what if we insert a "fictional" small value replacing a zero?
a<- as.numeric(traindf[i,(5+(107-1)*ntime):(5+(ntime-1) +(107-1)*ntime)])
b <- as.numeric(traindf[i,(5+(105-1)*ntime):(5+(ntime-1) +(105-1)*ntime)])

plot(a,b)
cor(a,b)

#substitute a zero
a_ <- a
a_[23] <-0.001

plot(a_,b)
cor(a_,b, use = "complete.obs")

c_l <- c()
#does it matter where i substitute?
for (i in 1:length(a)){
  a_ <- a
  a_[i] <-0.001
  c_l <- append(c_l,cor(a_,b))
}
plot(c_l[1:25])
c_l[1:25]

c_l[23]
b[23]


################better way is to substitute the NAs for no corr with zeros
#tryout
OBS=24
cor24 <- cor(t(train_OBS_l[[OBS]]))
cor24[is.na(cor24)] <- 0
cor24



##### ALGORITM

#1. scan all the observation
#2. make the correlation matrix
#3. flatten in array
#4. save it in matrix row of new dataset

#Empty matrix to fill
Connections_mat <- matrix(NA,nrow = nrow(traindf), ncol = 13340)

for ( i in 1:nrow(traindf)){
  #1. scan all observation
  #2. make corr matrix
  cor_mat_i <- cor(t(train_OBS_l[[i]]))
  #print(sd(cor_mat_i))
  #print(cor_mat_i[1:20,1:20])
  
  ## 2.1  HANDLE THE NAS!
  
  cor_mat_i[is.na(cor_mat_i)] <- 0

  #2.2 remove the diagonal of 1s
  cor_mat_i[cor_mat_i == 1] <- NA
  
  #3. flatten in array
  cor_arr_i <- as.vector(cor_mat_i)
  remove(cor_mat_i)
  cor_arr_i <- cor_arr_i[!is.na(cor_arr_i)]
  
  #4. save it in new matrix
  Connections_mat[i,] <- cor_arr_i
  
  print(i)
}

# there are no NAS!
mean(is.na(Connections_mat))


hist(Connections_mat, breaks=100)



##------------ USE the Cnnectome (Connections_mat) to build a pred model-------
Connect <- data.frame(scale(Connections_mat), y=as.factor(traindf$y))


svmfitcon = svm(y ~ ., data =Connect, kernel = "linear", gamma=1, cost = 10000, scale = TRUE,cross=10)
summary(svmfitcon)
table(svmfitcon$fitted)

#Logistic classif
#glmfitcon <- glm(y ~ ., data = Connect, family = "binomial")
#summary(glmfitcon)


##-----------COnnectome with positive numbers

#Empty matrix to fill
Connections_pos_mat <- matrix(NA,nrow = nrow(traindf), ncol = 13340)

for ( i in 1:nrow(traindf)){
  #1. scan all observation
  #2. make corr matrix
  cor_mat_i <- cor(t(train_OBS_l[[i]]))
  #print(sd(cor_mat_i))
  #print(cor_mat_i[1:20,1:20])
  
  ## 2.1  HANDLE THE NAS!
  cor_mat_i[is.na(cor_mat_i)] <- 0
  
  ## 2.1.2 square the matrix
  cor_mat_i <- abs(cor_mat_i)
  
  #2.2 remove the diagonal of 1s
  cor_mat_i[cor_mat_i == 1] <- NA
  
  #3. flatten in array
  cor_arr_i <- as.vector(cor_mat_i)
  remove(cor_mat_i)
  cor_arr_i <- cor_arr_i[!is.na(cor_arr_i)]
  
  #4. save it in new matrix
  Connections_pos_mat[i,] <- cor_arr_i
  
  print(i)
}

# there are no NAS!
mean(is.na(Connections_pos_mat))


hist(Connections_pos_mat, breaks=100)
summary(as.vector(Connections_pos_mat))

###--- use the Connect_pos
Connect_pos  <- data.frame(scale(Connections_pos_mat), y=as.factor(traindf$y))


svmfitcon_p = svm(y ~ ., data =Connect_pos, kernel = "linear", gamma=1, cost = 1000, scale = TRUE,cross=10)
summary(svmfitcon_p)
table(svmfitcon_p$fitted)



Connect_pos_pca <- prcomp(Connect_pos[,-13341], center = TRUE,scale. = TRUE)
plot(Connect_pos_pca$sdev)
sum(Connect_pos_pca$sdev[1:300])/sum(Connect_pos_pca$sdev)

Connect_pos_pca$x
summary(Connect_pos_pca)
C

Pca_Connect <- Connect_pos_pca$x[,1:300]
Pca_Connect2 <- data.frame(Pca_Connect, y=as.factor(traindf$y))

plot(Connect_pos_pca$x[,2],Connect_pos_pca$x[,3], col=as.factor(traindf$y))

svmfitcon_p = svm(y ~ ., data =Pca_Connect2, kernel = "radial", gamma=0.01, cost = 1000, scale = TRUE,cross=10)
summary(svmfitcon_p)
table(svmfitcon_p$fitted)


### ---eliminate the "symmetric" columns of the correlation matrix!-------------
# take only a diagonal part of the matrix


cor_mat_i <- cor(t(train_OBS_l[[1]]))

## 2.1  HANDLE THE NAS!
cor_mat_i[is.na(cor_mat_i)] <- 0

#3. take only upper triangular matrix without the diagonal, and flatten 
corr_arr_i <- cor_mat_i[upper.tri(cor_mat_i, diag = F)]
remove(cor_mat_i)


### ---eliminate the "symmetric" columns of the correlation matrix!-------------
# take only a diagonal part of the matrix

##### ALGORITM

#1. scan all the observation
#2. make the correlation matrix
#3. flatten in array
#4. save it in matrix row of new dataset

#Empty matrix to fill, dimension reduced
Connect_diag_mat <- matrix(NA,nrow = nrow(traindf), ncol = nroi*(nroi-1)/2)

for ( i in 1:nrow(traindf)){
  #1. scan all observation
  #2. make corr matrix
  cor_mat_i <- cor(t(train_OBS_l[[i]]))
  ## 2.1  HANDLE THE NAS!
  cor_mat_i[is.na(cor_mat_i)] <- 0
  
  
  #3. take only upper triangular matrix without the diagonal, and flatten 
  cor_arr_i <- cor_mat_i[upper.tri(cor_mat_i, diag = F)]
  remove(cor_mat_i)
  #4. save it in new matrix
  Connect_diag_mat[i,] <- cor_arr_i
  print(i)
}

# there are no NAS!
mean(is.na(Connect_diag_mat))

hist(Connect_diag_mat, breaks=100)

###### USE  THE NEW DATAFRAME

## DEFINE FUNCTION TO CREATE CONNECTOME

Create_Connectdf <- function(df_list, nroi1){
  OBS=length(df_list)
  #Empty matrix to fill, dimension reduced
  Connect_diag_mat1 <- matrix(NA,nrow = OBS, ncol = nroi1*(nroi1-1)/2)
  
  print(ncol(Connect_diag_mat1))
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

mean(Connect_diag_mat_ == Connect_diag_mat)

## ok! the function creates the same df
remove(Connect_diag_mat)

Connect_diag_mat2 <- data.frame(Connect_diag_mat_, traindf$age,traindf$sex,y=as.factor(traindf$y))


#######USE SVM TEST
Connect_diag_mat2

svmfit_diag_con = svm(y ~ ., data =Connect_diag_mat2, kernel = "linear", gamma=1, cost = 1000, scale = TRUE,cross=10)
summary(svmfit_diag_con)
table(svmfit_diag_con$fitted)

glm()


boxplot(Connect_diag_mat2[,1:100])
hist(Connect_diag_mat2[,2000])

## since data is gaussian try naive bayes?

nb <- naiveBayes(y ~ ., data = Connect_diag_mat2)
summary(nb)


Connect_diag_mat2[,6673]


##test of naivebayes?

nb2 = train(Connect_diag_mat2[,1:6672],Connect_diag_mat2[,6673],'nb',trControl=trainControl(method='cv',number=10))
summary(nb2)

pred <- predict(nb2,Connect_diag_mat2[,1:6672])
table(pred)

table(pred,Connect_diag_mat2[,6673])
confusionMatrix(table(pred,Connect_diag_mat2[,6673]))


#### sampling
idx <- sample(100,nrow(Connect_diag_mat2), replace=T)
train_Connect <- Connect_diag_mat2[-idx,]
test_Connect <- Connect_diag_mat2[idx,]

nb2 = train(train_Connect[,1:6672],train_Connect[,6673],'nb',trControl=trainControl(method='cv',number=10))
summary(nb2)

pred <- predict(nb2,test_Connect[,1:6672])
table(pred)

table(pred,test_Connect[,6673])
confusionMatrix(table(pred,test_Connect[,6673]))

##tryout


###############  KPCA TRY ------------------------------------------------------
dt<-as.matrix(Connect_diag_mat2[,-c(6672,6673)])       
rbf<-rbfdot(sigma=0.01)   
km<-kernelMatrix(rbf,dt)
kpc <- kpca(km,data=Connect_diag_mat2[,-c(6672,6673)],kernel="rbfdot",kpar=list(sigma=0.2),features=50)

kern_comp <- pcv(kpc)
kern_comp2 <- data.frame(kern_comp, y=as.factor(traindf$y))

plot(kern_comp2$X1,kern_comp2$X2,col= kern_comp2$y,lwd=2)




svmfit3 = svm(y ~ ., data = kern_comp2, kernel = "radial", gamma=0.001, cost = 100, scale = TRUE,cross=10)
summary(svmfit3)
#svmfit3$tot.accuracy
table(svmfit3$fitted)


##mean of accuracies
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
#svmfit3 = svm(y ~ ., data = kern_comp2, kernel = "radial", gamma=0.001, cost = 100, scale = TRUE,cross=10)



##BEST ACCURACY 61
## feat 50,kernel sigma 0.01
#svmfit3 = svm(y ~ ., data = kern_comp2, kernel = "radial", gamma=0.001, cost = 1000, scale = TRUE,cross=10)

##BEST ACCURACY 60.5
## features 15, kernel sigma 0.01
### svmfit3 = svm(y ~ ., data = kern_comp2, kernel = "linear", gamma=0.01, cost = 100, scale = TRUE,cross=10)







