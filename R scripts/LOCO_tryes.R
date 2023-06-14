

#quantile(npbs$boot.dist, 0.025)
#quantile(npbs$boot.dist, 0.975)



#library(boot)
#boot.ci(npbs$boot.dist,conf=0.95,type = "perc")
#conf=(1-alpha), 

#sample(delta_acc_list, replace = TRUE)




##method 1  ---------------------------------------
N <- 25
Proportion <- 4

Variables <- seq(1,10,1)
Theta_j_list <- c()
Lower_bund_list <- c()
Upper_bund_list <- c()

system.time(
  for (Var_j in Variables){
    print(Var_j)
    delta_acc_list <- Create_LOCO_list(N,train,Proportion,Var_j)
    
    #NON PARAMETRIC BOOTSTRAP CI
    npbs <- np.boot(x = delta_acc_list, R=10000,statistic = median, level = 0.95)
    Theta_j_list<-append(Theta_j_list,npbs$t0)
    
    Lower_bund_list <- append(Lower_bund_list,as.numeric(npbs$percent[1])) 
    Upper_bund_list <- append(Upper_bund_list,as.numeric(npbs$percent[2])) 
  }
)










#### Evaluate Variable importance for all variables TRY 1 -------------------

Variables <- seq(1,10,1)
Theta_j_list <- c()
N <- 500
for (Var_j in Variables){
  #Choose variable to exclude
  delta_acc_list <-c()
  for(k in 1:N){
    #1. Randomly split the data in two
    n1 <- floor(nrow(train)/10)
    
    idx <- sample(nrow(train),n1,replace = FALSE)
    train_tr <- train[-idx,]
    train_ts <-train[idx,]
    
    Ncols <- ncol(train_tr)
    
    #2. Compute estimate f_n1_hat
    svmfit_LOCO = svm(y ~ ., data = train_tr, kernel = "radial", gamma=0.01, cost = 100, scale = TRUE)
    
    f_n1_hat <- predict(svmfit_LOCO,train_ts[-(Ncols)])
    
    #3. select A variable j and compute estimate f_n1_wj_hat
    #Var_j
    seq_cols <- seq(1,Ncols,1)
    seq_cols_w = seq_cols[! seq_cols %in% Var_j]
    
    train_tr_w <- train_tr[,seq_cols_w]
    train_ts_w <- train_ts[,seq_cols_w]
    #new dataset train_tr_w is without variable
    #train_tr_w
    svmfit_LOCO = svm(y ~ ., data = train_tr_w, kernel = "radial", gamma=0.01, cost = 100, scale = TRUE)
    
    f_n1_wj_hat <- predict(svmfit_LOCO,train_ts_w[-(Ncols-1)])
    
    #4. Theta_j, the difference between the accuracy, delta_acuracy
    
    delta_acc <- accuracy(train_ts$y,f_n1_hat) - accuracy(train_ts_w$y,f_n1_wj_hat)
    delta_acc_list <- append(delta_acc_list, delta_acc)
  }
  
  Theta_j <- median(delta_acc_list)
  Theta_j_list <- append(Theta_j_list,Theta_j)
  print(paste(Var_j,Theta_j))
}

plot(Variables, Theta_j_list)

#Theta_j_list_500 <- Theta_j_list

#Theta_j_list_200 <- Theta_j_list
plot(Variables, Theta_j_list_500)








#### NOT THIS... TRY AGAIN WITH FIRST DATASET


###########LOCO ALGORITM ------------------------------------------------------
#1. Randomly split the data in two
Ncols <- ncol(train_tr)

n1 <- floor(nrow(train)/4)
n1
idx <- sample(nrow(train),n1,replace = FALSE)
train_tr <- train[-idx,]
train_ts <-train[idx,]

#2. Compute estimate f_n1_hat
svmfit_LOCO = svm(y ~ ., data = train_tr, kernel = "radial", gamma=0.01, cost = 1000, scale = TRUE)

f_n1_hat <- predict(svmfit_LOCO,train_tr[-(Ncols)])

#accuracy(train_tr$y, f_n1_hat)

#3. select A variable j and compute estimate f_n1_wj_hat
Var_j <- 9

seq_cols <- seq(1,Ncols,1)
seq_cols_w = seq_cols[! seq_cols %in% Var_j]

train_tr_w <- train_tr[,seq_cols_w]
train_ts_w <- train_ts[,seq_cols_w]

#new dataset train_tr_w is without variable
#train_tr_w
svmfit_LOCO = svm(y ~ ., data = train_tr_w, kernel = "radial", gamma=0.01, cost = 1000, scale = TRUE)

f_n1_wj_hat <- predict(svmfit_LOCO,train_tr_w[-(Ncols-1)])

#accuracy(train_tr_w$y,f_n1_wj_hat)



#4. Theta_j, the difference between the accuracy, delta_acuracy

accuracy(train_tr$y,f_n1_hat)
accuracy(train_tr_w$y,f_n1_wj_hat)

delta_acc <- accuracy(train_tr$y,f_n1_hat) -accuracy(train_tr_w$y,f_n1_wj_hat)
delta_acc


#abs(as.numeric(f_n1_hat =="autism") - as.numeric(train_tr$y=="autism")) - abs(as.numeric(f_n1_wj_hat =="autism") - as.numeric(train_tr_w$y=="autism"))



par(mfrow=c(2,1))
Plot_CI('frac',df_CI_perc[,1],df_CI_perc[,2],df_CI_perc[,3])
Plot_CI('diff',df_CI_diff[,1],df_CI_diff[,2],df_CI_diff[,3])

#seq_y <- round(seq(min(Lower_bund_list),max(Upper_bund_list),0.01),2)
#seq_y <- seq(0.95,1.05,0.01)
#axis(2, at=seq_y,labels=seq_y, col.axis="black", las=2)

#lines(Upper_bund_list, col="red")
#lines(Lower_bund_list, col="blue")

Save_Lower <- Lower_bund_list
Save_Upper <- Upper_bund_list
Save_ThetaJ <- Theta_j_list
df_Bounds <- data.frame(Save_ThetaJ,Save_Lower,Save_Upper)
df_Bounds


