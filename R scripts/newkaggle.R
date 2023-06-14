

library(Metrics)
library(e1071)
library(data.table)
library(kernlab)
library(ggplot2)
library(caret)
#install.packages('klaR')
library(klaR)




Connect_diag_mat2 <- data.frame(Connect_diag_mat_,
                                traindf$age,traindf$sex,y=as.factor(traindf$y))

Connect_diag_mat2[,1:5]

length(Connect_diag_mat2[,-c(6672,6673)])
length(Connect_test2)

Connect_test2[,1:6]

write.csv(data.frame(Connect_diag_mat2[,-c(6671,6672,6673)]), file = "Train_Connectome_df.csv", row.names = FALSE)
write.csv(data.frame(Connect_test2[,-6671]), file = "Test_Connectome_df.csv", row.names = FALSE)



###SUPPOSE WE LOAD THE NEW DATA FROM CSV


#binding together the data to perform kpca
total_new <- rbind(data.frame(Connect_diag_mat2[,-c(6671,6672,6673)]), data.frame(Connect_test2[,-6671]))



#kpca transform

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


#FIT WITH TRAIN
svmfit_kern = svm(y ~ ., data = train_kern_kaggle, kernel = "radial", gamma=0.001, cost = 1000, scale = TRUE,cross=10)
summary(svmfit_kern)

####PREDICT TEST!

##ATTEMPT 1 - kaggle 0.60
#cost = 100
pred_kern_test <- predict(svmfit_kern,test_kern_kaggle)

df <- data.frame(testdf$id,pred_kern_test)
colnames(df) <- c('id','target')
df
write.csv(df,file = 'Prediction_kern.csv',row.names = F )


#ATTEMPT 2 - kaggle 0.611
#cost = 1000

## ATTEMPT 3: Vary nfeatures

N_feat=50
kpc2 <- kpca(km2,data=total_new,kernel="rbfdot",kpar=list(sigma=0.2),features=N_feat)



# CREATE DATAFRAMES FROM KERNEL COMPONENTS
kern_comp_test <- pcv(kpc2)
kern_comp_test2 <- data.frame(kern_comp_test)

#SPLIT IN TRAIN AND TEST
train_kern_kaggle <- data.frame(kern_comp_test2[1:600,], y=as.factor(traindf$y))
test_kern_kaggle <- kern_comp_test2[601:799,]


#FIT WITH TRAin
svmfit_kern3 = svm(y ~ ., data = train_kern_kaggle, kernel = "radial", gamma=0.001, cost = 1000, scale = FALSE,cross=10)
summary(svmfit_kern3)



pred_kern_test2 <- predict(svmfit_kern,test_kern_kaggle)
#explore difference from previous pred
table(pred_kern_test,pred_kern_test2)
table(pred_kern_test2)

df2 <- data.frame(testdf$id,pred_kern_test2)
colnames(df2) <- c('id','target')
df2
write.csv(df2,file = 'Prediction_kern2.csv',row.names = F )




