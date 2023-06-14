library(Metrics)
library(e1071)
library(data.table)
library(kernlab)


testdf <- fread('statistical-learning-hw03-2022/test_hw03.csv') 


# CREATE a list of Df, each one containing a single ROI, with all time slots
test_ROI_l = list()
for (i in 1:nroi){
  dfs <- list(testdf[,(4+(i-1)*ntime):(4+(i-1)*ntime + (ntime-1))])
  test_ROI_l <- append(test_ROI_l,dfs)
}
remove(dfs)

test_ROI_l[[115]]



# CREATE a list of Df, each one containing the same time slot, with all ROI

#test dataframe as matrix
testdf_m <-as.matrix(testdf[,4:13343])

test_time_l = list()
for (i in 1:ntime){
  seq_time_i <- seq(i,13225+i,ntime)
  dfs <- list(data.frame(testdf_m[,seq_time_i]))
  test_time_l <- append(test_time_l,dfs)
}
remove(dfs)
heatmap(as.matrix(test_time_l[[1]]))


##### CREATE SUMMARY DF
Mean_sd_test_df <- matrix(NA,nrow = nrow(testdf),ncol = 2*nroi+2*ntime)
k=0
for(i in 1:nroi){
  print(k)
  s <- Create_mean_sd_info(test_ROI_l[[i]])
  Mean_sd_test_df[,(1+k):(2+k)]<- s
  k=k+2
}
for(i in 1:ntime){
  print(k)
  s <- Create_mean_sd_info(test_time_l[[i]])
  Mean_sd_test_df[,(1+k):(2+k)]<- s
  k=k+2
}

#heatmap(cor(Mean_sd_df))
hist(cor(Mean_sd_test_df))

Mean_sd_test_df2 <- data.frame(scale(Mean_sd_test_df))

#predict values
pred <- predict(svmfit,Mean_sd_test_df2)
table(pred)


df <- data.frame(testdf$id,pred)
colnames(df) <- c('id','target')
df
write.csv(df,file = 'Prediction1.csv',row.names = F )

