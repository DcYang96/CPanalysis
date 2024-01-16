library(readxl)
library(MuMIn)
library(tidyverse)
library(randomForest)
library(varSelRF)
library(pROC)
library(ggplot2)
library(caret)
library(rfPermute)
rm(list = ls())
dat <- read_excel("global_data.xlsx")
dat <- as.data.frame(dat)
dat_region <- c()


for (i in 1:nrow(dat)) {
  if(as.numeric(strsplit(dat$lat[i],"°"))<20){
    dat_region[i] <- 0}
  if(as.numeric(strsplit(dat$lat[i],"°"))<35&as.numeric(strsplit(dat$lat[i],"°"))>=20){
    dat_region[i] <- 1}
  if(as.numeric(strsplit(dat$lat[i],"°"))>35){
    dat_region[i] <- 2}
  
}


dat <- cbind(dat,dat_region)


dat_type <- sapply(1:nrow(dat),function(c)
  paste(strsplit( dat$Subgenotype[c],""  )[[1]][1:3],sep="",collapse=""))


new_dat <- data.frame(Subtype=dat_type,Density=dat$`Dairy cattle stocking density`,Humidity=dat$`Relative Humidity`,
  Precipitation=dat$`Annual Precipitation`,Temperature=dat$temperature,Lat=dat$lat,Lng=dat$lng)
new_dat <- new_dat %>%
  
  mutate(Subtype = as.factor(Subtype))



train <- sample(nrow(new_dat), nrow(new_dat)*0.7)
traindata <- new_dat[train,]
testdata <- new_dat[-train,]
res_rf= randomForest(Subtype ~ ., data = traindata, ntree=1000,importance=TRUE, proximity=TRUE)

oob.error.data <- data.frame(
  Trees=rep(1:nrow(res_rf$err.rate), times=3),
  Type=rep(c("OOB", "IIa", "IId"), each=nrow(res_rf$err.rate)),
  Error=c(res_rf$err.rate[,"OOB"],
    res_rf$err.rate[,"IIa"],
    res_rf$err.rate[,"IId"]))
res_rf= randomForest(Subtype ~ ., data = traindata, ntree=250,importance=TRUE, proximity=TRUE)
res_rf



res_imp <- data.frame(importance(res_rf,scale=TRUE), check.names = FALSE)





res_pred<-predict(res_rf, newdata=testdata,type="prob")

roc<-roc(as.ordered(testdata$Subtype) ,as.ordered(res_pred[,1]),smoth=TRUE)
plot(roc,print.auc=T, auc.polygon=T, grid=c(0.1, 0.2), grid.col=c("green","red"), 
  max.auc.polygon=T, auc.polygon.col="skyblue",print.thres=T)
mytraindata<- cbind(traindata[-1], matrix(runif(96 * nrow(traindata)), nrow(traindata), 96))
result<- rfcv(mytraindata[-1], traindata$Subtype, cv.fold = 10)
with(result,plot(n.var,error.cv,log="x",type="o",lwd=2))

result <- replicate(5, rfcv(traindata[-1], traindata$Subtype, cv.fold = 10), simplify=FALSE)
error.cv <- (sapply(result, '[[', 'error.cv'))


