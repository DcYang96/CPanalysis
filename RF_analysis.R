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




dat_type <- sapply(1:nrow(dat),function(c)
  paste(strsplit( dat$Subgenotype[c],""  )[[1]][1:3],sep="",collapse=""))


new_dat <- data.frame(Subtype=dat_type,Density=dat$`Dairy cattle stocking density`,Humidity=dat$`Relative Humidity`,
  Precipitation=dat$`Annual Precipitation`,Temperature=dat$temperature)
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
res_rf= randomForest(Subtype ~ ., data = traindata, ntree=500,importance=TRUE, proximity=TRUE)




res_imp <- data.frame(importance(res_rf,scale=TRUE), check.names = FALSE)


varImpPlot(res_rf, main = "variable importance",scale = T,color="red",gcolor=c("blue","yellow"),lcolor="blue")



res_pred<-predict(res_rf, newdata=testdata,type="prob")

roc<-roc(as.ordered(testdata$Subtype) ,as.ordered(res_pred[,1]),smoth=TRUE)
plot(roc,print.auc=T, auc.polygon=T, grid=c(0.1, 0.2), grid.col=c("green","red"), 
  max.auc.polygon=T, auc.polygon.col="skyblue",print.thres=T)

result <- replicate(10, rfcv(new_dat[-1], new_dat$Subtype, cv.fold = 10,step = 0.8), simplify=FALSE)
error.cv <- (sapply(result, '[[', 'error.cv'))



pre_dat <- new_dat[1:241,1:5]
pre_dat$Density <- 1:max(new_dat$Density )
pre_dat$Humidity <- mean(new_dat$Humidity)
pre_dat$Precipitation <- mean(new_dat$Precipitation)
  pre_dat$Temperature <- mean(new_dat$Temperature)


res_pred_type<-predict(res_rf, newdata=pre_dat[,2:5],type="prob")







save(res_imp,file="res_imp.RData")
save(oob.error.data,file="oob.error.data.RData")
save(roc,file="roc.RData")
save(result,file="result.RData")
save(error.cv,file="error.cv.RData")
save(res_pred_type,file="res_pred_type.RData")













rm(list = ls())
dat <- read_excel("global_data.xlsx")
dat <- as.data.frame(dat)




new_dat <- data.frame(Rate=dat$`Base mutation rate`,Density=dat$`Dairy cattle stocking density`,Humidity=dat$`Relative Humidity`,
  Precipitation=dat$`Annual Precipitation`,Temperature=dat$temperature)

# set.seed(1)
train <- sample(nrow(new_dat), nrow(new_dat)*0.7)
traindata <- new_dat[train,]
testdata <- new_dat[-train,]
res_rf= randomForest(Rate ~ ., data = traindata, ntree=1000,importance=TRUE)
res_rf
plot(res_rf,main="Bagging OOB Errors")


res_rf= randomForest(Rate ~ ., data = traindata, ntree=300,importance=TRUE)
res_rf

res_imp <- data.frame(importance(res_rf,scale=TRUE), check.names = FALSE)


varImpPlot(res_rf, main = "variable importance",scale = T,color="red",gcolor=c("blue","yellow"),lcolor="blue")




result <- replicate(10, rfcv(new_dat[-1], new_dat$Rate, cv.fold = 10,step = 0.8), simplify=FALSE)
res_cv <- data.frame(sapply(result, '[[', 'error.cv'))
res_cv$res <- rownames(res_cv)
res_cv <- reshape2::melt(res_cv, id = 'res')
res_cv$res <- as.numeric(as.character(res_cv$res))
res_cv.mean <- aggregate(res_cv$value, by = list(res_cv$res), FUN = mean)

ggplot(res_cv.mean, aes(Group.1, x)) +
  geom_line() +
  theme(panel.grid = element_blank(), panel.background = element_rect(color = 'black', fill = 'transparent')) +
  labs(title = '',x = 'Number of Varible', y = 'Cross-validation error')
error.cv <- (sapply(result, '[[', 'error.cv'))

matplot(result[[1]]$n.var, cbind(rowMeans(error.cv), error.cv), type="l",
  lwd=c(2, rep(1, ncol(error.cv))), col=2, lty=1, log="x",
  xlab="Number of variables", ylab="CV Error")








pre_dat <- new_dat[1:241,1:5]
pre_dat$Density <- 1:max(new_dat$Density )
pre_dat$Humidity <- mean(new_dat$Humidity)
pre_dat$Precipitation <- mean(new_dat$Precipitation)
pre_dat$Temperature <- mean(new_dat$Temperature)


res_pred_rate<-predict(res_rf, newdata=pre_dat[,2:5])


plot(res_pred_rate)


save(res_imp,file="res_imp_rate.RData")
save(res_rf,file="res_rf_rate.RData")

save(result,file="result_rate.RData")
save(error.cv,file="error.cv_rate.RData")
save(res_pred_rate,file="res_pred_rate.RData")













