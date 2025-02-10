library(readxl)
library(MuMIn)
library(tidyverse)
library(randomForest)
library(varSelRF)
library(pROC)
library(ggplot2)
library(caret)
library(rfPermute)
library(neuralnet)
library(lightgbm)
library(xgboost)
library(shapviz)
library(PRROC)
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


res_pred<-predict(res_rf, newdata=testdata[,-1])

pr_rf <- pr.curve(scores.class0 = res_pred[,2],
                  weights.class0 = as.numeric(testdata$Subtype)-1,
                  curve = TRUE)

# pr_data <- data.frame(Recall = pr_rf$curve[, 1],
#                       Precision = pr_rf$curve[, 2])
# 
# ggplot(pr_data, aes(x = Recall, y = Precision)) +
#   geom_line(color = "blue", size = 1) +
#   labs(title = "PR Curve",
#        
#        x = "Recall",
#        y = "Precision") +
#   theme_minimal()
roc_rf<-roc(as.ordered(testdata$Subtype) ,as.ordered(res_pred[,1]),smoth=TRUE)



new_dat$Subtype <- as.numeric(new_dat$Subtype)-1

traindata <- new_dat[train,]
testdata <- new_dat[-train,]
# traindata <- xgb.DMatrix(data = as.matrix(traindata[,-1]),label=traindata$Subtype)
# testdata <- xgb.DMatrix(data = as.matrix(testdata[,-1]),label=testdata$Subtype)
traindata <- list(data = as.matrix(traindata[,-1]),label=traindata$Subtype)
testdata <- list(data = as.matrix(testdata[,-1]),label=testdata$Subtype)

res_xgboost = xgboost(
  data = traindata$data,
  label = traindata$label,
  max_depth = 6, 
  eta = 0.3, 
  
  nrounds = 100,
  objective = "binary:logistic",
  eval_metric="auc")
xgb_pred <- predict(res_xgboost,testdata$data)

# pr_xgboost<- pr.curve(scores.class0 = xgb_pred[testdata$label == 1],
#                   scores.class1 = xgb_pred[testdata$label == 0],
#                   curve = TRUE)
pr_xgboost<-pr.curve(scores.class0 = xgb_pred,
                  weights.class0 = testdata$label,
                  curve = TRUE)

roc_xgboost<-roc(as.ordered(testdata$label) ,as.ordered(xgb_pred),smoth=TRUE)





traindata <- new_dat[train,]
testdata <- new_dat[-train,]

# traindata <- list(data = as.matrix(traindata[,-1]),label=traindata$Subtype)
testdata <- list(data = as.matrix(testdata[,-1]),label=testdata$Subtype)
traindata <- lgb.Dataset(data = data.matrix(traindata[,-1]),label=traindata$Subtype)
#testdata <- lgb.Dataset(data =  data.matrix(testdata[,-1]),label=testdata$Subtype)

res_gbm<- lgb.train(  params = list(        
  objective = 'binary',        
  metric = 'auc',         
  
  learning_rate = 0.1,         
  max_depth =2        ),
  data = traindata,
  nrounds = 100,
)
summary(res_gbm)


gbm_pred <- predict(res_gbm,testdata$data)
# pr_gbm <- pr.curve(scores.class0 = gbm_pred[testdata$label == 1],
#                     scores.class1 = gbm_pred[testdata$label == 0],
#                   curve = TRUE)

pr_gbm<-pr.curve(scores.class0 = gbm_pred,
                     weights.class0 = testdata$label,
                     curve = TRUE)

roc_gbm<-roc(as.ordered(testdata$label) ,as.ordered(gbm_pred),smoth=TRUE)


plot(roc_gbm)


normalize <- function(x) {
  (x - min(x)) / (max(x) - min(x))
}
new_dat2 <- as.data.frame(lapply(new_dat, normalize))
traindata <- new_dat2[train,]
testdata <- new_dat2[-train,]
# traindata <- xgb.DMatrix(data = as.matrix(traindata[,-1]),label=traindata$Subtype)
testdata <- list(data = as.matrix(testdata[,-1]),label=testdata$Subtype)

nn_res <- neuralnet(Subtype ~ Density+Humidity+Precipitation+Temperature, 
                    data = traindata , 
                    hidden = c(5,4,3,2),
                    stepmax=1e10,# 隐藏层
                    linear.output = FALSE)
nn_pred <- compute(nn_res,testdata$data)$net.result

# pr_nn<- pr.curve(scores.class0 = nn_pred[testdata$label == 1],
#                   scores.class1 = nn_pred[testdata$label== 0],
#                   curve = TRUE)

pr_nn<-pr.curve(scores.class0 = nn_pred,
                 weights.class0 = testdata$label,
                 curve = TRUE)

pr_data <- data.frame(Recall = pr_nn$curve[, 1],
                      Precision = pr_nn$curve[, 2])

ggplot(pr_data, aes(x = Recall, y = Precision)) +
  geom_line(color = "blue", size = 1) +
  labs(title = "PR Curve",
       
       x = "Recall",
       y = "Precision") +
  theme_minimal()

roc_nn<-roc(as.ordered(testdata$label) ,c(nn_pred),smoth=TRUE)
roc_nn


sv <- shapviz(
  res_xgboost,
  X_pred = data.matrix(new_dat[,-1]),
  X=data.matrix(new_dat[,-1])
)



save(nn_res,file="Figure/nn_res.RData")
save(res_gbm,file="Figure/res_gbm.RData")
save(res_xgboost,file="Figure/res_xgboost.RData")
save(res_rf,file="Figure/res_rf.RData")

save(roc_nn,file="Figure/roc_nn.RData")
save(roc_gbm,file="Figure/roc_gbm.RData")
save(roc_xgboost,file="Figure/roc_xgboost.RData")
save(roc_rf,file="Figure/roc_rf.RData")


save(pr_nn,file="Figure/pr_nn.RData")
save(pr_gbm,file="Figure/pr_gbm.RData")
save(pr_xgboost,file="Figure/pr_xgboost.RData")
save(pr_rf,file="Figure/pr_rf.RData")


save(sv,file="Figure/sv.RData")




