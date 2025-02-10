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




new_dat <- data.frame(Rate=dat$`Base mutation rate`,Density=dat$`Dairy cattle stocking density`,Humidity=dat$`Relative Humidity`,
                      Precipitation=dat$`Annual Precipitation`,Temperature=dat$temperature)



rf_folds <- createFolds(new_dat$Rate, k = 5, list = TRUE)

# 初始化结果容器
rf_r2 <- numeric(5)
rf_pcc <- numeric(5)
# K 折交叉验证
for (i in 1:5) {
  # 划分训练集和测试集
  test_idx <- rf_folds[[i]]
  train_idx <- setdiff(seq_along(new_dat$Rate), test_idx)
  
  train_X <- new_dat[train_idx, -1]
  train_y <- new_dat$Rate[train_idx]
  test_X <- new_dat[test_idx, -1]
  test_y <- new_dat$Rate[test_idx]
  
  # 训练随机森林回归模型
  rf_model <- randomForest(x = train_X, y = train_y, ntree = 1000, mtry = 3)
  
  # 在测试集上预测
  predictions <- predict(rf_model, newdata = test_X)
  
  # 计算 R^2
  ss_total <- sum((test_y - mean(test_y))^2)   # 总平方和
  ss_residual <- sum((test_y - predictions)^2) # 残差平方和
  rf_r2[i] <- 1 - (ss_residual / ss_total)
  rf_pcc[i] <- cor(test_y, predictions)
}





xgboost_folds <- createFolds(new_dat$Rate, k = 5, list = TRUE)

# 初始化结果容器
xgboost_r2 <- numeric(5)
xgboost_pcc <- numeric(5)
# K 折交叉验证
for (i in 1:5) {
  # 划分训练集和测试集
  test_idx <- xgboost_folds[[i]]
  train_idx <- setdiff(seq_along(new_dat$Rate), test_idx)
  
  train_X <- new_dat[train_idx, -1]
  train_y <- new_dat$Rate[train_idx]
  test_X <- new_dat[test_idx, -1]
  test_y <- new_dat$Rate[test_idx]
  

traindata <- xgb.DMatrix(data = as.matrix(train_X),label=train_y)
testdata <- xgb.DMatrix(data = as.matrix(test_X),label=test_y)

  xgboost_model <-xgboost(
    data = traindata,
    max_depth = 6, 
    eta = 0.3, 
    
    nrounds = 100,
    subsample = 0.8,               
    colsample_bytree = 0.8 ,       
    objective = "reg:squarederror")
  
  # 在测试集上预测
  predictions <- predict( xgboost_model, newdata = testdata)
  
  # 计算 R^2
  ss_total <- sum((test_y - mean(test_y))^2)   # 总平方和
  ss_residual <- sum((test_y - predictions)^2) # 残差平方和
  xgboost_r2[i] <- 1 - (ss_residual / ss_total)
  xgboost_pcc[i] <- cor(test_y, predictions)
}


lgbm_folds <- createFolds(new_dat$Rate, k = 5, list = TRUE)

# 初始化结果容器
lgbm_r2 <- numeric(5)
lgbm_pcc <- numeric(5)
# K 折交叉验证
for (i in 1:5) {
  # 划分训练集和测试集
  test_idx <- lgbm_folds[[i]]
  train_idx <- setdiff(seq_along(new_dat$Rate), test_idx)
  
  train_X <- new_dat[train_idx, -1]
  train_y <- new_dat$Rate[train_idx]
  test_X <- new_dat[test_idx, -1]
  test_y <- new_dat$Rate[test_idx]
  
  traindata <- lgb.Dataset(data = data.matrix(train_X),label=train_y)
  
  lgbm_model <-lgb.train(  params = list(        
    objective = 'regression',        
    
    metric = "rmse",               
    boosting = "gbdt",             
    learning_rate = 0.1,           
    max_depth =3     ,feature_fraction = 0.8,       
    bagging_fraction = 0.8,        
    bagging_freq = 5     ),
    data = traindata,
    nrounds = 100,
  )
 
  predictions <- predict( lgbm_model, newdata = as.matrix(test_X))
  
  # 计算 R^2
  ss_total <- sum((test_y - mean(test_y))^2)   # 总平方和
  ss_residual <- sum((test_y - predictions)^2) # 残差平方和
  lgbm_r2[i] <- 1 - (ss_residual / ss_total)
  lgbm_pcc[i] <- cor(test_y, predictions)
}


nn_folds <- createFolds(new_dat$Rate, k = 5, list = TRUE)
normalize <- function(x) {
  (x - min(x)) / (max(x) - min(x))
}
new_dat2 <- as.data.frame(lapply(new_dat, normalize))
# 初始化结果容器
nn_r2 <- numeric(5)
nn_pcc <- numeric(5)
# K 折交叉验证
for (i in 1:5) {
  # 划分训练集和测试集
  test_idx <- nn_folds[[i]]
  train_idx <- setdiff(seq_along(new_dat2$Rate), test_idx)
  
  train_X <- new_dat2[train_idx, -1]
  train_y <- new_dat2$Rate[train_idx]
  test_X <- new_dat2[test_idx, -1]
  test_y <- new_dat2$Rate[test_idx]
  
  traindata <- as.matrix(new_dat2[train_idx, ])
  testdata <- as.matrix(new_dat2[test_idx, ])
  
  nn_model <-neuralnet(Rate ~ Density+Humidity+Precipitation+Temperature, 
                         data = traindata , 
                         hidden = c(5,4,3),  # 隐藏层
                         linear.output = T)
  
  predictions <- compute( nn_model, as.matrix(testdata[,-1]))
  
  # 计算 R^2
  ss_total <- sum((test_y - mean(test_y))^2)   # 总平方和
  ss_residual <- sum((test_y - predictions$net.result)^2) # 残差平方和
  nn_r2[i] <- 1 - (ss_residual / ss_total)
  nn_pcc[i] <- cor(test_y, predictions$net.result)
}



r2 <- data.frame(rf_r2,xgboost_r2,lgbm_r2,nn_r2)
pcc <- data.frame(rf_pcc,xgboost_pcc,lgbm_pcc,nn_pcc)






xgboost_folds <- createFolds(new_dat$Rate, k = 5, list = TRUE)


  test_idx <- xgboost_folds[[2]]
  train_idx <- setdiff(seq_along(new_dat$Rate), test_idx)
  
  train_X <- new_dat[train_idx, -1]
  train_y <- new_dat$Rate[train_idx]
  test_X <- new_dat[test_idx, -1]
  test_y <- new_dat$Rate[test_idx]
  
  
  traindata <- xgb.DMatrix(data = as.matrix(train_X),label=train_y)
  testdata <- xgb.DMatrix(data = as.matrix(test_X),label=test_y)
  
  xgboost_model <-xgboost(
    data = traindata,
    max_depth = 6, 
    eta = 0.3, 
    
    nrounds = 100,
    subsample = 0.8,               
    colsample_bytree = 0.8 ,       
    objective = "reg:squarederror")
  

  predictions <- predict( xgboost_model, newdata = testdata)
  

  sv_reg <- shapviz(
    xgboost_model,
    X_pred = data.matrix(new_dat[,-1]),
    X=data.matrix(new_dat[,-1])
  )
  ss_total <- sum((test_y - mean(test_y))^2)   # 总平方和
  ss_residual <- sum((test_y - predictions)^2) # 残差平方和
   1 - (ss_residual / ss_total)


load(file="Figure/r2.RData")


save(r2,file="Figure/r2.RData")
save(pcc,file="Figure/pcc.RData")

save(  sv_reg,file="Figure/sv_reg.RData")

save(  xgboost_model,file="Figure/xgboost_model.RData")


