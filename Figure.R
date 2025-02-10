

load(file="Figure/roc_nn.RData")
load(file="Figure/roc_gbm.RData")
load(file="Figure/roc_xgboost.RData")
load(file="Figure/roc_rf.RData")


load(file="Figure/pr_nn.RData")
load(file="Figure/pr_gbm.RData")
load(file="Figure/pr_xgboost.RData")
load(file="Figure/pr_rf.RData")

load(file="Figure/sv.RData")

roc_data <- data.frame(
  fpr = c((1-roc_rf$specificities), (1-roc_xgboost$specificities),
          (1-roc_gbm$specificities),(1-roc_nn$specificities)),
  tpr = c(roc_rf$sensitivities, roc_xgboost$sensitivities,roc_gbm$sensitivities,roc_nn$sensitivities),
  model = rep(c("RF", "XGBOOST","lightGBM","NN"), c(length( roc_rf$sensitivities), 
                                                    length(roc_xgboost$sensitivities),length(roc_gbm$sensitivities),length(roc_nn$sensitivities))))


roc_data <- roc_data[nrow(roc_data):1,]


auc_rf <- auc(roc_rf)
auc_xgboost <- auc(roc_xgboost)
auc_gbm <- auc(roc_gbm)
auc_nn <- auc(roc_nn)


roc_data$model <- factor(roc_data$model, levels = c("RF", "XGBOOST","lightGBM","NN"))
auc_labels <- c(paste("RF (AUC =", round(auc_rf, 4), ")"),
                paste("XGBOOST (AUC =", round(auc_xgboost, 4), ")"),
                paste("lightGBM (AUC =", round(auc_gbm, 4), ")"),
                paste("NN (AUC =", round(auc_nn, 4), ")")
                
)


tt <- ggplot(roc_data, aes(x = fpr, y = tpr, color = model)) +
  geom_line(size = 1.5,alpha=0.5) +
  geom_abline(slope = 1, intercept = 0, size = 1.5,linetype = "dashed") + 
  scale_color_manual(values = c("darkgreen", "royalblue3","brown2","orange"), labels = auc_labels) +
  theme_minimal()+
  # scale_x_continuous(expand = c(0,0),breaks  =seq(0,1,0.25),limits = c(0,1))+
  # scale_y_continuous(expand = c(0,0),breaks  =seq(0,1,0.25),limits = c(0,1))+
  theme_bw()+
  theme(panel.grid.major=element_line(colour=NA),
        panel.background = element_rect(fill ="transparent",colour = NA),
        plot.background = element_rect(fill = "transparent",colour = NA),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(#face="bold",
          color="black", size=16),
        axis.text.y =element_text(#face="bold",
          color="black", size=16),plot.margin=unit(c(1,1,1,1),"cm"))+
  theme(axis.ticks.length.y = unit(0.4,"cm"))+    theme(axis.ticks.length.x = unit(0.4,"cm"))+
  theme(text=element_text(size=16,  family="serif"),panel.border = element_rect(fill=NA,color="black", size=1.5, linetype="solid"))+
  xlab(NULL)+ylab(NULL) + theme(legend.title = element_blank(), legend.position = c(0.8, 0.25))
pdf("Figure/Figure1A.pdf",width = 8,height = 8)
print(tt)

dev.off()




pr_data <- data.frame(
  Recall = c(pr_rf$curve[,1], pr_xgboost$curve[,1], pr_gbm$curve[,1], pr_nn$curve[,1]),

  Precision = c(pr_rf$curve[,2], pr_xgboost$curve[,2], pr_gbm$curve[,2], pr_nn$curve[,2]),
  model = rep(c("RF", "XGBOOST","lightGBM","NN"), c(length( pr_rf$curve[,1]), 
length(pr_xgboost$curve[,1]),length(pr_gbm$curve[,1]),length(pr_nn$curve[,1]))))


#pr_data <- pr_data[nrow(pr_data):1,]


ap_rf <- pr_rf$auc.integral
ap_xgboost <- pr_xgboost$auc.integral
ap_gbm <- pr_gbm$auc.integral
ap_nn <- pr_nn$auc.integral


pr_data$model <- factor(pr_data$model, levels = c("RF", "XGBOOST","lightGBM","NN"))
auc_labels <- c(paste("RF (AP =", round(ap_rf, 4), ")"),
                paste("XGBOOST (AP =", round(ap_xgboost, 4), ")"),
                paste("lightGBM (AP =", round(ap_gbm, 4), ")"),
                paste("NN (AP =", round(ap_nn, 4), ")")
                
)

tt <- ggplot(pr_data, aes(x =  Recall, y = Precision, color = model)) +
  geom_line(size = 1.5,alpha=0.5) +
 # geom_abline(slope = 1, intercept = 0, size = 1.5,linetype = "dashed") + 
  scale_color_manual(values = c("darkgreen", "royalblue3","brown2","orange"), labels = auc_labels) +
  theme_minimal() +
  # scale_x_continuous(expand = c(0,0),breaks  =seq(0,1,0.25),limits = c(-0.1,1.1))+
  # scale_y_continuous(expand = c(0,0),breaks  =seq(0,1,0.25),limits = c(-0.1,1.1))+  
  theme_bw()+
  theme(panel.grid.major=element_line(colour=NA),
        panel.background = element_rect(fill ="transparent",colour = NA),
        plot.background = element_rect(fill = "transparent",colour = NA),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(#face="bold",
          color="black", size=16),
        axis.text.y =element_text(#face="bold",
          color="black", size=16),plot.margin=unit(c(1,1,1,1),"cm"))+
  theme(axis.ticks.length.y = unit(0.4,"cm"))+    theme(axis.ticks.length.x = unit(0.4,"cm"))+
  theme(text=element_text(size=16,  family="serif"),panel.border = element_rect(fill=NA,color="black", size=1.5, linetype="solid"))+
  xlab(NULL)+ylab(NULL) + theme(legend.title = element_blank(), legend.position = c(0.25, 0.25))
pdf("Figure/Figure1B.pdf",width = 8,height = 8)
print(tt)

dev.off()



tt <- sv_importance(sv, kind = "both") +
  theme_minimal()+
  # scale_x_continuous(expand = c(0,0),breaks  =seq(0,1,0.25),limits = c(0,1))+
  # scale_y_continuous(expand = c(0,0),breaks  =seq(0,1,0.25),limits = c(0,1))+
  theme_bw()+
  theme(panel.grid.major=element_line(colour=NA),
        panel.background = element_rect(fill ="transparent",colour = NA),
        plot.background = element_rect(fill = "transparent",colour = NA),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(#face="bold",
          color="black", size=16),
        axis.text.y =element_text(#face="bold",
          color="black", size=16),plot.margin=unit(c(1,1,1,1),"cm"))+
  theme(axis.ticks.length.y = unit(0.4,"cm"))+    theme(axis.ticks.length.x = unit(0.4,"cm"))+
  theme(text=element_text(size=16,  family="serif"),panel.border = element_rect(fill=NA,color="black", size=1.5, linetype="solid"))+
  xlab(NULL)+ylab(NULL) + theme(legend.title = element_blank(), legend.position = c(0.9, 0.25))
pdf("Figure/Figure1E.pdf",width = 8,height = 8)
print(tt)

dev.off()


x <- c("Density", "Humidity", "Precipitation", "Temperature")
tt <- sv_dependence(sv, v = x,color_var=c("Humidity","Density","Density","Density"))
pdf("Figure/FigureS1A.pdf",width = 8,height = 8)
print(tt)

dev.off()






load(file="Figure/r2.RData")
load(file="Figure/pcc.RData")
load(file="Figure/sv_reg.RData")

colMeans(r2)
colMeans(pcc)

library(tidyr)
library(dplyr)


r2_data <- data.frame(t(r2))
r2_data <- cbind(c("RF", "XGBoost", "LightGBM", "NN"),r2_data)
colnames(r2_data) <- c("Model","Fold1","Fold2","Fold3","Fold4","Fold5")



pcc_data <- data.frame(t(pcc))
pcc_data <- cbind(c("RF", "XGBoost", "LightGBM", "NN"),pcc_data)
colnames(pcc_data) <- c("Model","Fold1","Fold2","Fold3","Fold4","Fold5")

# 计算均值和标准差
r2_long <- r2_data %>%
  pivot_longer(cols = starts_with("Fold"), names_to = "Fold", values_to = "Value") %>%
  group_by(Model) %>%
  summarise(Mean = mean(Value), SD = sd(Value)) %>%
  mutate(Metric = "R2")

pcc_long <- pcc_data %>%
  pivot_longer(cols = starts_with("Fold"), names_to = "Fold", values_to = "Value") %>%
  group_by(Model) %>%
  summarise(Mean = mean(Value), SD = sd(Value)) %>%
  mutate(Metric = "PCC")

# 合并数据
combined_data <- bind_rows(r2_long, pcc_long)

combined_data <-combined_data[nrow(combined_data):1,]
model_colors <- c("RF" = "darkgreen", "XGBoost" = "royalblue3", "LightGBM" = "brown2", "NN" = "orange")


tt <- ggplot(combined_data, aes(x = Model, y = Mean, fill = Model)) +
  geom_bar(
    data = subset(combined_data, Metric == "R2"),
    stat = "identity",
    position = position_dodge(width = 0.8),
    color = "black", # R²柱子带黑色边框
    width = 0.4
  )  +
  geom_errorbar(
    data = subset(combined_data, Metric == "R2"),
    aes(ymin = Mean - SD, ymax = Mean + SD),
    position = position_dodge(width = 0.8),
    width = 0.2
  ) +
  scale_y_continuous(expand = c(0,0),breaks =seq(0,1,0.2),limits = c(0,1))+
  
  scale_fill_manual(values = model_colors) +
  theme(panel.grid.major=element_line(colour=NA),
        panel.background = element_rect(fill ="transparent",colour = NA),
        plot.background = element_rect(fill = "transparent",colour = NA),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(#face="bold",
          color="black", size=16),
        axis.text.y =element_text(#face="bold",
          color="black", size=16),plot.margin=unit(c(1,1,1,1),"cm"))+
  theme(axis.ticks.length.y = unit(0.4,"cm"))+    theme(axis.ticks.length.x = unit(0.4,"cm"))+
  theme(text=element_text(size=16,  family="serif"),panel.border = element_rect(fill=NA,color="black", size=1.5, linetype=NULL))+
  xlab(NULL)+ylab(NULL) + theme(legend.position  = "none")
pdf("Figure/Figure1C.pdf",width = 8,height = 8)
print(tt)

dev.off()


tt <- ggplot(combined_data, aes(x = Model, y = Mean, fill = Model)) +
  geom_bar(
    data = subset(combined_data, Metric == "PCC"),
    stat = "identity",
    position = position_dodge(width = 0.8),
    color = "black", # R²柱子带黑色边框
    width = 0.4
  )  +
  geom_errorbar(
    data = subset(combined_data, Metric == "PCC"),
    aes(ymin = Mean - SD, ymax = Mean + SD),
    position = position_dodge(width = 0.8),
    width = 0.2
  ) +
  scale_y_continuous(expand = c(0,0),breaks =seq(0,1,0.2),limits = c(0,1))+
  
  scale_fill_manual(values = model_colors) +
  theme(panel.grid.major=element_line(colour=NA),
        panel.background = element_rect(fill ="transparent",colour = NA),
        plot.background = element_rect(fill = "transparent",colour = NA),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(#face="bold",
          color="black", size=16),
        axis.text.y =element_text(#face="bold",
          color="black", size=16),plot.margin=unit(c(1,1,1,1),"cm"))+
  theme(axis.ticks.length.y = unit(0.4,"cm"))+    theme(axis.ticks.length.x = unit(0.4,"cm"))+
  theme(text=element_text(size=16,  family="serif"),panel.border = element_rect(fill=NA,color="black", size=1.5, linetype=NULL))+
  xlab(NULL)+ylab(NULL) + theme(legend.position  = "none")
pdf("Figure/Figure1D.pdf",width = 8,height = 8)
print(tt)

dev.off()



tt <- sv_importance(sv_reg, kind = "both") +
  theme_minimal()+
  # scale_x_continuous(expand = c(0,0),breaks  =seq(0,1,0.25),limits = c(0,1))+
  # scale_y_continuous(expand = c(0,0),breaks  =seq(0,1,0.25),limits = c(0,1))+
  theme_bw()+
  theme(panel.grid.major=element_line(colour=NA),
        panel.background = element_rect(fill ="transparent",colour = NA),
        plot.background = element_rect(fill = "transparent",colour = NA),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(#face="bold",
          color="black", size=16),
        axis.text.y =element_text(#face="bold",
          color="black", size=16),plot.margin=unit(c(1,1,1,1),"cm"))+
  theme(axis.ticks.length.y = unit(0.4,"cm"))+    theme(axis.ticks.length.x = unit(0.4,"cm"))+
  theme(text=element_text(size=16,  family="serif"),panel.border = element_rect(fill=NA,color="black", size=1.5, linetype="solid"))+
  xlab(NULL)+ylab(NULL) + theme(legend.title = element_blank(), legend.position = c(0.1, 0.25))
pdf("Figure/Figure1F.pdf",width = 8,height = 8)
print(tt)

dev.off()


x <- c("Density", "Humidity", "Precipitation", "Temperature")
tt <- sv_dependence(sv_reg, v = x,color_var=c("Humidity","Density","Density","Density"))
pdf("Figure/FigureS1B.pdf",width = 8,height = 8)
print(tt)

dev.off()


