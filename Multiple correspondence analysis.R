library("FactoMineR") 
library("factoextra")

rm(list = ls())


library(readxl)
dat <- read_excel("global_data.xlsx")
dat <- as.data.frame(dat)


dat_type <- sapply(1:nrow(dat),function(c)
  paste(strsplit( dat$Subgenotype[c],""  )[[1]][1:3],sep="",collapse=""))

library(scatterplot3d)

new_dat <- data.frame(type=dat_type,density=dat$`Dairy cattle stocking density`,Humidity=dat$`Relative Humidity`,
  Precipitation=dat$`Annual Precipitation`,temperature=dat$temperature)
new_dat$density <- cut(dat$`Dairy cattle stocking density`,include.lowest=T,quantile(dat$`Dairy cattle stocking density`,c(0,0.33,0.66,1)),labels=c("L_DSD","M_DSD","H_DSD"))

new_dat$Humidity <- cut(dat$`Relative Humidity`,include.lowest=T,quantile(dat$`Relative Humidity`,c(0,0.33,0.66,1)),labels=c("L_H","M_H","H_H"))

new_dat$Precipitation <- cut(dat$`Annual Precipitation`,include.lowest=T,quantile(dat$`Annual Precipitation`,c(0,0.33,0.66,1)),labels=c("L_P","M_P","H_P"))

new_dat$temperature <- cut(dat$`temperature`,include.lowest=T,quantile(dat$`temperature`,c(0,0.33,0.66,1)),labels=c("L_T","M_T","H_T"))
new_dat$rate <- cut(dat$`Base mutation rate`,include.lowest=T,quantile(dat$`Base mutation rate`,c(0,0.33,0.66,1)),labels=c("L_BMR","M_BMR","H_BMR"))

summary(new_dat)
res.mca <- MCA(new_dat[,c(1:4,6)], graph = T)
tt <- fviz_mca_var(res.mca, 
  repel = TRUE,  ggtheme = theme_minimal() )+ theme_bw() +
  theme(panel.grid.major=element_line(colour=NA),
    panel.background = element_rect(fill ="transparent",colour = NA),
    plot.background = element_rect(fill = "transparent",colour = NA),
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(#face="bold",
      color="black", size=16),
    axis.text.y =element_text(#face="bold",
      color="black", size=16),plot.margin=unit(c(1,1,1,1),"cm"))+
  theme(axis.ticks.length.y = unit(0.2,"cm"))+    theme(axis.ticks.length.x = unit(0.2,"cm"))+
  theme(text=element_text(size=16,  family="serif"),panel.border = element_rect(fill=NA,color="black", size=1.5, linetype="solid"))

pdf("Figure2A.pdf",width = 8,height = 5)
print(tt)

dev.off()




grp <- as.factor(new_dat[, "type"])
tt <- fviz_mca_ind(res.mca,  habillage = grp,
  addEllipses = TRUE, repel = TRUE,label="none")+ theme_bw() +
  theme(panel.grid.major=element_line(colour=NA),
    panel.background = element_rect(fill ="transparent",colour = NA),
    plot.background = element_rect(fill = "transparent",colour = NA),
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(#face="bold",
      color="black", size=16),
    axis.text.y =element_text(#face="bold",
      color="black", size=16),plot.margin=unit(c(1,1,1,1),"cm"))+
  theme(axis.ticks.length.y = unit(0.2,"cm"))+    theme(axis.ticks.length.x = unit(0.2,"cm"))+
  theme(text=element_text(size=16,  family="serif"),panel.border = element_rect(fill=NA,color="black", size=1.5, linetype="solid"))

pdf("Figure2B.pdf",width = 6,height = 6)
print(tt)

dev.off()


grp <- as.factor(new_dat[, "rate"])
tt <- fviz_mca_ind(res.mca,  habillage = grp,
  addEllipses = TRUE, repel = TRUE,label="none")+ theme_bw() +
  theme(panel.grid.major=element_line(colour=NA),
    panel.background = element_rect(fill ="transparent",colour = NA),
    plot.background = element_rect(fill = "transparent",colour = NA),
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(#face="bold",
      color="black", size=16),
    axis.text.y =element_text(#face="bold",
      color="black", size=16),plot.margin=unit(c(1,1,1,1),"cm"))+
  theme(axis.ticks.length.y = unit(0.2,"cm"))+    theme(axis.ticks.length.x = unit(0.2,"cm"))+
  theme(text=element_text(size=16,  family="serif"),panel.border = element_rect(fill=NA,color="black", size=1.5, linetype="solid"))

pdf("Figure2C.pdf",width = 6,height = 6)
print(tt)

dev.off()



library(readxl)
library(NbClust)
library(psych)
dat1 <- read_excel("Chinese_data.xlsx",sheet=1)
dat2 <- read_excel("Chinese_data.xlsx",sheet=2)
dat1 <- as.data.frame(dat1)
dat2 <- as.data.frame(dat2)

row.names(dat1 )<- dat1[,1]
row.names(dat2 )<- dat1[,1]
dat1 <- dat1[,-1]


dat2 <- dat2[,-1]

p1 <- c(2,6,7,11,12,13,15,16,17,19,21,27,28,30,32,34,35,38)
p2 <- c(1:10)
l1 <- c(11,27,9,21,22,12,26,24,18,2,5,10,15,14,3,4,8,7,1,17,28,20,6,16,19,31,13,25,30,29,23)
l2 <- c(17,24,1,18,4,6,11,5,26,8,21,13,23,20,2,3,12,15,14,16,30,28,7,27,19,31,10,25,29,22,9)



dat1 <- dat1[,p1]
sat1 <- scale(dat1)
dat2 <- dat2[,p2]
sat2 <- scale(dat2)




fit1 <- hclust(dist(sat1),method = "average")
fit2 <- hclust(dist(sat2),method = "average")


tt <-fviz_dend(fit1,k=4,cex=1.2,rect = T,lwd=1,color_labels_by_k = T,main = NULL,  ggtheme = theme_minimal() )+
  scale_x_continuous(breaks  =NULL)+scale_y_continuous(breaks  =NULL)+xlab(NULL)+ylab(NULL)+ theme_bw() +
  theme(panel.grid.major=element_line(colour=NA),
    panel.background = element_rect(fill ="transparent",colour = NA),
    plot.background = element_rect(fill = "transparent",colour = NA),
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(#face="bold",
      color="black", size=16),
    axis.text.y =element_text(#face="bold",
      color="black", size=16),plot.margin=unit(c(1,1,1,1),"cm"))+
  theme(axis.ticks.length.y = unit(0.2,"cm"))+    theme(axis.ticks.length.x = unit(0.2,"cm"))+
  theme(text=element_text(size=16,  family="serif"),panel.border = element_rect(fill=NA,color="transparent", size=0, linetype=NULL))

pdf("Figure3c.pdf",width = 9,height = 9)
print(tt)

dev.off()
tt <-fviz_dend(fit2,k=3,cex=1.2,rect = T,lwd=1,color_labels_by_k = T,main = NULL,  ggtheme = theme_minimal() )+
  scale_x_continuous(breaks  =NULL)+scale_y_continuous(breaks  =NULL)+xlab(NULL)+ylab(NULL)+ theme_bw() +
  theme(panel.grid.major=element_line(colour=NA),
    panel.background = element_rect(fill ="transparent",colour = NA),
    plot.background = element_rect(fill = "transparent",colour = NA),
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(#face="bold",
      color="black", size=16),
    axis.text.y =element_text(#face="bold",
      color="black", size=16),plot.margin=unit(c(1,1,1,1),"cm"))+
  theme(axis.ticks.length.y = unit(0.2,"cm"))+    theme(axis.ticks.length.x = unit(0.2,"cm"))+
  theme(text=element_text(size=16,  family="serif"),panel.border = element_rect(fill=NA,color="black", size=0, linetype=NULL))
pdf("Figure3d.pdf",width = 9,height = 9)
print(tt)

dev.off()


clusters1<-cutree(fit1,k=4)
clusters2<-cutree(fit2,k=3)
get_cluster_res <- function(clusters1,l1){
  cl1 <- rep(NA,length(clusters1))
  for (i in 1:length(table(clusters1))) {
    kk <-   order(as.numeric(sapply(1:length(table(clusters1)),function(c)mean(l1[which(as.numeric(clusters1)==c)]))),decreasing = F)
    cl1[which(clusters1==kk[i])]=i
  }
  return(cl1)}

cl1 <- get_cluster_res(clusters1,l1)
cl2 <- get_cluster_res(clusters2,l2)






dat <- read_excel("chinese_data.xlsx",sheet=3)
dat <- as.data.frame(dat)



dat[which(dat[,2]=="华东"),2]="CN_E"
dat[which(dat[,2]=="东北"),2]="CN_NE"
dat[which(dat[,2]=="华北"),2]="CN_N"
dat[which(dat[,2]=="华中"),2]="CN_C"
dat[which(dat[,2]=="华南"),2]="CN_S"
dat[which(dat[,2]=="西北"),2]="CN_NW"
dat[which(dat[,2]=="西南"),2]="CN_SW"
n1 <- c("TC_1C","TC_2C","TC_3C","TC_4C","TC_5C")
n2 <- c("H_IRD","M_IRD","L_IRD")

rdat<- c()
for (i in 1:15) {
  k=rowSums(dat[i,3:6])
  city1 <- matrix(NA,nrow = k,ncol = 5)
  city1[,1] <- dat[i,1]
  city1[,2] <- dat[i,2]
  city1[,4] <- n1[cl1[which(names(clusters1)==dat[i,1])]]
  city1[,5] <- n2[cl2[which(names(clusters2)==dat[i,1])]]
  
  
  city1[,3] <- c(rep("IIdA14G1",dat[i,3]),rep("IIdA15G1",dat[i,4]),rep("IIdA19G1",dat[i,5]),rep("IIdA20G1",dat[i,6]))
  
  rdat<- rbind(rdat,city1)
}
rdat <- as.data.frame(rdat)

colnames(rdat) <- c("City","Reigon","Type","Transportation capacity","Intensive rearing degree")
library("FactoMineR") 
library("factoextra")
res.mca <- MCA(rdat[,c(2:5)], graph = F)
tt <- fviz_mca_var(res.mca, 
  repel = TRUE,shape.var =1:4 ,ggtheme = theme_minimal() )+ theme_bw() +
  theme(panel.grid.major=element_line(colour=NA),
    panel.background = element_rect(fill ="transparent",colour = NA),
    plot.background = element_rect(fill = "transparent",colour = NA),
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(#face="bold",
      color="black", size=16),
    axis.text.y =element_text(#face="bold",
      color="black", size=16),plot.margin=unit(c(1,1,1,1),"cm"))+
  theme(axis.ticks.length.y = unit(0.2,"cm"))+    theme(axis.ticks.length.x = unit(0.2,"cm"))+
  theme(text=element_text(size=16,  family="serif"),panel.border = element_rect(fill=NA,color="black", size=1.5, linetype="solid"))
tt

pdf("Figure3A.pdf",width = 6,height = 6)
print(tt)

dev.off()

tt


fviz_mca_var(res.mca, choice = "mca.cor",repel = TRUE)
fviz_ellipses(res.mca, c("Type"), geom = "point")
grp <- as.factor(rdat[, "Type"])
tt <- fviz_mca_ind(res.mca,  habillage = grp,
  addEllipses = F, repel = TRUE,label="none")+ theme_bw() +
  theme(panel.grid.major=element_line(colour=NA),
    panel.background = element_rect(fill ="transparent",colour = NA),
    plot.background = element_rect(fill = "transparent",colour = NA),
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(#face="bold",
      color="black", size=16),
    axis.text.y =element_text(#face="bold",
      color="black", size=16),plot.margin=unit(c(1,1,1,1),"cm"))+
  theme(axis.ticks.length.y = unit(0.2,"cm"))+    theme(axis.ticks.length.x = unit(0.2,"cm"))+
  theme(text=element_text(size=16,  family="serif"),panel.border = element_rect(fill=NA,color="black", size=1.5, linetype="solid"))
tt

pdf("Figure3b.pdf",width = 7,height = 6)
print(tt)

dev.off()

tt











nc<-NbClust(as.data.frame(sat1),distance = "euclidean",min.nc = 2,max.nc = 10,
  method = "average")
nc1 <- table(nc$Best.n[1,])
nc<-NbClust(as.data.frame(sat2),distance = "euclidean",min.nc = 2,max.nc = 10,
  method = "average")
nc2 <- table(nc$Best.n[1,])

dat <- data.frame(x=2:10,y1=c(6,4,6,0,4,0,0,1,3),y2=c(5,6,5,0,0,2,0,3,2))
dat2 <- data.frame(x=2:10,y1=c(0,0,6,0,0,0,0,0,0),y2=c(0,6,0,0,0,0,0,0,0))

tt <- ggplot()

tt <-tt + geom_bar(data = dat ,mapping=aes(dat$x,dat$y1),stat = "identity",col="royalblue3",fill="#87CEFA",width = 1)#+geom_text(data=dat1,aes(label=z),vjust=1.5)

tt <-tt + geom_bar(data = dat ,mapping=aes(dat$x,-dat$y2),stat = "identity",col="brown2",fill="#FFDAB9",width = 1)
tt <-tt + geom_bar(data = dat2 ,mapping=aes(x,y1),stat = "identity",col="royalblue3",fill="palegreen3",width = 1)#+geom_text(data=dat1,aes(label=z),vjust=1.5)

tt <-tt + geom_bar(data = dat2 ,mapping=aes(x,-y2),stat = "identity",col="brown2",fill="palegreen3",width = 1)

tt <- tt+xlab(NULL)+ylab(NULL)

tt <-tt +scale_x_continuous(expand = c(0,0),breaks = 2:10)+scale_y_continuous(expand = c(0,0),breaks =seq(-6,6,2), labels =abs(seq(-6,6,2)) ,limits = c(-6,6))

tt <- tt+ theme_bw() +
  theme(panel.grid.major=element_line(colour=NA),
    panel.background = element_rect(fill = "transparent",colour = NA),
    plot.background = element_rect(fill = "transparent",colour = NA),
    panel.grid.minor = element_blank()
  )
tt


tt1 <- tt+ theme_bw() +
  theme(panel.grid.major=element_line(colour=NA),
    panel.background = element_rect(fill ="transparent",colour = NA),
    plot.background = element_rect(fill = "transparent",colour = NA),
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(#face="bold",
      color="black", size=20),
    axis.text.y =element_text(#face="bold",
      color="black", size=20),plot.margin=unit(c(1,1,1,1),"cm"))+
  theme(axis.ticks.length.y = unit(0.4,"cm"))+    theme(axis.ticks.length.x = unit(0.4,"cm"))+
  theme(text=element_text(size=16,  family="serif"),panel.border = element_rect(fill=NA,color="black", size=1.5, linetype="solid"))

tt1
pdf("Figure S1.pdf",width = 9,height = 6)
print(tt1)

dev.off()

dat1 <- matrix(NA,nrow = 7,ncol = 6)
for (i in 1:7) {
  dat1[i,] <-  colSums( dat[which(dat[,1]==unique(dat[,1])[i]),-1])
  
}
row.names(dat1 ) <- unique(dat[,1])
colnames(dat1) <- colnames(dat)[-1]

colSums(dat1)
dat2
dat2 <-ca(dat1)
dat3<-cacoord(dat2,type="principal",dim=1:2)
dat3 <- cbind(rbind((dat3[[1]]),dat3[[2]]),c(rep("Geographic area",nrow(dat3[[1]])),rep("Genotype",nrow(dat3[[2]]))))
colnames(dat3) <- c("x","y","Type") 
dat3 <- as.data.frame(dat3)
dat3[,1] <- as.numeric(dat3[,1])
dat3[,2] <- as.numeric(dat3[,2])
plot(ca(dat1))
ggplot(dat3, aes(x =dat3$x , y = dat3$y,color=dat3$Type)) +
  geom_point() +
  stat_ellipse(level = 0.9)


