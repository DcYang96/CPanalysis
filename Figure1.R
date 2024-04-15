library(ggplot2)
library(pROC)
load(file="res_imp.RData")
load(file="oob.error.data.RData")
load(file="roc.RData")
load(file="result.RData")
load(file="error.cv.RData")



dat <- res_imp[,3:4]
dat <- cbind(dat,name=c(4,3,2,1))
tt <- ggplot()

tt <-tt + geom_bar(data = dat ,mapping=aes(dat$name,dat$MeanDecreaseAccuracy),stat = "identity",col="royalblue3",fill="#87CEFA",width = 1)#+geom_text(data=dat1,aes(label=z),vjust=1.5)

tt <-tt + geom_bar(data = dat ,mapping=aes(dat$name,-dat$MeanDecreaseGini),stat = "identity",col="brown2",fill="#FFDAB9",width = 1)
tt <- tt+xlab(NULL)+ylab(NULL)+coord_flip()

tt <-tt +scale_x_continuous(expand = c(0,0),breaks  =NULL)+scale_y_continuous(expand = c(0,0),breaks =seq(-50,50,10), labels =abs(seq(-50,50,10)) ,limits = c(-50,50))

tt <- tt+ theme_bw() +
  theme(panel.grid.major=element_line(colour=NA),
    panel.background = element_rect(fill = "transparent",colour = NA),
    plot.background = element_rect(fill = "transparent",colour = NA),
    panel.grid.minor = element_blank()
  )+coord_flip()

tt <-tt +annotate("text",x=4,y=0,label="Dairy cattle stocking density",size=7)
tt <-tt +annotate("text",x=3,y=0,label="Relative Humidity",size=7)
tt <-tt +annotate("text",x=2,y=0,label="Annual Precipitation",size=7)
tt <-tt +annotate("text",x=1,y=0,label="Annual Temperature",size=7)

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

pdf("Figure1B.pdf",width = 10,height = 5)
print(tt1)

dev.off()





#####################################################################
tt2 <- ggplot(data=oob.error.data, aes(x=Trees, y=Error)) +
  geom_line(aes(color=Type),linewidth=1)+ theme_bw() +
  theme(panel.grid.major=element_line(colour=NA),
    panel.background = element_rect(fill ="transparent",colour = NA),
    plot.background = element_rect(fill = "transparent",colour = NA),
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(#face="bold",
      color="black", size=20),
    axis.text.y =element_text(#face="bold",
      color="black", size=20),plot.margin=unit(c(1,1,1,1),"cm"))+
  theme(axis.ticks.length.y = unit(0.4,"cm"))+    theme(axis.ticks.length.x = unit(0.4,"cm"))+
  theme(text=element_text(size=16,  family="serif"),panel.border = element_rect(fill=NA,color="black", size=1.5, linetype="solid"))+xlab(NULL)+ylab(NULL)
pdf("Figure1A.pdf",width = 8,height = 6)
print(tt2)

dev.off()




tt <- ggroc(roc, 
  legacy.axes = TRUE ,linetype=1,col="darkgreen",alpha = 0.5,linewidth=1.5)+
  geom_segment(aes(x = 0, xend = 1, y = 0, yend = 1), 
    color="black", linetype=2,linewidth=1.5)+
  theme_bw()+annotate(geom = "text",x=0.75,y=0.4,label="AUC=0.994",size=6)+
  theme(panel.grid.major=element_line(colour=NA),
    panel.background = element_rect(fill ="transparent",colour = NA),
    plot.background = element_rect(fill = "transparent",colour = NA),
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(#face="bold",
      color="black", size=16),
    axis.text.y =element_text(#face="bold",
      color="black", size=16),plot.margin=unit(c(1,1,1,1),"cm"))+
  theme(axis.ticks.length.y = unit(0.4,"cm"))+    theme(axis.ticks.length.x = unit(0.4,"cm"))+
  theme(text=element_text(size=16,  family="serif"),panel.border = element_rect(fill=NA,color="black", size=1.5, linetype="solid"))+xlab(NULL)+ylab(NULL)
pdf("Figure1c.pdf",width = 6,height = 5)
print(tt)

dev.off()



res_cv <- data.frame(sapply(result, '[[', 'error.cv'))
res_cv$res <- rownames(res_cv)
res_cv <- reshape2::melt(res_cv, id = 'res')
res_cv$res <- as.numeric(as.character(res_cv$res))
res_cv.mean <- aggregate(res_cv$value, by = list(res_cv$res), FUN = mean)



tt <- ggplot()
for (i in 1:10) {
  dat <- data.frame(x=1:4,y=error.cv[4:1,i])
  tt <- tt+ geom_line(dat,mapping = aes(x,y),color="darkgrey",linetype=2,linewidth=0.6)
}
dat <- data.frame(x=1:4,y=res_cv.mean[,2])
tt <- tt+ geom_line(dat,mapping = aes(x,y),color="brown2",linetype=1,linewidth=1)
tt <- tt+ geom_point(dat,mapping = aes(x,y),color="brown2",size=3)
tt<-tt+ theme_bw() +
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
tt <- tt+xlab(NULL)+ylab(NULL)

tt

pdf("Figure1d.pdf",width = 7,height = 4)
tt

dev.off()
