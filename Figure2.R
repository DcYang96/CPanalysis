fn <- function(p, x){
  
  y =p[1]/(1 +  p[2]*exp(-(p[3]*x)) )
  
  return( y)
}




loss <- function(p,x,y){sum((y-fn(p,x))^2)}
load(file="res_pred_type.RData")
load(file="res_pred_rate.RData")

library(ggplot2)
################################Figure2A
  
  dat <- data.frame(x=1:nrow(res_pred_type),y=res_pred_type[,1])
  y <- dat[,2]
  x <- dat[,1]  
  
  theta <- c(max(y),
    max((y[-1]-y[-length(y)])/(x[-1]-x[-length(x)])),
    x[which.max(((y[-1]-y[-length(y)])/(x[-1]-x[-length(x)])))]-y[which.max(((y[-1]-y[-length(y)])/(x[-1]-x[-length(x)])))]/max((y[-1]-y[-length(y)])/(x[-1]-x[-length(x)])))  
  # theta <- c(1.2,0.8,5)
  fan <- optim(theta,loss, x=x, y=y)

  meancure <- data.frame(x=c(1:nrow(res_pred_type)),y=fn(x=c(1:nrow(res_pred_type)),p=fan$par))
  
  fan2 <- optim(par = c(8),loss, p= as.numeric(fan$par), y=c(0.5),lower=0,upper=250,method = "Brent")
  fan3 <- optim(par = c(8),loss, p= as.numeric(fan$par), y=c(fan$par)[1]*0.99,lower=0,upper=250,method = "Brent")
  
  Ti=log(fan$par[2])/fan$par[3]
  
  ff <- ggplot()
  ff <- ff+geom_point(data=dat,aes(x,y),size=4,shape=1,col="brown2")
 ff
  ff <- ff+geom_line(data=meancure,aes(x,y),size=1.5,col="darkgreen")
  ff <- ff+xlab(NULL)+ylab(NULL)
  ff <- ff + annotate("segment",x=Ti,xend = Ti,y=0,yend = fn(x=Ti,p=fan$par),linetype="dashed",colour="black",size=1.5)
 # ff <- ff + annotate("segment",x=fan2$par,xend = fan2$par,y=0,yend = fn(x=fan2$par,p=fan$par),linetype="dashed",colour="black",size=1.5)
  ff <- ff + annotate("segment",x=0,xend = 250,y=0.5,yend = 0.5,linetype="dashed",colour="black",size=1.5)
  ff <- ff + annotate("segment",x=fan3$par,xend =fan3$par,y=0,yend = fn(x=fan3$par,p=fan$par),linetype="dashed",colour="black",size=1.5)
  
   ff <- ff+geom_point(data=dat,aes(x=fan2$par,y=0.5),size=3,color="royalblue3")
  ff <- ff+geom_point(data=dat,aes(x=Ti,y=fn(x=Ti,p=fan$par)),size=3,color="royalblue3")
  ff <- ff+geom_point(data=dat,aes(x=fan3$par,y=fn(x=fan3$par,p=fan$par)),size=3,color="royalblue3")
  
    ff <- ff +scale_x_continuous(expand = c(0, 0),breaks=sort(c(seq(0,250,50)),F),labels=c(0,50,100,150,200,250),limits = c(0,250))#breaks = seq(0,16,by = 4),labels = seq(0,0.16,by = 0.04)


  
  
  
  
    ff <- ff +scale_y_continuous(expand = c(0, 0),limits = c(0,1))
    ff <-ff+ theme_bw() +
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
    
ff  
 


pdf("Figure4A.pdf",width = 8,height = 5)
print(ff)

dev.off()















################

dat <- data.frame(x=1:nrow(res_pred_type),y=res_pred_rate)
y <- dat[,2]
x <- dat[,1]  

theta <- c(max(y),
  max((y[-1]-y[-length(y)])/(x[-1]-x[-length(x)])),
  x[which.max(((y[-1]-y[-length(y)])/(x[-1]-x[-length(x)])))]-y[which.max(((y[-1]-y[-length(y)])/(x[-1]-x[-length(x)])))]/max((y[-1]-y[-length(y)])/(x[-1]-x[-length(x)])))  
# theta <- c(1.2,0.8,5)
fan <- optim(theta,loss, x=x, y=y)

meancure <- data.frame(x=c(1:nrow(res_pred_type)),y=fn(x=c(1:nrow(res_pred_type)),p=fan$par))

fan3 <- optim(par = c(8),loss, p= as.numeric(fan$par), y=c(fan$par)[1]*0.99,lower=0,upper=250,method = "Brent")

Ti=log(fan$par[2])/fan$par[3]

ff <- ggplot()
ff <- ff+geom_point(data=dat,aes(x,y),size=4,shape=1,col="brown2")
ff
ff <- ff+geom_line(data=meancure,aes(x,y),size=1.5,col="darkgreen")
ff <- ff+xlab(NULL)+ylab(NULL)
ff <- ff + annotate("segment",x=Ti,xend = Ti,y=0,yend = fn(x=Ti,p=fan$par),linetype="dashed",colour="black",size=1.5)
# ff <- ff + annotate("segment",x=fan2$par,xend = fan2$par,y=0,yend = fn(x=fan2$par,p=fan$par),linetype="dashed",colour="black",size=1.5)
ff <- ff + annotate("segment",x=fan3$par,xend =fan3$par,y=0,yend = fn(x=fan3$par,p=fan$par),linetype="dashed",colour="black",size=1.5)

ff <- ff+geom_point(data=dat,aes(x=Ti,y=fn(x=Ti,p=fan$par)),size=3,color="royalblue3")
ff <- ff+geom_point(data=dat,aes(x=fan3$par,y=fn(x=fan3$par,p=fan$par)),size=3,color="royalblue3")

ff <- ff +scale_x_continuous(expand = c(0, 0),breaks=sort(c(seq(0,250,50)),F),labels=c(0,50,100,150,200,250),limits = c(0,250))#breaks = seq(0,16,by = 4),labels = seq(0,0.16,by = 0.04)




ff <- ff +scale_y_continuous(expand = c(0, 0),limits = c(0,0.16))
ff <-ff+ theme_bw() +
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

ff  
pdf("Figure4B.pdf",width = 8,height = 5)
print(ff)

dev.off()
