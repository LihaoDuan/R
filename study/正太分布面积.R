##获得密度曲线XY值
value30 <- density(sample1000_30$weight)
value29 <- density(sample1000_29$weight)
plot(value30)
plot(value29,xlim=c(22.5,27), xaxt="n")
Axis(side=1,at=seq(22.5, 27, by = .5))
Axis(side=2, labels=FALSE)
lines(value30,col="blue",lwd=1, ylim=c(0,1))

##求一个标准差的值域
xbegin <- mean(sample1000_30$weight)-sd(sample1000_30$weight)
xend <- mean(sample1000_30$weight)+sd(sample1000_30$weight)

range(sample1000_30$weight)

##求1个标准差的值域
poly_range <- value30$x >= xbegin & value30$x < xend
sum(poly_range)

polygon(c(xbegin, value30$x[poly_range], xend),                # X-Coordinates of polygon range
        c(0, value30$y[poly_range], 0),   density = 10, angle = 45,              # Y-Coordinates of polygon range
        col = "#353436",border = "black",lwd=.5)  



##求2个标准差的值域
xbegin1 <- mean(sample1000_30$weight)-2*sd(sample1000_30$weight)
xend1 <- mean(sample1000_30$weight)+2*sd(sample1000_30$weight)

range(sample1000_30$weight)


poly_range1 <- value30$x >= xbegin1 & value30$x < xend1
sum(poly_range1)

polygon(c(xbegin1, value30$x[poly_range1], xend1),                # X-Coordinates of polygon range
        c(0, value30$y[poly_range1], 0),   density = 10, angle = 45,              # Y-Coordinates of polygon range
        col = "blue",border = "black",lwd=.5)  



##29个一袋样本统计量

##求2个标准差的值域
xbegin2 <- mean(sample1000_29$weight)-2*sd(sample1000_29$weight)
xend2 <- mean(sample1000_29$weight)+2*sd(sample1000_29$weight)

range(sample1000_29$weight)


poly_range2 <- value29$x >= xbegin2 & value29$x < xend2
sum(poly_range2)

polygon(c(xbegin2, value29$x[poly_range2], xend2),                # X-Coordinates of polygon range
        c(0, value29$y[poly_range2], 0),   density = 10, angle = -45,              # Y-Coordinates of polygon range
        col = "red",border = "black",lwd=.5)  


d


##抽样，样本容量10，replace为T表示采取有重复的抽样，F不重复抽样，即无放回抽样
bag <- sample(kj$weight,10,F)

##构建样本
set.seed(1234)
sample1000_10<-data.frame(time=1:1000,mean=0,var=0,weight=0)
View(sample1000_10)

for (i in 1:1000){
  sample1000_10$mean[i] <- mean(sample(kj$weight,10,F))
  sample1000_10$var[i] <- var(sample(kj$weight,10,F))
  sample1000_10$weight[i] <- sum(sample(kj$weight,10,F))
}


sample1000_9<-data.frame(time=1:1000,mean=0,var=0,weight=0)
View(sample1000_9)

for (i in 1:1000){
  sample1000_9$mean[i] <- mean(sample(kj$weight,9,F))
  sample1000_9$var[i] <- var(sample(kj$weight,9,F))
  sample1000_9$weight[i] <- sum(sample(kj$weight,9,F))
}

sample1000_8<-data.frame(time=1:1000,mean=0,var=0,weight=0)
View(sample1000_8)

for (i in 1:1000){
  sample1000_8$mean[i] <- mean(sample(kj$weight,8,F))
  sample1000_8$var[i] <- var(sample(kj$weight,8,F))
  sample1000_8$weight[i] <- sum(sample(kj$weight,8,F))
}

sample1000_7<-data.frame(time=1:1000,mean=0,var=0,weight=0)

for (i in 1:1000){
  sample1000_7$mean[i] <- mean(sample(kj$weight,7,F))
  sample1000_7$var[i] <- var(sample(kj$weight,7,F))
  sample1000_7$weight[i] <- sum(sample(kj$weight,7,F))
}

sample1000_6<-data.frame(time=1:1000,mean=0,var=0,weight=0)

for (i in 1:1000){
  sample1000_6$mean[i] <- mean(sample(kj$weight,6,F))
  sample1000_6$var[i] <- var(sample(kj$weight,6,F))
  sample1000_6$weight[i] <- sum(sample(kj$weight,6,F))
}

sample1000_5<-data.frame(time=1:1000,mean=0,var=0,weight=0)

for (i in 1:1000){
  sample1000_5$mean[i] <- mean(sample(kj$weight,5,F))
  sample1000_5$var[i] <- var(sample(kj$weight,5,F))
  sample1000_5$weight[i] <- sum(sample(kj$weight,5,F))
}

sample1000_4<-data.frame(time=1:1000,mean=0,var=0,weight=0)

for (i in 1:1000){
  sample1000_4$mean[i] <- mean(sample(kj$weight,4,F))
  sample1000_4$var[i] <- var(sample(kj$weight,4,F))
  sample1000_4$weight[i] <- sum(sample(kj$weight,4,F))
}

##求2个标准差的值域
xbegin3 <- mean(sample1000_10$weight)-2*sd(sample1000_10$weight)
xend3 <- mean(sample1000_10$weight)+2*sd(sample1000_10$weight)

poly_range3 <- value10$x >= xbegin3 & value10$x < xend3

value10 <- density(sample1000_10$weight)
value9 <- density(sample1000_9$weight)
value8 <- density(sample1000_8$weight)
value7 <- density(sample1000_7$weight)
value6 <- density(sample1000_6$weight)
value5 <- density(sample1000_5$weight)
value4 <- density(sample1000_4$weight)

plot(value10,xlim=c(6.5,10))
lines(value9,col="blue",lwd=1, ylim=c(0,1))

##10个一袋样本2个标准差阴影
polygon(c(xbegin3, value10$x[poly_range3], xend3),                # X-Coordinates of polygon range
        c(0, value10$y[poly_range3], 0),   density = 10, angle = -45,              # Y-Coordinates of polygon range
        col = "red",border = "black",lwd=.5)  


##9个一袋样本2个标准差阴影
xbegin4 <- mean(sample1000_9$weight)-2*sd(sample1000_9$weight)
xend4 <- mean(sample1000_9$weight)+2*sd(sample1000_9$weight)

poly_range4 <- value9$x >= xbegin4 & value9$x <= xend4
polygon(c(xbegin4, value9$x[poly_range4], xend4),                # X-Coordinates of polygon range
        c(0, value9$y[poly_range4], 0),   density = 10, angle = 45,              # Y-Coordinates of polygon range
        col = "blue",border = "black",lwd=.5)  

##8个一袋样本2个标准差阴影
xbegin5 <- mean(sample1000_8$weight)-2*sd(sample1000_8$weight)
xend5 <- mean(sample1000_8$weight)+2*sd(sample1000_8$weight)

poly_range5 <- value8$x >= xbegin5 & value8$x <= xend5
polygon(c(xbegin5, value8$x[poly_range5], xend5),                # X-Coordinates of polygon range
        c(0, value8$y[poly_range5], 0),   density = 10, angle = 45,              # Y-Coordinates of polygon range
        col = "blue",border = "black",lwd=.5)  

plot(value8,xlim=c(5.5,9))
lines(value9,col="blue",lwd=1, ylim=c(0,1))


##7个一袋样本2个标准差阴影
xbegin6 <- mean(sample1000_7$weight)-2*sd(sample1000_7$weight)
xend6 <- mean(sample1000_7$weight)+2*sd(sample1000_7$weight)

poly_range6 <- value7$x >= xbegin6 & value7$x <= xend6
polygon(c(xbegin6, value7$x[poly_range6], xend6),                # X-Coordinates of polygon range
        c(0, value7$y[poly_range6], 0),   density = 10, angle = 45,              # Y-Coordinates of polygon range
        col = "BLACK",border = "black",lwd=.5)  

plot(value7,xlim=c(4.5,8))
lines(value8,col="blue",lwd=1, ylim=c(0,1))

##6个一袋样本2个标准差阴影
xbegin7 <- mean(sample1000_6$weight)-2*sd(sample1000_6$weight)
xend7 <- mean(sample1000_6$weight)+2*sd(sample1000_6$weight)

poly_range7 <- value6$x >= xbegin7 & value6$x <= xend7
polygon(c(xbegin7, value6$x[poly_range7], xend7),                # X-Coordinates of polygon range
        c(0, value6$y[poly_range7], 0),   density = 10, angle = -45,              # Y-Coordinates of polygon range
        col = "blue",border = "black",lwd=.5)  

plot(value6,xlim=c(3.5,8))
lines(value7,col="blue",lwd=1, ylim=c(0,1))

##5个一袋样本2个标准差阴影
xbegin8 <- mean(sample1000_5$weight)-2*sd(sample1000_5$weight)
xend8 <- mean(sample1000_5$weight)+2*sd(sample1000_5$weight)

poly_range8 <- value5$x >= xbegin8 & value5$x <= xend8
polygon(c(xbegin8, value5$x[poly_range8], xend8),                # X-Coordinates of polygon range
        c(0, value5$y[poly_range8], 0),   density = 10, angle = 45,              # Y-Coordinates of polygon range
        col = "RED",border = "black",lwd=.5)  

plot(value6,xlim=c(3.5,8))
lines(value7,col="blue",lwd=1, ylim=c(0,1))

##4个一袋样本2个标准差阴影
xbegin9 <- mean(sample1000_4$weight)-2*sd(sample1000_4$weight)
xend9 <- mean(sample1000_4$weight)+2*sd(sample1000_4$weight)

poly_range9 <- value5$x >= xbegin9 & value4$x <= xend9
polygon(c(xbegin9, value4$x[poly_range9], xend9),                # X-Coordinates of polygon range
        c(0, value4$y[poly_range9], 0),   density = 10, angle = -45,              # Y-Coordinates of polygon range
        col = "GREEN",border = "black",lwd=.5)  

plot(value4,xlim=c(2.9,7))
lines(value7,col="BLACK",lwd=1, ylim=c(0,1))



m <- seq(-3, 3,0.1)
n <- dnorm(m,0,1)
plot(x = m,
     y = dnorm(m,0,1),
     type = "l",                            # Specify that you want to plot a line graph
     lwd = 2,                               # Thickness of line
     lty = 1,
     col = "black",                          # Colour of line
     xlim=c(-3,3),                          # Set limit of x-axis
     ylim=c(0,0.4),                         # Set limit of y-axis
     frame.plot=TRUE,                       # Do plot the frame of the graph
     xlab=" ",                    # Title for x-axis
     ylab= " ",                             # Title for y-axis
     axes = FALSE,                          # Don't plot the scales by default
     main=paste(" "))     # Main title
title(main="Area Under a Normal Distribution", cex.main = 2.5, line=0.5, cex.lab=2) # "line" adjusts distance to axis, cex.lab adjusts size
title(xlab="Standard Deviation", cex.lab = 1.5, line=3) #  # "line" adj
Axis(side=1,at=seq(22.5, 27, by = .5))
Axis(side=2, labels=FALSE)

# Create data for the area to shade +-3SD
cord.1x <- c(-1,seq(-1,1,0.01),1) 
cord.1y <- c(0,dnorm(seq(-1,1,0.01)),0) 
# Make a curve
#curve(dnorm(x,0,1), xlim=c(-3,3), main='Standard Normal',lwd=2) 
# Add the shaded area.
polygon(cord.1x,cord.1y,col='grey60')
text(2,0.25,paste("68%"),cex=4,col="grey60")
#dev.off()
dnorm(seq(-1,1,0.01))




# draw the normal curve
curve(dnorm(x,0,1), xlim=c(-3,3), main="Normal density")

# define shaded region
from.z <- -3
to.z <- qnorm(.025)

S.x  <- c(from.z, seq(from.z, to.z, 0.01), to.z)
S.y  <- c(0, dnorm(seq(from.z, to.z, 0.01)), 0)
polygon(S.x,S.y, col="red")

plot(1, 1, col = "white", xlab = "X", ylab = "Y")


library(ggplot2)

plot_area <- function(min,max){
  function(x){
    y <- dnorm(x)
    y[x<min | x>max] <-NA
    return(y)
  }
}

pnorm(-1.4)  #p-value

ggplot(data.frame(x=c(-4,4)), aes(x=x)) +
  theme_bw() +
  stat_function(fun=plot_area(-4, -1.4), geom="area", fill="red", alpha=0.2) +  
  stat_function(fun=dnorm) +
  geom_text(aes(y=0, x=c(-1.4),label=paste("x=",c(-1.4)), vjust=1))
