options("repos" = c(CRAN="https://mirrors.tuna.tsinghua.edu.cn/CRAN/"))

install.packages("ABC",repos="http://mirror.bjtu.edu.cn/ ")
install.packages("ggsci")
install.packages("sm")

install.packages("eoffice")
library(eoffice)

library(readxl)
library(ggsci)
kj <- read_excel("E:/R/新建文件夹/十字扣件重量.xlsx",sheet="Sheet2")
View(kj)

##简单绘图
par(mfrow=c(2,2))

##指定组数和颜色
p <- hist(kj$weight,breaks = 20,col = "red",freq = T,
     xlab="扣件重量/KG",ylab = "频数",main = "扣件重量分布",
     xlim=c(0.6,1.2))

##添加轴须图
rug(jitter(kj$weight))
lines(density(kj$weight),col="blue",lwd=2) 
legend("topright",legend = c("核密度曲线"),col = "blue",lty = 1,lwd = 2) 
box()


topptx(p,filename = "p.pptx",width = 6,height = 4)

##绘制核密度图
par(mfrow=c(2,2))
##默认核密度图
d <- (density(kj$weight))
plot(d,main="每袋扣件核密度曲线",frame.plot=TRUE)


mean(kj$weight)-sd(kj$weight)
mean(kj$weight)+sd(kj$weight)
d$x[139]
d$y[139]
library(ggplot2)
rug(jitter(kj$weight),col = "brown")

library(sm)
sm.density.compare(kj$weight,group,col=c(1:6),lty=1:6,lwd=2,xlab="扣件重量",ylab="密度")
title(main = "每袋扣件核密度曲线")
group <- factor(kj$group,levels = c(1:6),
                labels = c("第1袋","第2袋","第3袋",
                           "第4袋","第5袋","第6袋"))
colfill <- c(1:6)
legend("topright",levels(group),col = colfill,lty = 1:6,lwd = 2,
       x.intersp=1,y.intersp=1, cex = 1)



par(no.readonly = TRUE)

##扣件重量近似服从正太分布
##以这6袋，???180个扣件为样本，每次从中随机抽30个构成一袋，
##记Wi为第i次抽样获得的30个扣件的总量和（也就是每袋重量）???
##进行1000次抽样，获得1000个袋子的重量（W1-W1000???

##构建样本
set.seed(1234)
sample1000<-data.frame(time=1:1000,mean=0,var=0,weight=0)
View(sample1000)

##抽样，样本容???30，replace为T表示采取有重复的抽样，F不重复抽样，即无放回抽样
bag <- sample(kj$weight,30,F)

for (i in 1:1000){
  sample1000$mean[i] <- mean(sample(kj$weight,30,F))
  sample1000$var[i] <- var(sample(kj$weight,30,F))
  sample1000$weight[i] <- sum(sample(kj$weight,30,F))
}

t.test(sample1000$weight)
var(sample1000$weight)
sd(sample1000$weight)
##标准差是0.48
##正太检???
shapiro.test(sample1000$weight)
library(ggpubr)
qqnorm(sample1000$weight)
qqline(sample1000$weight)
##密度???
plot(density(sample1000$weight))

##指定组数和颜???
hist(sample1000$weight,breaks=30,col = "red",freq = F,
     xlab="扣件重量/KG",ylab = "频数",main = "扣件重量分布"
     )
##添加轴须图
rug(jitter(sample1000$weight))
lines(density(sample1000$weight),col="blue",lwd=2)
box()
polygon(density(sample1000$weight),col = "gray", border = "red",alpha=.3)


sort(sample1000$weight)
table(sample1000$weight)
length(unique(sample1000$weight))
b <- c(1:10, 9:2)

plot(c(1, 9), 1:2, type = "n",xlim =c(1,9) )
polygon(1:4, c(2,1,2,1),
        col = c("red", "blue"),
        border = c("green", "yellow"),
        lwd = 3, lty = c("dashed", "solid"))


d





library(ggplot2)
g <- ggplot(kj, aes(x=weight))

ggplot(kj, aes(x=weight)) + geom_density(stat = "density",fill = "gray", alpha=.3,colour="blue")

ggplot(kj, aes(x=weight)) + geom_density()

ggplot(kj, aes(x=weight)) + 
  geom_histogram(aes(y=..density..),      # 这一步很重要,使用density代替y???
                 binwidth=.02,
                 colour="black", fill="white") +scale_color_npg()
  geom_density(alpha=.2, fill="#FF6666")  # 重叠部分采用透明设置


g + geom_histogram(binwidth = .01,color="black",fill="white") + 
  geom_vline(xintercept = mean(kj$weight))+
  geom_density(alpha = 0.2, fill = "#FF6666")+
  labs(title="Bar Chart", 
       subtitle="Manufacturer of vehicles", 
       caption="Source: Frequency of Manufacturers from 'mpg' dataset") +
  theme(axis.text.x = element_text(angle=65, vjust=0.6))

g + geom_histogram(aes(y=..density..),binwidth = .01,color="black",
                   fill="lightblue",linetype = "dashed",freq=F) + 
  geom_density(aes(y=..scaled..),alpha = 0.2, fill = "#FF6666")+
  geom_vline(xintercept = mean(kj$weight),color="#ef8a62",size=1)+
  geom_vline(xintercept = median(kj$weight),color="#67a9cf",size=1)+
  geom_hline(yintercept = 1,color="#ef8a62",size=1)+
  labs(title="Bar Chart", 
       subtitle="Manufacturer of vehicles", 
       caption="Source: Frequency of Manufacturers from 'mpg' dataset") +
  theme(axis.text.x = element_text(angle=65, vjust=0.6))








##构建样本
set.seed(1234)
sample1000_30<-data.frame(time=1:1000,mean=0,var=0,weight=0)
View(sample1000_30)

##抽样，样本容量30，replace为T表示采取有重复的抽样，F不重复抽样，即无放回抽样
bag <- sample(kj$weight,30,F)

for (i in 1:1000){
  sample1000_30$mean[i] <- mean(sample(kj$weight,30,F))
  sample1000_30$var[i] <- var(sample(kj$weight,30,F))
  sample1000_30$weight[i] <- sum(sample(kj$weight,30,F))
}


##抽样，样本容量29
sample1000_29<-data.frame(time=1:1000,mean=0,var=0,weight=0)

bag_29 <- sample(kj$weight,29,F)
View(sample1000_29)

for (i in 1:1000){
  sample1000_29$mean[i] <- mean(sample(kj$weight,29,F))
  sample1000_29$var[i] <- var(sample(kj$weight,29,F))
  sample1000_29$weight[i] <- sum(sample(kj$weight,29,F))
}


##频率分布
ggplot(sample1000, aes(x=weight)) +
  geom_histogram(binwidth = .02,color="black",
                 fill="lightblue",linetype = "dashed")
table(sample1000$weight)

##密度分布
ggplot(sample1000, aes(x=vote)) +
  geom_histogram(aes(y=stat(count)/sum(count)),binwidth = .01,color="black",
                 fill="lightblue",linetype = "dashed")

##密度分布
ggplot(sample1000, aes(x=vote)) +
  geom_histogram(aes(y=..density..),binwidth = .01,color="black",
                 fill="lightblue",linetype = "dashed")

##合在一???
ggplot(sample1000, aes(x=vote)) +
  geom_histogram(aes(y=..density..),binwidth = .01,color="black",
                 fill="lightblue",linetype = "dashed")+
  geom_density(colour="blue")

#正态性检???
shapiro.test(sample1000$mean)
qqnorm(sample1000$mean)
qqline(sample1000$mean)

shapiro.test(sample1000$var)
qqnorm(sample1000$var)
qqline(sample1000$var)

shapiro.test(sample1000$vote)
qqnorm(sample1000$vote)
qqline(sample1000$vote)

var(sample1000$vote)
sqrt(var(sample1000$vote))

t.test(sample1000$vote)
var(sample1000$vote)

var(sample500$vote)
sqrt(var(sample500$vote))
mean(sample500$vote)
t.test(sample500$vote)





####20的样???
sample20<-data.frame(time=1:1000,mean=0,var=0,vote=0)
View(sample20)

##抽样，样本容???30，replace为T表示采取有重复的抽样，F不重复抽样，即无放回抽样
bag <- sample(kj$weight,30,F)

for (i in 1:1000){
  sample20$mean[i] <- mean(sample(kj$weight,20,F))
  sample20$var[i] <- var(sample(kj$weight,20,F))
  sample20$vote[i] <- sum(sample(kj$weight,20,F))
}

var(sample20$vote)
sqrt(var(sample20$vote))


####10的样???
sample10<-data.frame(time=1:1000,mean=0,var=0,vote=0)
View(sample10)

##抽样，样本容???30，replace为T表示采取有重复的抽样，F不重复抽样，即无放回抽样
bag <- sample(kj$weight,10,F)

for (i in 1:1000){
  sample10$mean[i] <- mean(sample(kj$weight,10,F))
  sample10$var[i] <- var(sample(kj$weight,10,F))
  sample10$vote[i] <- sum(sample(kj$weight,10,F))
}
View(sample10)
var(sample10$vote)
sqrt(var(sample10$vote))

##合在一???
ggplot(sample10, aes(x=vote)) +
  geom_histogram(aes(y=..density..),binwidth = .01,color="black",
                 fill="lightblue",linetype = "dashed")+
  geom_density(colour="blue")


####5的样???
sample5<-data.frame(time=1:1000,mean=0,var=0,vote=0)
View(sample5)

##抽样，样本容???30，replace为T表示采取有重复的抽样，F不重复抽样，即无放回抽样
bag <- sample(kj$weight,10,F)

for (i in 1:1000){
  sample5$mean[i] <- mean(sample(kj$weight,5,F))
  sample5$var[i] <- var(sample(kj$weight,5,F))
  sample5$vote[i] <- sum(sample(kj$weight,5,F))
}
View(sample5)
var(sample5$vote)
sqrt(var(sample5$vote))
t.test(sample5$vote)

##合在一???
ggplot(sample5, aes(x=vote)) +
  geom_histogram(aes(y=..density..),binwidth = .01,color="black",
                 fill="lightblue",linetype = "dashed")+
  geom_density(colour="blue")


c <- c(175,176,173,175,174,173,173,176,173,179)
var(c)
t.test(c)

mean1 <- mean(sample500$vote)  #16.8274
var1 <- var(sample500$vote)    #0.1662818

mean1-1.96*var1/sqrt(500)   #16.81282
mean1+1.96*var1/sqrt(500)   #16.84198

sum(sample500$vote)/500
t.test(sample500$vote)
ggplot(data = sample500,aes(x=vote))+geom_histogram(binwidth = 0.05)+
  geom_vline(xintercept = mean1)+
  geom_vline(xintercept = 16.81282)+geom_vline(xintercept = 16.84198)+
  geom_density()

##30个样???
sample500a<-data.frame(time=1:500,mean=0,var=0,vote=0)
b <- sample(kj$weight,30,F)

for (i in 1:500){
  sample500a$mean[i] <- mean(sample(kj$weight,30,F))
  sample500a$var[i] <- var(sample(kj$weight,30,F))
  sample500a$vote[i] <- sum(sample(kj$weight,30,F))
}

View(sample500a)
shapiro.test(sample500a$mean)
shapiro.test(sample500a$var)
shapiro.test(sample500a$vote)

mean2 <- mean(sample500a$vote)  #25.25808
var2 <- var(sample500a$vote)    #0.2341398

mean2-1.96*var2/sqrt(500)   #25.23756
mean2+1.96*var2/sqrt(500)   #25.2786

sum(sample500$vote)/500
t.test(sample500$vote)
ggplot(data = sample500,aes(x=vote))+geom_histogram(binwidth = 0.05)+
  geom_vline(xintercept = mean1)+
  geom_vline(xintercept = 16.81282)+geom_vline(xintercept = 16.84198)+
  geom_density()

    