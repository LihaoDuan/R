x <- rnorm(1000) 
x
plot(x)
hist(x)
mean(x)
var(x)
sd(x)
quantile(x)
 data("diamonds")
p1 <- ggplot(subset(diamonds,carat >= 2.2),
             aes(x=table,y=price,colour=cut)) +
  geom_point(alpha=.7) +
  geom_smooth(method = "loess",alpha=.05,size=1,span=1) +
  theme_bw()

p1 +scale_color_npg()

install.packages("d3Network")
install.packages('networkD3')
# Load package
library(d3Network)

# Load energy projection data
library(curl)
# Library
library(networkD3)

# A connection data frame is a list of flows with intensity for each flow
links <- data.frame(
  source=c("group_A","group_A", "group_B", "group_C", "group_C", "group_E"), 
  target=c("group_C","group_D", "group_E", "group_F", "group_G", "group_H"), 
  value=c(2,3, 2, 3, 1, 3)
)

# From these flows we need to create a node data frame: it lists every entities involved in the flow
nodes <- data.frame(
  name=c(as.character(links$source), 
         as.character(links$target)) %>% unique()
)

# With networkD3, connection must be provided using id, not using real name like in the links dataframe.. So we need to reformat it.
links$IDsource <- match(links$source, nodes$name)-1 
links$IDtarget <- match(links$target, nodes$name)-1

# Make the Network
p <- sankeyNetwork(Links = links, Nodes = nodes,
                   Source = "IDsource", Target = "IDtarget",
                   Value = "value", NodeID = "name", 
                   sinksRight=FALSE)
p

# save the widget
# library(htmlwidgets)
# saveWidget(p, file=paste0( getwd(), "/HtmlWidget/sankeyBasic1.html"))

plot(value29,xlim=c(22.5,27), xaxt="n", 
     main = "29个一袋和30个一袋重量分布", ylab="密度", xlab="每袋扣件重量")
Axis(side=1, at=seq(22.5, 27, by = .5), las=0)
Axis(side=2, labels=FALSE)
lines(value30,col="black",lwd=1, ylim=c(0,1))
legend("topright", legend=c("30个一袋装95%概率区间", "29个一袋装95%概率区间"), fill=c("blue", "red"),
       density=c(25, 25), bty="n") 

polygon(c(xbegin1, value30$x[poly_range1], xend1),                # X-Coordinates of polygon range
        c(0, value30$y[poly_range1], 0),   density = 10, angle = 45,              # Y-Coordinates of polygon range
        col = "blue",border = "black",lwd=.5)  

polygon(c(xbegin2, value29$x[poly_range2], xend2),                # X-Coordinates of polygon range
        c(0, value29$y[poly_range2], 0),   density = 10, angle = -30,              # Y-Coordinates of polygon range
        col = "red",border = "black",lwd=.5)  
axis(1, at = c(round(xbegin1,1), round(xbegin2,1), round(xend1,1), round(xend2,1)), 
     labels = c(rep("",4)),
     cex.axis=.8, las = 2, tck=.015,
     col.ticks  = "red", col.axis="red")

axis(1, at = c(round(xbegin1,1),round(xend1,1) ), 
     labels = c(rep("",2)),
     cex.axis=.8, las = 2, tck=.015,
     col.ticks  = "blue", col.axis="red")

axis(1, at = c(round(xbegin2,1), round(xend2,1)), 
     labels = c(rep("",2)),
     cex.axis=.8, las = 2, tck=.015,
     col.ticks  = "red", col.axis="red")

lablist1 <- c(round(xbegin1,1), round(xbegin2,1), round(xend1,1), round(xend2,1))
text(lablist1, par("usr")[3] + 0.01, labels = lablist1, srt = 0, pos = 1, xpd = TRUE, cex = .8, col = "red")

axis(1, at = , labels = NA)

text(lablist1, par("usr")[3] + 0.01, labels = lablist1, srt = 0, pos = 1, xpd = TRUE, cex = .8, 
     col = c("red","blue","red","blue"))



---
  title: "Untitled"
output: html_document
---
  
  ```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

## R Markdown

This is an R Markdown document. Markdown is a simple formatting syntax for authoring HTML, PDF, and MS Word documents. For more details on using R Markdown see <http://rmarkdown.rstudio.com>.

### **原始数据分布**

收集了180个扣件的重量数据，一共6袋，每袋30个扣件

```{r , echo=FALSE}
## 原始数据

##收集了180个扣件的重量数据，一共6袋，每袋30个扣件,数据分布如下图

library(readxl)
kj <- read_excel("E:/R/新建文件夹/十字扣件重量.xlsx",sheet="Sheet2")

hist(kj$weight,breaks = 20,col = "red",freq = T,
     xlab="扣件重量/KG",ylab = "频数",main = "扣件重量频数分布直方图",
     xlim=c(0.6,1.2))
##添加轴须图
rug(jitter(kj$weight))

lines(density(kj$weight),col="blue",lwd=2)
legend("topright",legend = c("核密度曲线"),col = "blue",lty = 1,lwd = 2)

box()

hist(kj$weight,breaks = 20,col = "red",freq = F,
     xlab="扣件重量/KG",ylab = "密度",main = "扣件重量分布",
     xlim=c(0.6,1.2))
##添加轴须图
rug(jitter(kj$weight))

lines(density(kj$weight),col="blue",lwd=2)
legend("topright",legend = c("核密度曲线"),col = "blue",lty = 1,lwd = 2)

box()


library(sm)
sm.density.compare(kj$weight,kj$group,col=c(1:6),lty=1:6,lwd=2,xlab="扣件重量/KG",ylab="密度")
title(main = "每袋扣件核密度曲线")
group <- factor(kj$group,levels = c(1:6),
                labels = c("第1袋","第2袋","第3袋",
                           "第4袋","第5袋","第6袋"))
colfill <- c(1:6)
legend("topright",levels(group),col = colfill,lty = 1:6,lwd = 2,
       x.intersp=1,y.intersp=1, cex = 1)
```

### **可以看出单个扣件重量集中在0.8kg左右**


####   以收集到的180个扣件重量为样本

每次随机从中取出30个扣件，并计算30个扣件总重量(相当于30个一袋的重量)，一共进行1000次，相当于模拟了1000个扣件袋子。

每次随机从中取出29个扣件，并计算29个扣件总重量(相当于29个一袋的重量)，一共进行1000次，相当于模拟了1000个扣件袋子。

```{r , echo=FALSE}
##构建样本
set.seed(1234)

##抽样，样本容量30、29
sample1000_30 <- data.frame(time=1:1000,mean=0,var=0,weight=0)
sample1000_29 <- data.frame(time=1:1000,mean=0,var=0,weight=0)

for (i in 1:1000){
  sample1000_30$mean[i] <- mean(sample(kj$weight,30,F))
  sample1000_30$var[i] <- var(sample(kj$weight,30,F))
  sample1000_30$weight[i] <- sum(sample(kj$weight,30,F))
}

for (i in 1:1000){
  sample1000_29$mean[i] <- mean(sample(kj$weight,29,F))
  sample1000_29$var[i] <- var(sample(kj$weight,29,F))
  sample1000_29$weight[i] <- sum(sample(kj$weight,29,F))
}

##获得密度曲线XY值
value30 <- density(sample1000_30$weight)
value29 <- density(sample1000_29$weight)

##绘图
plot(value29,xlim=c(22.5,27), xaxt="n", 
     main = "29个一袋和30个一袋重量分布", ylab="密度", xlab="每袋扣件重量/kg")
Axis(side=1,at=seq(22.5, 27, by = .5),las=0)
Axis(side=2, labels=FALSE)
lines(value30,col="black",lwd=1, ylim=c(0,1))
legend("topright", legend=c("30个一袋装重量95%概率区间", "29个一袋装重量95%概率区间"), fill=c("blue", "red"),
       density=c(25, 25), bty="n") 

##求30个一袋2个标准差的值域
xbegin1 <- mean(sample1000_30$weight)-2*sd(sample1000_30$weight)
xend1 <- mean(sample1000_30$weight)+2*sd(sample1000_30$weight)

poly_range1 <- value30$x >= xbegin1 & value30$x <= xend1

polygon(c(xbegin1, value30$x[poly_range1], xend1),                # X-Coordinates of polygon range
        c(0, value30$y[poly_range1], 0),   density = 10, angle = 45,              # Y-Coordinates of polygon range
        col = "blue",border = "black",lwd=.5)  

##求29个一袋2个标准差的值域
xbegin2 <- mean(sample1000_29$weight)-2*sd(sample1000_29$weight)
xend2 <- mean(sample1000_29$weight)+2*sd(sample1000_29$weight)

poly_range2 <- value29$x >= xbegin2 & value29$x <= xend2

polygon(c(xbegin2, value29$x[poly_range2], xend2),                # X-Coordinates of polygon range
        c(0, value29$y[poly_range2], 0),   density = 10, angle = -45,              # Y-Coordinates of polygon range
        col = "red",border = "black",lwd=.5)  

axis(1, at = c(round(xbegin1,1),round(xend1,1) ), 
     labels = c(rep("",2)),
     cex.axis=.8, las = 2, tck=.015,
     col.ticks  = "blue", col.axis="red")

axis(1, at = c(round(xbegin2,1), round(xend2,1)), 
     labels = c(rep("",2)),
     cex.axis=.8, las = 2, tck=.015,
     col.ticks  = "red", col.axis="red")

lablist1 <- c(round(xbegin1,1), round(xbegin2,1), round(xend1,1), round(xend2,1))

text(lablist1, par("usr")[3] + 0.01, labels = lablist1, srt = 0, pos = 1, xpd = TRUE, cex = .8, 
     col = c("blue","red","blue","red"))
lines(c(24.2,25.3), c(0,0), col = "red", lwd = 1.5)
```

由上图可以看出：

30个扣件装一袋的重量其95%的概率区间是在24.2-26.2kg，表示随机从样本中抽取30个扣件作为一袋，

抽取100次，其中95次袋子的重量在24.2-26.2kg区间内；

29个扣件装一袋的重量其95%的概率区间是在23.4-25.3kg，表示随机从样本中抽取29个扣件作为一袋，

抽取100次，其中95次袋子的重量在23.4-25.3kg区间内；

由于30袋和29袋装重量95%概率区间在24.2-25.3kg部分重合，对于那些重量在该区间的扣件我们无法判断其实来自29袋还是30袋。

因此30个一袋无法根据重量验收。


Note that the `echo = FALSE` parameter was added to the code chunk to prevent printing of the R code that generated the plot.


### **原始数据分布**

```{r , echo=FALSE}
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

value7 <- density(sample1000_7$weight)
value6 <- density(sample1000_6$weight)
value5 <- density(sample1000_5$weight)
value4 <- density(sample1000_4$weight)

plot(value4,xlim=c(2.9,7))

##7个一袋样本2个标准差阴影
xbegin6 <- mean(sample1000_7$weight)-2*sd(sample1000_7$weight)
xend6 <- mean(sample1000_7$weight)+2*sd(sample1000_7$weight)

redtrans <- rgb(255, 0, 0, 127, maxColorValue=255)

poly_range6 <- value7$x >= xbegin6 & value7$x <= xend6
polygon(c(xbegin6, value7$x[poly_range6], xend6),                # X-Coordinates of polygon range
        c(0, value7$y[poly_range6], 0),               # Y-Coordinates of polygon range
        col ="#0000FF22", border = "black",lwd=.5) 

##6个一袋样本2个标准差阴影
xbegin7 <- mean(sample1000_6$weight)-2*sd(sample1000_6$weight)
xend7 <- mean(sample1000_6$weight)+2*sd(sample1000_6$weight)

poly_range7 <- value6$x >= xbegin7 & value6$x <= xend7
polygon(c(xbegin7, value6$x[poly_range7], xend7),                # X-Coordinates of polygon range
        c(0, value6$y[poly_range7], 0),              # Y-Coordinates of polygon range
        col = "#00FF0022",border = "black",lwd=.5)  

##5个一袋样本2个标准差阴影
xbegin8 <- mean(sample1000_5$weight)-2*sd(sample1000_5$weight)
xend8 <- mean(sample1000_5$weight)+2*sd(sample1000_5$weight)

poly_range8 <- value5$x >= xbegin8 & value5$x <= xend8
polygon(c(xbegin8, value5$x[poly_range8], xend8),                # X-Coordinates of polygon range
        c(0, value5$y[poly_range8], 0),               # Y-Coordinates of polygon range
        col = "#FF000022",border = "black",lwd=.5)  

##4个一袋样本2个标准差阴影
xbegin9 <- mean(sample1000_4$weight)-2*sd(sample1000_4$weight)
xend9 <- mean(sample1000_4$weight)+2*sd(sample1000_4$weight)

poly_range9 <- value5$x >= xbegin9 & value4$x <= xend9
polygon(c(xbegin9, value4$x[poly_range9], xend9),                # X-Coordinates of polygon range
        c(0, value4$y[poly_range9], 0),               # Y-Coordinates of polygon range
        col = "#00FF0022",border = "black",lwd=.5)  



lines(value7,col="BLACK",lwd=1, ylim=c(0,1))
```

