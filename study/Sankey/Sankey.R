library(d3Network)

setwd('E:/R/新建文件夹/【批量下载】Sankey等/我的资源/桑基图')

sankey <- read.table("sankey.txt",header = TRUE,sep = "\t")
View(sankey)

nodes <- data.frame(name = unique(c(as.character(sankey$source),as.character(sankey$target))),stringsAsFactors = FALSE)
View(nodes)

nodes$ID <- 0:(nrow(nodes)-1)
sankey <- merge(sankey,nodes,by.x = "source",by.y = "name")

sankey <- merge(sankey,nodes,by.x = "target",by.y = "name")
View(sankey)

colnames(sankey) <- c("X","Y","value","source","target")
sankey <- subset(sankey,select = c("source","target","value"))
nodes <- subset(nodes,select = c("name"))


library(networkD3)
sankeyNetwork(Links = sankey,Nodes = nodes,fontSize = 10,
         Source = "source",Target = "target",Value = "value",
         NodeID = "name")

d3Sankey(Links = sankey,Nodes = nodes,fontsize = 20,
         Source = "source",Target = "target",Value = "value",
         NodeID = "name",file = "Sankey.html",
         width = 1200,height = 900)




library(readxl)
class23 <- read_excel("201906-202009开累统计.xls",sheet="class23")
View(class23)

nodes23 <- data.frame(name = unique(c(as.character(class23$source),as.character(class23$target))),stringsAsFactors = FALSE)
View(nodes23)

nodes23$ID <- 0:(nrow(nodes23)-1)
class23 <- merge(class23,nodes23,by.x = "source",by.y = "name")

class23 <- merge(class23,nodes23,by.x = "target",by.y = "name")


colnames(class23) <- c("X","Y","value","source","target")
class23 <- subset(class23,select = c("source","target","value"))
nodes23 <- subset(nodes23,select = c("name"))


library(networkD3)
sankeyNetwork(Links = class23,Nodes = nodes23,fontSize = 10,
              Source = "source",Target = "target",Value = "value",
              NodeID = "name")

##
class34 <- read_excel("201906-202009开累统计.xls",sheet="class34")
View(class34)

nodes34 <- data.frame(name = unique(c(as.character(class34$source),as.character(class34$target))),stringsAsFactors = FALSE)
View(nodes34)

nodes34$ID <- 0:(nrow(nodes34)-1)
class34 <- merge(class34,nodes34,by.x = "source",by.y = "name")

class34 <- merge(class34,nodes34,by.x = "target",by.y = "name")


colnames(class34) <- c("X","Y","value","source","target")
class34 <- subset(class34,select = c("source","target","value"))
nodes34 <- subset(nodes34,select = c("name"))


library(networkD3)
sankeyNetwork(Links = class34,Nodes = nodes34,fontSize = 10,
              Source = "source",Target = "target",Value = "value",
              NodeID = "name")





class34 <- read_excel("201906-202009开累统计.xls",sheet="class45")
class34 <- read_excel("201906-202009开累统计.xls",sheet="Sheet6")
class <- read_excel("201906-202009开累统计.xls",sheet="Sheet4")
class <- as.data.frame(class)
##combine
class <- as.data.frame(rbind(class23,class34))
class$source <- class$source-1
class$target <- class$target-1
View(class)

nodes <- data.frame(name = unique(c(as.character(class$source),as.character(class$target))),stringsAsFactors = FALSE)


nodes$ID <- 0:(nrow(nodes)-1)
class <- merge(class,nodes,by.x = "source",by.y = "name")

class <- merge(class,nodes,by.x = "target",by.y = "name")


colnames(class) <- c("X","Y","value","source","target")
class <- subset(class,select = c("source","target","value"))
nodes <- subset(nodes,select = c("name"))


library(networkD3)
sankeyNetwork(Links = class,Nodes = nodes,fontSize = 10,
              Source = "source",Target = "target",Value = "value",
              NodeID = "name")
