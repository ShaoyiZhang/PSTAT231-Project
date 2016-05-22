setwd("/Users/Shawn/Desktop/PSTAT231-Project")
library(data.table)
kobe = data.table(read.csv("kobe.csv"))

kobe$game_date = as.Date(kobe$game_date, "%Y-%m-%d")
head(kobe$game_date)
kobe[,home:=1]
kobe[substr(matchup,5,5)=='@',home:=0]
kobe[,matchup:=NULL]
library(tree)
kobe$home
summary(kobe)

kobe = kobe[,lat:=NULL]
kobe = kobe[,lon:=NULL]
kobe[opponent=="NOH",opponent:="NOP"]
kobe[]
kobe$opponent[opponent=="VAN":="MEM"]  
kobe$opponent[kobe$opponent=="SEA":="OKC"]  
kobe$opponent[kobe$opponent=="NJN":="BKN"] 
# NOH = NOP
# VAN = MEM
# SEA = OKC
# NJN = BKN
library(rpart)
kobe = data.table(read.csv("kobe.csv"))
kobe1 = kobe[shot_made_flag!='NA',]
str(kobe)
str(kobe1)
kobe
#orig.tree = rpart(data = kobe1,formula = shot_made_flag~.-opponent-action_type,na.action = NULL)
#table(kobe$com)
#kobe.keep = kobe[,c("period","shot_zone_area","shot_made_flag",""),with=F]

orig.tree = rpart(data = kobe,formula = shot_made_flag~.,na.action = NULL,control=rpart.control(minsplit=30, cp=0.001))

str(kobe.keep)
plot(orig.tree)
text(orig.tree)



library(randomForest)
tree.random = randomForest(formula = as.factor(shot_made_flag)~.-(matchup+action_type),na.action = NULL,data=kobe)
tree.random
str(kobe)


head(kobe.keep$shot_zone_area)
kobe.drop = kobe[c(-3,-4,-5,-8,-20,-21)]#subset(kobe,drop = c(1,2,3,4,5))
str(kobe.keep)
summary(kobe1)
