setwd("/Users/Shawn/Desktop/PSTAT231-Project")
library(data.table)
kobe = data.table(read.csv("kobe.csv"))

kobe$game_date = as.Date(kobe$game_date, "%Y-%m-%d")
# no latitude and longitude
kobe[,lon:=NULL]
kobe[,lat:=NULL]
kobe[,game_id:=NULL]
kobe[,game_event_id:=NULL]
kobe[,team_id:=NULL]
kobe[,team_name:=NULL]
#kobe[,shot_id:=NULL]
kobe[,seconds:=minutes_remaining*60+seconds_remaining,by=1:nrow(kobe)]
kobe[,minutes_remaining:=NULL]
kobe[,seconds_remaining:=NULL]
# get rid of mininutes_remaining
# going to replace matchup
kobe[,home:=1]
kobe[substr(matchup,5,5)=='@',home:=0]
kobe[,matchup:=NULL]
# some teams changed their names or locations
kobe[opponent=="NOH",opponent:="NOP"]
kobe[opponent=="VAN",opponent:="MEM"]  
kobe[opponent=="SEA",opponent:="OKC"]  
kobe[opponent=="NJN",opponent:="BKN"] 

kobe[]
table(kobe$action_type)
as.vector(table(kobe$action_type))
as.factor(table(kobe$action_type))

# check if we need action_type
action.fit = lm(data = kobe, shot_made_flag~action_type)
summary(action.fit)
combine.fit = lm(data = kobe,shot_made_flag~combined_shot_type)
summary(combine.fit)

action.glm = glm(data = kobe, shot_made_flag~action_type)
summary(action.glm)
combine.glm = glm(data = kobe,shot_made_flag~combined_shot_type)
summary(combine.glm)
# action_type is important

# try delete action type
kobe[,action_type:=NULL]

# watch for the correlation between loc_x/loc_y with shot zone/shot distance
library(corrplot)
kobe.keep = kobe[,c("period","shot_distance","loc_x","loc_y","playoffs"),with=F]
corr = corrplot(cor(kobe.keep))
# shot distance has a high correlation between loc_y
kobe[,loc_y:=NULL]
str(kobe)

# try decision tree
library(rpart)
orig.tree = rpart(data = kobe,formula = shot_made_flag~.-shot_id,na.action = NULL,control=rpart.control(minsplit=30, cp=0.001))
plot(orig.tree)
text(orig.tree)
table(kobe$combined_shot_type)
# Bank shot and Dunk are good


kobe[]
str(kobe)

library(tree)
kobe$home
summary(kobe)

kobe = kobe[,lat:=NULL]
kobe = kobe[,lon:=NULL]

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
#

orig.tree = rpart(data = kobe,formula = shot_made_flag~.,na.action = NULL,control=rpart.control(minsplit=30, cp=0.001))

str(kobe.keep)
plot(orig.tree)
text(orig.tree)



library(randomForest)
tree.random = randomForest(formula = as.factor(shot_made_flag)~.-(matchup+action_type+game_event_id+team_id+team_name+lat+lon),na.action = NULL,data=kobe)
varImpPlot(tree.random)
str(kobe)
table(kobe$action_type)



#install.packages("corrplot")
library(corrplot)




head(kobe.keep$shot_zone_area)
kobe.drop = kobe[c(-3,-4,-5,-8,-20,-21)]#subset(kobe,drop = c(1,2,3,4,5))
str(kobe.keep)
summary(kobe1)
