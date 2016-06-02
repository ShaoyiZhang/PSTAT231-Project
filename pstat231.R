setwd("/Users/Shawn/Desktop/PSTAT231-Project")
set.seed(3)
library(data.table)
kobe = data.table(read.csv("kobe.csv"))
kobe = kobe[complete.cases(kobe),]
test.index = sample(seq_len(nrow(kobe)), size = floor(nrow(kobe)*0.1),replace = F)
train.index = setdiff(seq_len(nrow(kobe)),test.index)


kobe = kobe[complete.cases(kobe),]
kobe$game_date = as.Date(kobe$game_date, "%Y-%m-%d")
# latitude and longitude are obviously useless
kobe[,lon:=NULL]
kobe[,lat:=NULL]
kobe[,game_id:=NULL]
kobe[,game_event_id:=NULL]
kobe[,team_id:=NULL]
kobe[,team_name:=NULL]
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
library(rminer)
kobe$opponent = delevels(kobe$opponent, c("NOH","VAN","SEA","NJN"), label = NULL)

# watch for the correlation between loc_x/loc_y with shot zone/shot distance
# library(corrplot)
# kobe.keep = kobe[,c("period","shot_distance","loc_x","loc_y","playoffs"),with=F]
# corr = corrplot(cor(kobe.keep))
# # shot distance has a high correlation between loc_y
#kobe[,loc_y:=NULL]

# check if we need action_type
action.fit = lm(data = kobe, shot_made_flag~action_type)
summary(action.fit)
combine.fit = lm(data = kobe,shot_made_flag~combined_shot_type)
summary(combine.fit)

action.glm = glm(data = kobe, shot_made_flag~action_type)
summary(action.glm)
combine.glm = glm(data = kobe,shot_made_flag~combined_shot_type)
summary(combine.glm)
# not all action type are statistically significant

# action_type is important

freqTable = data.table(action_types = levels(kobe$action_type),frequency = as.vector(table(kobe$action_type)))
freqTable =  freqTable[frequency>=50]
freqTable
kobe[,type:=combined_shot_type,by=1:nrow(kobe)]
kobe[action_type %in% freqTable$action_types,type:=action_type]#, by=1:nrow(kobe)]
str(kobe)
levels(kobe$type)

# delete action type
kobe[,action_type:=NULL]
kobe[,combined_shot_type:=NULL]
######################### data cleaning complete ###############################

# contigency table
tbl1 = table(kobe$shot_type,kobe$shot_zone_area)
chi1 = chisq.test(tbl1,correct = F)
chi1

tbl2 = table(kobe$shot_type,kobe$shot_zone_basic)
chi2 = chisq.test(tbl2,correct = F)
chi2

tbl3 = table(kobe$shot_type,kobe$shot_zone_area)
chi3 = chisq.test(tbl3,correct = F)
chi3


#head(kobe$shot_zone_area)

# try boxplot to see correlation between categorical variables
#box.shot_type = ggplot(data = kobe, aes(x = shot_type, y = shot_made_flag))+geom_boxplot()
#box.shot_type
#str(kobe)
# try decision tree
library(rpart)
orig.tree = rpart(data = kobe,formula = shot_made_flag~.-shot_id,na.action = NULL,control=rpart.control(minsplit=30, cp=0.001))
plot(orig.tree)
text(orig.tree)
summary(orig.tree)
# try random forrest
library(randomForest)
kobe.rf = randomForest(formula=as.factor(shot_made_flag)~.-shot_id,na.action=NULL,data = kobe,ntree=500)
imptplot = varImpPlot(kobe.rf)
imptplot = as.data.table(imptplot,keep.rownames = T)
str(as.data.frame(imptplot))

impt.sort = setorder(imptplot, cols = "MeanDecreaseGini")
#impt.sort
# Important variables:
# opponent, type, seconds, loc_x, game_date, season, shot_distance, period
#1,2,3,5,6,7,12,13,14,15,17
col.keep = c(impt.sort$rn[7:15],"shot_made_flag","shot_id")
colnames(kobe)
kobe.keep = subset(kobe,select = col.keep)

colnames(kobe.keep)
head(kobe.keep)

# actuall prediction 
test = kobe.keep[test.index,]
train = kobe.keep[train.index,]


keep.rf = randomForest(formula=as.factor(shot_made_flag)~.-shot_id,na.action=NULL,data = train,ntree=500)
imptplot = varImpPlot(keep.rf)
imptplot

rand.pred = predict(keep.rf,test,type="class")
rand.conti.table = table(rand.pred,test$shot_made_flag)
rf.error.rate = (rand.conti.table[3] + rand.conti.table[2])/nrow(test)
rf.error.rate

rand.train = predict(keep.rf,train,type="class")
rand.train.table = table(rand.train,train$shot_made_flag)
rf.train.error = (rand.train.table[3] + rand.train.table[2])/nrow(train)
rf.train.error

glm.fit = glm(data = train, shot_made_flag~.-shot_id,family = binomial)#(link = "logit"))
glm.probs = predict(glm.fit,test,type="response")
glm.pred=rep("Shot fail",nrow(test))
glm.pred[glm.probs>0.5]="Shot Made"
glm.conti.table = table(glm.pred,test$shot_made_flag)
glm.error.rate = (glm.conti.table[3] + glm.conti.table[2])/nrow(test)
glm.error.rate
glm.fit

glm.probs.train = predict(glm.fit,train,type="response")
str(glm.probs.train)
glm.pred.train = rep("Shot fail",nrow(train))
glm.pred.train[glm.probs.train>0.5]="Shot Made"
glm.conti.table.train = table(glm.pred.train,train$shot_made_flag)
glm.error.rate.train = (glm.conti.table.train[3] + glm.conti.table.train[2])/nrow(train)
glm.error.rate

#c(1996-97,1997-98,1998-99,1999-00,2000-01,2001-02,2002-03,2003-04,2004-05,2005-06,2006-07,2007-08,2008-09,2009-10,2010-11,2011-12,2012-12,2013-14,2015-16)
#library(boot)
#cost<-function(y,pi=0) {mean(abs(y - pi)>0.5)}
#cvglm = cv.glm(train,glm.fit,cost)
salary = data.table(
  c("1996-97","1997-98","1998-99","1999-00","2000-01","2001-02","2002-03","2003-04","2004-05","2005-06","2006-07","2007-08","2008-09","2009-10","2010-11","2011-12","2012-12","2013-14","2014-15","2015-16"),c(1015000,1167240,1319000,9000000,10130000,11250000,12375000,13500000,14175000,15946875,17718750,19490625,21262500,23034375,24806250,25244493,27849149,30453805,23500000,25000000)
)
plot(salary)
#areer: $328,238,062


# junk b