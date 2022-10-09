# Prediction-using-decision-tree
Predicting how many employees left the company

hr_train=read.csv("C:/Users/Namdeo/Downloads/hr_train.csv", stringsAsFactors = F)
hr_test=read.csv("C:/Users/Namdeo/Downloads/hr_test.csv", stringsAsFactors = F)

hr_test$left= NA

hr_train$data = 'train'
hr_test$data = 'test'

hr = rbind(hr_train,hr_test)

library(dplyr)

glimpse(hr)

table(hr$sales)
table(hr$Work_accident)
table(hr$satisfaction_level)
table(hr$last_evaluation)
table(hr$number_project)
table(hr$average_montly_hours)
table(hr$time_spend_company)
table(hr$promotion_last_5years)
table(hr$salary)
table(hr_train$left)

lapply(hr,function(x) sum(is.na(x)))

CreateDummies=function(data,var,freq_cutoff=0){
  t=table(data[,var])
  t=t[t>freq_cutoff]
  t=sort(t)
  categories=names(t)[-1]
  for( cat in categories){
    name=paste(var,cat,sep="_")
    name=gsub(" ","",name)
    name=gsub("-","_",name)
    name=gsub("\\?","Q",name)
    name=gsub("<","LT_",name)
    name=gsub("\\+","",name)
    name=gsub("\\/","_",name)
    name=gsub(">","GT_",name)
    name=gsub("=","EQ_",name)
    name=gsub(",","",name)
    data[,name]=as.numeric(data[,var]==cat)
  }
  data[,var]=NULL
  return(data)
}

names(hr)[sapply(hr,function(x) is.character(x))]

cat_cols = c("salary")

for(cat in cat_cols){
 hr=CreateDummies(hr,cat,50)
}

hr$left=as.factor(hr$left)

hr$left=as.numeric(hr$left==1)

for(col in names(hr)){
  if(sum(is.na(hr[,col]))>0 & !(col %in% c("data","left"))){
    hr[is.na(hr[,col]),col]=mean(hr[hr$data=='train',col],na.rm=T)
  }
}

hr_train=hr %>% filter(data=='train') %>% select(-data)
hr_test=hr %>% filter(data=='test') %>% select (-data,-left)

set.seed(123)
s=sample(1:nrow(hr_train),0.7*nrow(hr_train))
hr_train1=hr_train[s,]
hr_train2=hr_train[-s,]
 

#Decision tree

library(tree)

hr.tree = tree(left~.,data=hr_train1)
hr.tree

plot(hr.tree)
text(hr.tree)

val.score = predict(hr.tree,newdata = hr_train2,type="vector")[,1]
val.score

pROC::roc(hr_train2$left,val.score)$auc


hr.tree.final = tree(left~.,data=hr_train)
hr.tree

test.score=predict(hr.tree.final,newdata=hr_test,type='vector')[,1]
test.score
