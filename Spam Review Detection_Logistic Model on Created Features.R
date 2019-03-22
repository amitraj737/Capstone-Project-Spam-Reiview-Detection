#Import Data 
df=read.csv('C:/Users/Anchit/Desktop/Capstone/all_spam_with_features_final_dataset.csv')
table(df$Spam)
str(df)
#Checking the missing values
apply(df,2,function(x) sum(is.na(x)))
#Removing the missing values
df=na.omit(df)

table(df1$Spam)

#Imputing the ero values instead of junk value
df[df=="Inf"]=0

#Splitting the data into train and test in equal proportion
library(caTools)
set.seed(88)
split <- sample.split(df, SplitRatio = 0.75)

train <- subset(df, split == TRUE)
test  <- subset(df, split == FALSE)

#Removing the unrelavant variables
library(dplyr)
train=train%>% select(-"X",-"asin",-"reviewText",-"helpful",-"overall",-"reviewTime",-"reviewerID",-"reviewerName",-"summary",-"unixReviewTime")
table(train$Spam)

##Balancing the data
library(ROSE)

train <- ROSE(Spam ~ ., data = train,  seed=111)$data

#Apply the Logistic Model
base=glm(Spam~.,data = train,family = binomial)

summary(base)
formula(base)

Model=glm(Spam ~ No_of_feedbacks + No_helpful_feedbacks + Perc_helpful_feedbacks + 
             Len_review_title + len_review_body + Pos_review_Prod_sort_acs + 
            Perc_pos_review + Perc_neg_review + 
             Perc_num_wrd + Perc_caps_wrd + perc_all_cap_wrd  + 
             dev_prod_rate + senti_review,family = binomial, data = train)

test$score=predict(Model,newdata = test,type = "response")
train$score=predict(Model,newdata = train,type = "response")

#Calculation of Odds ratio
exp(Model$coefficients[-1])
sort(exp(Model$coefficients[-1]),decreasing = T)

###########################################AUC Curve with Feedback features##################################
library(ROCR)
ROCpred <- prediction(train$score,train$Spam)
ROCperf <- performance(ROCpred,"tpr","fpr")
plot(ROCperf)
plot(ROCperf, colorize=T, 
     print.cutoffs.at=seq(0,1,0.1), text.adj=c(-0.2,1.7),main="Roc Curve with Feedback features")

library(InformationValue)
auroc=AUROC(test$Spam,test$score)
auroc

######################AUC Curve without Feddback Features###################################################
Model1=glm(Spam ~  Len_review_title + len_review_body + Pos_review_Prod_sort_acs + 
             Perc_pos_review + Perc_neg_review + 
             Perc_num_wrd + Perc_caps_wrd + perc_all_cap_wrd  + 
             dev_prod_rate + senti_review,family = binomial, data = train)
test$score1=predict(Model1,newdata = test,type = "response")
train$score1=predict(Model1,newdata = train,type = "response")

library(ROCR)
ROCpred <- prediction(train$score1,train$Spam)
ROCperf <- performance(ROCpred,"tpr","fpr")
plot(ROCperf)
plot(ROCperf, colorize=T, 
     print.cutoffs.at=seq(0,1,0.1), text.adj=c(-0.2,1.7),main="Roc Curve without feedback features")

library(InformationValue)
auroc=AUROC(test$Spam,test$score1)
auroc


###################################Writing the Test data with highest score value of ReviewText######################

str(test)
table(test$Spam)
test1=test[,c("reviewText","Spam","score")]

write.csv(test1,file = "C:/Users/Anchit/Desktop/Capstone/test1.csv",row.names=FALSE)
