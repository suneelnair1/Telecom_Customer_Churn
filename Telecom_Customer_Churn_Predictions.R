# Setting the working directory 

setwd("C:/Users/suneelnair1/Documents/Sunil/Class_notes/PHD_Hackathon/data_description/20180120_PHD_Batch31_Classification_Patterns_ForStudents/TrainData")

# Reading the Test and Train data

Train_Demographics <- read.csv("Train_Demographics.csv",na.strings = '?')
Train_AccountInfo <- read.csv("Train_AccountInfo.csv",na.strings = 'MISSINGVAL')
Train_ServicesOptedFor <- read.csv("Train_ServicesOptedFor.csv")
Train_Churn <- read.csv("Train.csv")

#install.packages("reshape2")
library("reshape2")

Train_ServicesOptedFor <- dcast(Train_ServicesOptedFor,CustomerID~TypeOfService)

head(Train_ServicesOptedFor)
dim(Train_ServicesOptedFor)
dim(Train_Demographics)
dim(Train_AccountInfo)
dim(Train_Churn)

str(Train_Demographics)
str(Train_ServicesOptedFor)
str(Train_AccountInfo)
str(Train_Churn)

Train <- merge(x=Train_AccountInfo,y=Train_ServicesOptedFor, by.x = "CustomerID", by.y = "CustomerID")
Train <- merge(x=Train,y=Train_Demographics,by.x="CustomerID",by.y="HouseholdID")
Train <- merge(x=Train,y=Train_Churn,by.x = "CustomerID", by.y = "CustomerID")

str(Train)
dim(Train)



Test_Demographics <- read.csv("Test_Demographics.csv",na.strings = '?')
Test_AccountInfo <- read.csv("Test_AccountInfo.csv",na.strings = 'MISSINGVAL')
Test_ServicesOptedFor <- read.csv("Test_ServicesOptedFor.csv")
Test_Churn <- read.csv("Test.csv")

Test_ServicesOptedFor <- dcast(Test_ServicesOptedFor,CustomerID~TypeOfService)

dim(Test_ServicesOptedFor)

Test <- merge(x=Test_AccountInfo,y=Test_ServicesOptedFor, by.x = "CustomerID", by.y = "CustomerID")
Test <- merge(x=Test,y=Test_Demographics,by.x="CustomerID",by.y="HouseholdID")
Test <- merge(x=Test,y=Test_Churn,by.x = "CustomerID", by.y = "CustomerID")

str(Train)
str(Test)

dim(Test)
dim(Train)


is.na(Train$Churn)
sum(is.na(Train))
is.na(Train)
is.na(Test)
sum(is.na(Test))
nrow(Train)

dim(Train)
dim(Test)

install.packages("ggpubr")

library(dplyr)
library(DMwR)
library(gtools)
library(plyr)
library(ggpubr)


Train[Train=="?"] <- NA
Train[Train=="NA"]<- NA
Train[Train==""]<- NA
Train[Train==" "]<- NA

Test[Test=="?"] <- NA
Test[Test=="NA"]<- NA
Test[Test==""]<- NA
Test[Test==" "]<- NA

sum(is.na(Train))
sum(is.na(Test))

Train$Churn <- as.factor(Train$Churn)

nrow(Test)

# Using Imputation to impute the missing values in Train and Test datasets

Train <- centralImputation(Train)
Test <- centralImputation(Test)

sum(is.na(Train))
sum(is.na(Test))

str(Train)
str(Test)

Train$DeviceProtection <- as.factor(Train$DeviceProtection)
Train$HasPhoneService <- as.factor(Train$HasPhoneService)
Train$InternetServiceCategory <- as.factor(Train$InternetServiceCategory)
Train$MultipleLines <- as.factor(Train$MultipleLines)
Train$OnlineBackup <- as.factor(Train$OnlineBackup)
Train$OnlineSecurity <- as.factor(Train$OnlineSecurity)
Train$StreamingMovies <- as.factor(Train$StreamingMovies)
Train$StreamingTelevision <- as.factor(Train$StreamingTelevision)
Train$TechnicalSupport <- as.factor(Train$TechnicalSupport)


Train$HasPartner <- as.factor(mapvalues(Train$HasPartner,from=c("1","2"), to=c("Yes","No")))
Train$HasDependents <- as.factor(mapvalues(Train$HasDependents,from=c("1","2"), to=c("Yes","No")))
Train$Retired <- as.factor(mapvalues(Train$Retired,from=c("0","1"), to=c("Yes","No")))

Test$DeviceProtection <- as.factor(Test$DeviceProtection)
Test$HasPhoneService <- as.factor(Test$HasPhoneService)
Test$InternetServiceCategory <- as.factor(Test$InternetServiceCategory)
Test$MultipleLines <- as.factor(Test$MultipleLines)
Test$OnlineBackup <- as.factor(Test$OnlineBackup)
Test$OnlineSecurity <- as.factor(Test$OnlineSecurity)
Test$StreamingMovies <- as.factor(Test$StreamingMovies)
Test$StreamingTelevision <- as.factor(Test$StreamingTelevision)
Test$TechnicalSupport <- as.factor(Test$TechnicalSupport)

Test$HasPartner <- as.factor(mapvalues(Test$HasPartner,from=c("1","2"), to=c("Yes","No")))
Test$HasDependents <- as.factor(mapvalues(Test$HasDependents,from=c("1","2"), to=c("Yes","No")))
Test$Retired <- as.factor(mapvalues(Test$Retired,from=c("0","1"), to=c("Yes","No")))

str(Train)
str(Test)

droplevels(Train) -> Train
droplevels(Test) -> Test
levels(Test$PaymentMethod) <- levels(Train$PaymentMethod)

Train <- subset(Train, select=c(-DOC))
Train <- subset(Train, select=c(-DOE))
Train <- subset(Train, select=c(-Country))
Train <- subset(Train, select=c(-State))
Train <- subset(Train,select=c(-CustomerID))

Test <- subset(Test, select=c(-DOC))
Test <- subset(Test, select=c(-DOE))
Test <- subset(Test, select=c(-Country))
Test <- subset(Test, select=c(-State))
Test <- subset(Test,select=c(-CustomerID))

# Testing for correlation between Train attributes

Education <- as.numeric(Train$Education)
Education

Contract_Type <- as.numeric(Train$ContractType)

cor_test1 <- cor.test(Education,Contract_Type,method=c("pearson","kendall","spearman"))
cor_test1

cor_val1 <- cor(Education,Contract_Type)
cor_val1

# Plots

ggscatter(Train,x = "Education",y = "Contract_Type",add = "reg.line",conf.int = TRUE,cor.coef = TRUE,cor.method = "pearson",xlab = "Education level",ylab = "Contract Type")

scatter.smooth(Education,Contract_Type)

cor_test2 <- cor.test(Train$TotalCharges,Contract_Type,method=c("pearson","kendall","spearman"))
cor_test2

cor_val2 <- cor(Train$TotalCharges,Contract_Type)
cor_val2

scatter.smooth(Train$TotalCharges,Contract_Type)


# Logistic Regression model

logistic <- glm(Churn~.,data = Train, family = binomial)

summary(logistic)

predictions_logistic <- predict(logistic,newdata = Test,type = "response")
predictions_logistic <- ifelse(predictions_logistic > 0.5,1,0)
predictions_logistic

predictions_logistic[predictions_logistic==0]="No"
predictions_logistic[predictions_logistic==1]="Yes"

write_logistic_df=predictions_logistic
write_df_cust = Test$CustomerID

write.csv(write_df_cust,'prediction_logit_cust.csv',row.names = F,quote = F)
write.csv(write_logistic_df,'prediction_logit_target.csv',row.names = F,quote = F)

Test <- cbind(Test,Churn=predictions_logistic)

str(Test)

library("caret")

mat_logit = confusionMatrix(Test$Churn,predictions_logistic)
mat_logit

m2 = table(Test$Churn,predictions_logistic)

n = sum(m2)
nc = nrow(m2)
diag = diag(m2)
rowsums = apply(m2,1,sum)
colsums = apply(m2,2,sum)
p = rowsums/n
q = colsums/n

accuracy = sum(diag)/n
accuracy

precision = diag/colsums
recall = diag/rowsums
recall

f1 = 2* precision * recall/(precision + recall)

data.frame(precision,recall,f1)



library("ROCR")


prob_test <- predict(logistic,type = "response")
pred <- prediction(prob_test, Train$Churn)

perf <- performance(pred, measure="tpr", x.measure="fpr")

# Plot the ROC

plot(perf,col=rainbow(10), colorize = T, print.cutoffs.at = seq(0,1,0.05))

# Extract the AUC score of ROC

perf_auc <- performance(pred, measure ="auc")

auc <- perf_auc@y.values[[1]]
auc



# Decision Tree model

library(rpart)
library(rpart.plot)

model_rpart <- rpart(Churn~.,data = Train,method = "class")

summary(model_rpart)

# Plotting the Decision Tree

set.seed(99)
rpart.plot(model_rpart)



# Predictions for Decision trees model

#id <- which(!(Test$CustomerID %in% levels(Train$CustomerID)))
#Test$CustomerID[id] <- NA

predictions_dtree <- predict(model_rpart,newdata = Test, type = "class")
predictions_dtree

# Writing the predictions.csv file

write_df=Test$CustomerID
write_df2=predictions_dtree

write.csv(write_df,'prediction_cust.csv',row.names = F,quote = F)
write.csv(write_df2,'prediction_dtree_target.csv',row.names = F,quote = F)

# Patterns for the Decision Tree model

# install.packages("partykit")
library("partykit")

pathpred <- function(object, ...)
{
  if(!inherits(object, "party")) object <- as.party(object)
  
  rval <- data.frame(response = predict(object, type = "response", ...))
  rval$prob <- predict(object, type = "prob", ...)
  
  rls <- partykit:::.list.rules.party(object)
  
  rval$rule <- rls[as.character(predict(object, type = "node", ...))]
  
  return(rval)
}

pred_path <- pathpred(model_rpart)

pred_path[c(1,51,101),]


# Apriori and Association rules for Decision trees model

library(arules)
library(dummies)

Train_rules <- dummy.data.frame(Train,sep=".")
head(Train_rules)

str(Train_rules)
Train_rules <- data.frame(sapply(Train,as.factor))

rules <- apriori(Train_rules, parameter = list(supp = 0.001,conf=0.5))

head(rules)


rules_conf <- sort(rules, by = "confidence", decreasing=TRUE)

head(rules_conf)

rules_conf

rules_lift <- sort(rules, by = "lift", decreasing=TRUE)

head(rules_lift)

# Printing the rules

RP5_1 <- rpart(Churn ~ ., Train, control = rpart.control(cp = 0.5))
RP5_2 <- rpart(Churn ~ ., Train, control = rpart.control(cp = 0.005))
RP5_3 <- rpart(Churn ~ ., Train, control = rpart.control(cp = 5e-04))

par(mfcol = c(1, 3))
plot(RP5_1, main = "cp=0.1")
text(RP5_1, cex = 0.7)
plot(RP5_2, main = "cp=0.001")
text(RP5_2, cex = 0.7)
plot(RP5_3, main = "cp=0.0001")
text(RP5_3, cex = 0.7)

require(rattle)
library(rattle)

RP5_2
RP5_1

RP5_3


# Computing Metrics for the Decision Trees model

library("caret")

mat1 = confusionMatrix(Test$Churn,predictions_dtree)
mat1
m1=table(Test$Churn,predictions_dtree)

n = sum(m1)
nc = nrow(m1)
diag = diag(m1)
rowsums = apply(m1,1,sum)
colsums = apply(m1,2,sum)
p = rowsums/n
q = colsums/n

accuracy = sum(diag)/n
accuracy

precision = diag/colsums
recall = diag/rowsums
recall

f1 = 2* precision * recall/(precision + recall)

data.frame(precision,recall,f1)


# Random Forest algorithm

library("randomForest")

str(Train)

Train$BaseCharges <- as.factor(Train$BaseCharges)
Train$TotalCharges <- as.factor(Train$TotalCharges)
Train$Retired <- as.factor(Train$Retired)


Train <- subset(Train,select=c(-Country))
Train <- subset(Train,select=c(-State))
Train <- subset(Train,select=c(-DOC))
Train_rf <- subset(Train,select=-c(BaseCharges,TotalCharges))

str(Train_rf)

rf_model <- randomForest(Churn~.,data=Train_rf,ntree=100,mtry=5,importance=TRUE)

summary(rf_model)

Test_rf <- subset(Test,select=-c(BaseCharges,TotalCharges))
Test_rf$Retired <- as.factor(Test_rf$Retired)

str(Train_rf)
str(Test_rf)

Test_rf <- subset(Test_rf,select=c(-Churn))

#levels(Test_rf$PaymentMethod) <- levels(Train_rf$PaymentMethod)

predictions_rf <- predict(rf_model,newdata = Test_rf,type="response")

predictions_rf

write_df=Test$CustomerID
write_df2=predictions_rf
write_df2

write.csv(write_df,'prediction_cust_rf.csv',row.names = F,quote = F)
write.csv(write_df2,'prediction_target_rf.csv',row.names = F,quote = F)

# Metrics for Random forest model

m2 = confusionMatrix(Test_rf$Churn,predictions_rf)

m2
mat2 = table(Test_rf$Churn,predictions_rf)

n = sum(mat2)
nc = nrow(mat2)
diag = diag(mat2)
rowsums = apply(mat2,1,sum)
colsums = apply(mat2,2,sum)
p = rowsums/n
q = colsums/n

accuracy = sum(diag)/n
accuracy

precision = diag/colsums
recall = diag/rowsums
recall

f1 = 2* precision * recall/(precision + recall)

data.frame(precision,recall,f1)


#install.packages("MASS")
#install.packages("cats")
library(MASS)
library(e1071)

str(Train_rf)

svm_model <- svm(Churn~., data=Train_rf)
summary(svm_model)

str(Train_rf)
str(Test_rf)

predictions <- predict(svm_model,Test_rf)
predictions

write_df2=predictions
write_df2

write.csv(write_df2,'prediction_target_svm.csv',row.names = F,quote = F)

# Metrics for SVM model

m3 <- confusionMatrix(predictions,Test_rf$Churn)
m3

mat3 = table(Test_rf$Churn,predictions)

n = sum(mat3)
nc = nrow(mat3)
diag = diag(mat3)
rowsums = apply(mat3,1,sum)
colsums = apply(mat3,2,sum)
p = rowsums/n
q = colsums/n

accuracy = sum(diag)/n
accuracy

precision = diag/colsums
recall = diag/rowsums

f1 = 2* precision * recall/(precision + recall)

data.frame(precision,recall,f1)
