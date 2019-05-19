getwd()
setwd("D:/NEU/Intermediate Analytics/Assignments/Project/Final Project")
banking <- read.csv("banking_data.csv")
str(banking)
summary(banking)
is.factor(banking$marital)
head(banking)

install.packages("Amelia")
library(Amelia)
missmap(banking,main="Missing Values vs Observed", col = c("BLACK","LIGHT BLUE" ), legend= FALSE)

## Finding the count of NA values
sapply(banking,function(x) sum(is.na(x)))

## Finding the count of  Unique values
sapply(banking, function(x) length(unique(x)))

## Replacing NA values with 999
banking$education[is.na(banking$education)]<- 999 ##Replace NA by introducing new integer value: 999
banking$occupation[is.na(banking$occupation)]<- 999 ##Replace NA by introducing new integer value: 999


###Intercept only model
intercept_only_model <- glm(y~1,family = binomial, data = banking)
coef(intercept_only_model)

install.packages("gmodels")
library("gmodels")

##CrossTable
CrossTable(banking$y)


## Finding the probability count for education category
banking$education <- factor(banking$education,labels=c("LOW","INTERMEDIATE","HIGH","UNKNOWN"))
str(banking$education)
levels(banking$education)
education_data<- glm(y~education, data= banking, family='binomial')
summary(education_data)

##y = intercept + b1x1+b2x2+b3x3
y1_value<- -2.34201+0.44785*1 ##high
y2_value<- -2.34201 ##low

##OddsRatio
high_odd<- exp(y1)
low_odd<- exp(y2)
odds_high_l <- h_odd/l_odd
table_Value<- table(banking$y,banking$education, exclude= c("INTERMEDIATE", "UNKNOWN"))
CrossTable(banking$education,banking$y)  


## Finding the probability count for Week category
install.packages("descr")
library(descr)

banking[banking$day==1,]$day<-"Monday"
banking[banking$day==2,]$day<-"Tuesday"
banking[banking$day==3,]$day<-"Wednesday"
banking[banking$day==4,]$day<-"Thursday"
banking[banking$day==5,]$day<-"Friday"
banking$day<-as.factor(banking$day)
str(banking$day)
levels(banking$day)

bankdays_model<-glm(y~day,family = "binomial",data = banking)
summary(bankdays_model)

CrossTable(banking$day,banking$y)

Days <- c("Monday","Tuesday","Wedenesday","Thursday","Friday")
Probabilities <- c(0.1080874,0.1177998,0.1166708,0.1211875,0.1080874)
str(Days)
str(Probabilities)
days_table<-data.frame(days=Days,Prob=Probabilities)

ggplot(days_table,aes(x=Days,y=Probabilities))+geom_bar(stat="identity",fill=c("darkgrey","darkgrey","blue","darkgrey","darkgrey"))+ labs(title="Impact of days on Term Deposit")+xlab("Days in a Week")+ ylab("Probability with Term deposit")

######## Occupation Plot ######

CrossTable(banking$occupation,banking$y)
install.packages("ggplot2")
library(ggplot2)
Occp <- c("A","B","C")
ProbOc <- c(0.075,0.117,0.236)
occupation_table <- data.frame(Occupation= Occp,Probabilitiesofoccupation=ProbOc)

ggplot(occupation_table,aes(x=Occp,y=ProbOc))+geom_bar(stat="identity",fill=c("darkgrey","darkgrey","blue"))+ labs(title="Impact of Occupation on Term Deposit")+xlab("Occupations")+ ylab("Probability with Term deposit")

######## Eduation status Plot ######

CrossTable(banking$education,banking$y)

str(banking$education)
banking$education[is.na(banking$education)]<- 999
banking$education <- factor(banking$education,labels=c("LOW","INTERMEDIATE","HIGH","UNKNOWN"))
str(banking$education)

Edu <- c("High","Low", "Intermediate")
ProbE <- c("0.130","0.087","0.108")
education_table <- data.frame(Education= Edu,ProbabilitiesofEducation=ProbE)
ggplot(education_table,aes(x=Edu,y=ProbE))+geom_bar(stat="identity",fill=c("blue","darkgrey","darkgrey"))+ labs(title="Impact of Eduaction on Term Deposit")+xlab("Education")+ ylab("Probability with Term deposit")

#Logistic Regression for Predicting Term Deposit sign up
set.seed(12345)
split<- sample(seq_len(nrow(banking)),size= floor(0.80*nrow(banking)))
train_data<- banking[split, ]
test_data<- banking[-split, ]
head(train_data)
head(test_data)


banking_model<- glm(y~., data= train_data,family=binomial(link='logit'))

library("MASS")
install.packages("ROCR")
library("ROCR")

test_data$predicted_data<- predict(banking_model,test_data,type='response')
head(test_data$predicted_data, 10)

test_data$predicted_Y_value<- ifelse(test_data$predicted_data>0.5, 1,0)
head(test_data$predicted_Y_value, 20)

test_data$predicted_number<- ifelse(test_data$predicted_data>0.5, 1,0)
test_data$predicted_Y_value<- factor(test_data$predicted_Y_value, levels=c("0","1"),labels=c("no","yes"))  
str(test_data$y)
str(test_data$predicted_Y_value)



confusion_matrix_table<- table(test_data$predicted_Y_value, test_data$y)
(7085+381)/sum(confusion_matrix_table)
install.packages('InformationValue')
library(InformationValue)

mis_clasification_error<- mean(test_data$predicted_Y_value != test_data$y)
model_accuracy<- 1-mis_clasification_error

miss_class_value<- misClassError(as.integer(test_data$y), as.integer(test_data$predicted_Y_value), threshold = 0.5)
accuracy<- 1-miss_class_value

library(ROCR)
predict_model<- predict(banking_model, test_data,type="response")
prediction_value <- prediction(predictions = predict_model,test_data$y)
performance_value<- performance(prediction_value,measure = "tpr",x.measure="fpr")
performance_value
plot(performance_value, main="Performance Ratio")
## AREA UNDER CURVE
area_under_curve<- performance(prediction_value, measure = "auc")
area_under_curve<- area_under_curve@y.values[[1]]
area_under_curve

##ROC
#plotROC(as.integer(test_data$y),test_data$predicted_data)

sensitivity(as.numeric(test_data$y),as.numeric(test_data$predicted_Y_value), threshold = 0.5)

install.packages("pROC")
library("pROC")

x_value<- rnorm(1000)
prediction_made<- exp(5*x_value)/(1+exp(5*x_value))
y_value<- 1 *(runif(1000)< prediction_made)
model_check<- glm(y_value~x_value, family="binomial")
predict_result<- predict(model_check, type= "response")
roc_curve<- roc(y_value~predict_result)
plot(roc_curve, main = "Roc Curve")

second_prediction_made<- exp(0*x_value)/(1+exp(0*x_value))
y2_value<- 1 *(runif(1000)< second_prediction_made)
model_check_2<- glm(y2_value~x, family="binomial")
predict_result_2<- predict(model_check_2, type= "response")
roc_curve_2<- roc(y2_value~predict_result_2)
plot(roc_curve_2, main="Linearity Curve ")


