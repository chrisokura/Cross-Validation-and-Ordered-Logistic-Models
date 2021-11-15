### Cross Validation of logistic models

library(readxl)
library(lmtest)
library(caret)
# install.packages("e1071")
# install.packages("maxLik")
library(e1071)
library(maxLik)
library(MASS) 
#install.packages("MASS")

myData <- read_excel("~/__Grad School/Data for Weeks 8-9.xlsx", 
                     sheet = "Spam")
head(myData)
options(scipen = 999)

# Split Data into training and test set
TData <- myData[1:375,]
VData <- myData[376:500,]

#Logistic Model 1
Model1 <- glm(Spam ~ Recipients + Hyperlinks + Characters, family = binomial, data = TData); summary(Model1)
Pred1 <- predict(Model1, VData, type = "response")
Binary1 <- round(Pred1); 100*mean(VData$Spam == Binary1)

#Logistic Model 2. Dropping the variable "Character" because it is not significantly significant at the 5% level. 
Model2 <- glm(Spam ~ Recipients + Hyperlinks, family = binomial, data = TData); summary(Model2)
Pred2 <- predict(Model2, VData, type = "response")
Binary2 <- round(Pred2); 100*mean(VData$Spam == Binary2)

# cbind(myData, Pred1, Binary1, Pred2, Binary2)

Modelf <- glm(Spam ~ Recipients + Hyperlinks + Characters, family = binomial, data =
                myData); summary(Modelf)

# predicting the probability of an email being spam with the following characteristics
predict(Modelf, data.frame(Recipients=20, Hyperlinks=5, Characters=60),
        type="response")

#LR test to determine if the individually insignificant variables are jointly significant
lrtest(Model1,Model2)

# Intution: it is better to report the percentage of correctly classified observations for
# both outcomes of the response variable, referred to as the sensitivity and the specificity measures. 

# True positive (TP) is a Class 1 observation that is correctly classified by the model 
# True negative (TN) is a Class 0 observation that is correctly classified by the model
# False positive (FP) is a Class 0 observation that is incorrectly classified as a Class 1
# False negative (FN) is a Class 1 observation that is incorrectly classified as a Class 0


## Printing out confusion matrices and model metrics
confusionMatrix(table(Binary1, VData$Spam), positive="1")
confusionMatrix(table(Binary2, VData$Spam),positive="1")

# The relevance of the performance measures depends on the related misclassification costs. The
# choice of the cutoff value can greatly influence the resulting performance measures. 

# In this example, the proportion on SPAM emails is 0.5253, so we will change the cutoff point to equal 0.5253

# Model 1
Binary1b <- round(Pred1+0.50-mean(TData$Spam))
confusionMatrix(table(Binary1b, VData$Spam), positive="1")
# Accuracy = 0.664; Sensitivity = 0.6885; Specificity = 0.6406

# Model 2
Binary2b <- round(Pred2+0.50-mean(TData$Spam))
confusionMatrix(table(Binary2b, VData$Spam), positive="1")
# Accuracy = 0.656; Sensitivity = 0.6721; Specificity = 0.6406

# Ordered Logistic Regression
myData <- read_excel("~/__Grad School/Data for Weeks 8-9.xlsx", 
                     sheet = "Feedback")
head(myData)


### Ordered Logistic Regression
# MLE from scratch
# Creating binary variables for Personality Type and Feedback
myData$Analyst <- ifelse(myData$Personality=="Analyst",1,0)
myData$Diplomat <- ifelse(myData$Personality=="Diplomat",1,0)
myData$Explorer <- ifelse(myData$Personality=="Explorer",1,0)
myData$Sentinel <- ifelse(myData$Personality=="Sentinel",1,0)
myData$Low <- ifelse(myData$Feedback=="Low",1,0)
myData$Medium <- ifelse(myData$Feedback=="Medium",1,0)
myData$High <- ifelse(myData$Feedback=="High",1,0)

MLf <- function(beta)
{
  y1 <- myData$Low
  y2 <- myData$Medium
  y3 <- myData$High
  mu = beta[1] + beta[2]*myData$Age + beta[3]*myData$Certificates + beta[4]*myData$Analyst +
    beta[5]*myData$Diplomat + beta[6]*myData$Explorer
  gm <- beta[7]
  p1 = 1/(1+exp(mu)); p2 = 1/(1+(exp(mu-gm)))- p1; p3 = 1 - (p1+p2)
  sum(y1*log(p1) + y2*log(p2) + y3*log(p3))
}

my = 0.5*myData$Low + 1*myData$Medium + 2*myData$High
Linear_Model <- lm(my~Age+Certificates+Analyst+Diplomat+Explorer, data = myData)
sv0 <- coef(Linear_Model); sv <- c(sv0,1)
Model <- maxLik(logLik = MLf, start = sv); summary(Model)

# Making predictions for the feedback outcomes for each personality type for a sales
# representative with three certificates and 30 years of age.
bb <- coef(Model); gm <- bb[7]
mu <- bb[1] + bb[2]*30 + bb[3]*3 + bb[4]*1 #Analyst
p1 = 1/(1+exp(mu)); p2 = 1/(1+(exp(mu-gm)))- p1; p3 = 1 - (p1+p2); c(p1,p2,p3)
# 0.2182960 0.4485837 0.3331203
mu <- bb[1] + bb[2]*30 + bb[3]*3 + bb[5]*1 #Diplomat
p1 = 1/(1+exp(mu)); p2 = 1/(1+(exp(mu-gm)))- p1; p3 = 1 - (p1+p2); c(p1,p2,p3)
# 0.2105723 0.4460439 0.3433838
mu <- bb[1] + bb[2]*30 + bb[3]*3 + bb[6]*1 #Explorer
p1 = 1/(1+exp(mu)); p2 = 1/(1+(exp(mu-gm)))- p1; p3 = 1 - (p1+p2); c(p1,p2,p3)
# 0.1609739 0.4180409 0.4209852
mu <- bb[1] + bb[2]*30 + bb[3]*3 #Sentinel
p1 = 1/(1+exp(mu)); p2 = 1/(1+(exp(mu-gm)))- p1; p3 = 1 - (p1+p2); c(p1,p2,p3)
# 0.3775988 0.4354551 0.1869460 

myData$class <- factor(myData$Feedback, levels=c("Low", "Medium", "High"), ordered=TRUE)
Model2 <- polr(class ~ Age+Certificates+Analyst+Diplomat+Explorer, data=myData)
summary(Model2)

predict(Model2, data.frame(Age = 30, Certificates = 3, Analyst = 1, Diplomat = 0, Explorer = 0), type = "probs")
predict(Model2, data.frame(Age = 30, Certificates = 3, Analyst = 0, Diplomat = 1, Explorer = 0), type = "probs")
predict(Model2, data.frame(Age = 30, Certificates = 3, Analyst = 0, Diplomat = 0, Explorer = 1), type = "probs")
predict(Model2, data.frame(Age = 30, Certificates = 3, Analyst = 0, Diplomat = 0, Explorer = 0), type = "probs")


