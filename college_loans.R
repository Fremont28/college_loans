#libraries
library(e1071) 
library(plyr)
library(jtools)
library(ggplot2)
library(mlr)
#create binary 1,0 for credit history
grads$Credit_History=as.factor(grads$Credit_History) 
grads$Gender=ifelse(grads$Gender=="Yes",1,0)
grads$Gender=as.factor(grads$Gender)
grads$Self_Employed=ifelse(grads$Self_Employed=="Yes",1,0)
grads$Self_Employed=as.factor(grads$Self_Employed)
grads$Property_Area=ifelse(grads$Property_Area=="Urban",1,0)
#fit the naive bayes model
nb_model=naiveBayes(Credit_History~Gender+Married+Dependents+Education+
                      Self_Employed,data=grads)
nb_model 
#probability that those that did not graduate have credit history is 0.20 (0.29 no credit history)
count(grads,grads$Credit_History)
#predictions
nb_pred=predict(nb_model,grads)
table(nb_pred,grads$Credit_History) 

#interactions with continuous predictors 
fit_lr=lm(ApplicantIncome~CoapplicantIncome*Education+Married+Credit_History+Dependents*CoapplicantIncome+Education,data=grads)
summ(fit_lr)
summ(fit_lr,scale=TRUE)

#interaction plot between coapplicant income and education
interact_plot(fit_lr,pred="CoapplicantIncome",modx="Education",plot.points=TRUE)+
  xlab("Co-Applicant Income")+ylab("Applicant Income")+ggtitle("Graduating Boosts Income")+
  theme(plot.title = element_text(hjust = 0.5)) 
interact_plot(fit_lr,pred="CoapplicantIncome",modx="Married",plot.points=TRUE)

#income level by school status
education_income=ddply(grads,.(Education),summarize,avg_income=mean(CoapplicantIncome))
education_income

