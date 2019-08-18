# case study 2
# 20190808

emp = read.csv("/Users/wailunchung/Documents/GitHub/DS_6306_case_study_02/CaseStudy2-data.csv",header = TRUE)
summary(emp)
nrow(emp)
ncol(emp)
names(emp)

emp2 <- emp[c(-1,-3)] #exclude 1st and 3rd columns
names(emp2)

# find distribution of categorical variable vs Attrition/salary
library(ggplot2)
library(plyr)
library(dplyr)
# BusinessTravel -y Non-Travel,Travel_Rarely,Travel_Frequently
bt_freq <- ddply(emp, .(emp$BusinessTravel,emp$Attrition), nrow)
names(bt_freq)<-c('BusinessTravel','Attrition','count')
ggplot(bt_freq, aes(x=bt_freq$BusinessTravel,y=count,fill=Attrition)) + geom_bar(stat = "identity")
emp["BusinessTravel_n"]<-0
emp <- within(emp, BusinessTravel_n[BusinessTravel == 'Travel_Rarely'] <- 1)
emp <- within(emp, BusinessTravel_n[BusinessTravel == 'Travel_Frequently'] <- 2)
summary(emp)
#Department - y
dpt_freq <- ddply(emp, .(emp$Department,emp$Attrition), nrow)
names(dpt_freq)<-c('Department','Attrition','count')
ggplot(dpt_freq, aes(x=Department,y=count,fill=Attrition))+ggtitle("Attrition by Department") + geom_bar(stat = "identity") +
  theme(plot.title = element_text(hjust = 0.5))

#EducationField - y
edu_freq <- ddply(emp, .(emp$EducationField,emp$Attrition), nrow)
names(edu_freq)<-c('EducationField','Attrition','count')
ggplot(edu_freq, aes(x=EducationField,y=count,fill=Attrition)) + geom_bar(stat = "identity")+ theme(axis.text.x = element_text(angle = 90, hjust = 1))
#Gender - y
gen_freq <- ddply(emp, .(emp$Gender,emp$Attrition), nrow)
names(gen_freq)<-c('Gender','Attrition','count')
ggplot(gen_freq, aes(x=Gender,y=count,fill=Attrition)) + geom_bar(stat = "identity")

#MaritalStatus -y
mar_freq <- ddply(emp, .(emp$MaritalStatus,emp$Attrition), nrow)
names(mar_freq)<-c('MaritalStatus','Attrition','count')
ggplot(mar_freq, aes(x=MaritalStatus,y=count,fill=Attrition)) + geom_bar(stat = "identity")+ theme(axis.text.x = element_text(angle = 90, hjust = 1))
#OverTime - yes
ovt_freq <- ddply(emp, .(emp$OverTime,emp$Attrition), nrow)
names(ovt_freq)<-c('OverTime','Attrition','count')
ggplot(ovt_freq, aes(x=OverTime,y=count,fill=Attrition)) + geom_bar(stat = "identity")

#boxplot for numeric variable
#DistanceFromHome	Education
#EnvironmentSatisfaction	HourlyRate	JobInvolvement	JobLevel	JobSatisfaction	
#MonthlyIncome	MonthlyRate	NumCompaniesWorked
#PercentSalaryHike	PerformanceRating	RelationshipSatisfaction
#StockOptionLevel	TotalWorkingYears	TrainingTimesLastYear	WorkLifeBalance
#YearsAtCompany	YearsInCurrentRole	YearsSinceLastPromotion	YearsWithCurrManager
boxplot(DistanceFromHome ~ Attrition, data=emp, ylab = "DistanceFromHome")
boxplot(Education ~ Attrition, data=emp, ylab = "Education")
boxplot(EnvironmentSatisfaction ~ Attrition, data=emp, ylab = "EnvironmentSatisfaction")
boxplot(HourlyRate ~ Attrition, data=emp, ylab = "HourlyRate")
boxplot(JobInvolvement ~ Attrition, data=emp, ylab = "JobInvolvement") # N
boxplot(JobLevel ~ Attrition, data=emp, ylab = "JobLevel")
boxplot(JobSatisfaction ~ Attrition, data=emp, ylab = "JobSatisfaction")
boxplot(MonthlyIncome ~ Attrition, data=emp, ylab = "MonthlyIncome")
boxplot(MonthlyRate ~ Attrition, data=emp, ylab = "MonthlyRate")

boxplot(NumCompaniesWorked ~ Attrition, data=emp, ylab = "NumCompaniesWorked")
ggplot(emp, aes(x = factor(1), y = NumCompaniesWorked)) +
  geom_boxplot(width = 0.4, fill = "white") +
  geom_jitter(aes(color = Attrition, shape = Attrition), 
              width = 0.1, size = 1) +
  scale_color_manual(values = c("#00AFBB", "#E7B800")) + 
  labs(x = NULL)   # Remove x axis label


boxplot(PercentSalaryHike ~ Attrition, data=emp, ylab = "PercentSalaryHike") #N
ggplot(emp, aes(x = factor(1), y = PercentSalaryHike)) +
  geom_boxplot(width = 0.4, fill = "white") +
  geom_jitter(aes(color = Attrition, shape = Attrition), 
              width = 0.1, size = 1) +
  scale_color_manual(values = c("#00AFBB", "#E7B800")) + 
  labs(x = NULL)   # Remove x axis label

boxplot(PerformanceRating ~ Attrition, data=emp, ylab = "PerformanceRating") # 
per_freq <- ddply(emp, .(emp$PerformanceRating,emp$Attrition), nrow)
per_freq
109/(109+23)
621/(621+117)
ggplot(emp, aes(x = factor(1), y = PerformanceRating)) +
  geom_boxplot(width = 0.4, fill = "white") +
  geom_jitter(aes(color = Attrition, shape = Attrition), 
              width = 0.1, size = 1) +
  scale_color_manual(values = c("#00AFBB", "#E7B800")) + 
  labs(x = NULL)   # Remove x axis label
boxplot(RelationshipSatisfaction ~ Attrition, data=emp, ylab = "RelationshipSatisfaction")
boxplot(StockOptionLevel ~ Attrition, data=emp, ylab = "StockOptionLevel")
boxplot(TotalWorkingYears ~ Attrition, data=emp, ylab = "TotalWorkingYears")
boxplot(TrainingTimesLastYear ~ Attrition, data=emp, ylab = "TrainingTimesLastYear")
boxplot(WorkLifeBalance ~ Attrition, data=emp, ylab = "WorkLifeBalance")

boxplot(YearsAtCompany ~ Attrition, data=emp, ylab = "YearsAtCompany")
ggplot(emp, aes(x = factor(1), y = YearsAtCompany)) +
  geom_boxplot(width = 0.4, fill = "white") +
  geom_jitter(aes(color = Attrition, shape = Attrition), 
              width = 0.1, size = 1) +
  scale_color_manual(values = c("#00AFBB", "#E7B800")) + 
  labs(x = NULL)   # Remove x axis label
boxplot(YearsInCurrentRole ~ Attrition, data=emp, ylab = "YearsInCurrentRole")
boxplot(YearsSinceLastPromotion ~ Attrition, data=emp, ylab = "YearsSinceLastPromotion")
boxplot(YearsWithCurrManager ~ Attrition, data=emp, ylab = "YearsWithCurrManager")

emp7_box <- melt(emp7, id.var = "Attrition")
emp7_box
p <- ggplot(data = emp7_box, aes(x=variable, y=value)) + geom_boxplot(aes(fill=Attrition))
p + facet_wrap( ~ variable, scales="free")



#************************************************
############ Cleaning emp data #############
emp3 <-emp
nums <-unlist(lapply(emp3, is.numeric)) # numeric only col
emp4 <-emp3[,nums] # include numeric
emp5 <-emp4[,-1] # exclude ID
emp6 <-emp5[, -which(names(emp5) %in% c("EmployeeCount","StandardHours"))]
emp6 <-emp6[, -which(names(emp6) %in% c("JobLevel","TotalWorkingYears","PerformanceRating","YearsAtCompany","YearsInCurrentRole","YearsWithCurrManager"))]
emp7 <-emp6
emp7$Attrition<-emp3$Attrition
# exclude JobLevel(corelate to monthlyIncome),"TotalWorkingYears"YearsAtCompany(corelate to age,YearsInCurrentRole,etc),
# exclude ID,"EmployeeCount" 
names(emp7)
emp7$Attrition


#Department - y
dpt_freq <- ddply(emp, .(emp$Department,emp$Attrition), nrow)
names(dpt_freq)<-c('Department','Attrition','count')
ggplot(dpt_freq, aes(x=Department,y=count,fill=Attrition))+ggtitle("Attrition by Department") + geom_bar(stat = "identity") +
  theme(plot.title = element_text(hjust = 0.5))

#JobRole -y
rol_freq <- ddply(emp, .(emp$JobRole,emp$Attrition), nrow)
names(rol_freq)<-c('JobRole','Attrition','count')
ggplot(rol_freq, aes(x=JobRole,y=count,fill=Attrition)) +
  ggtitle("Attrition by Job Role")+ 
  geom_bar(stat = "identity")+ 
  theme(axis.text.x = element_text(angle = 90, hjust = 1),plot.title = element_text(hjust = 0.5))



# check correlation between numeric variables
library(corrplot)
correlations<-cor(emp7)
corrplot(correlations,method="circle")

#Boxplots
emp7_box <- melt(emp7, id.var = "Attrition")
p <- ggplot(data = emp7_box, aes(x=variable, y=value)) + geom_boxplot(aes(fill=Attrition))
p + facet_wrap( ~ variable, scales="free")



############### set sample size ###############
sample_size <- floor(0.60 * nrow(emp7))
set.seed(123)

###### set 60/40 sample #####
train_ind <- sample(seq_len(nrow(emp7)), size = sample_size)
train_emp7 <- emp7[train_ind, ]
test_emp7 <- emp7[-train_ind, ]
#x <- train_emp7[c(-22)] #exclude monthly income
#y <- train_emp7$Attrition
#y
test_emp7

################### naive bayes:Attrition ##################
library(e1071)
nb_classifier<-naiveBayes(train_emp7,train_emp7$Attrition)
predicted<-predict(nb_classifier,test_emp7)
confusionMatrix(predicted, test_emp7$Attrition)

#Reference
#Prediction  No Yes
#No  291   1
#Yes   0  56

#Accuracy : 0.9971          
#95% CI : (0.9841, 0.9999)
#No Information Rate : 0.8362          
#P-Value [Acc > NIR] : <2e-16          

#Kappa : 0.9894          

#Mcnemar's Test P-Value : 1               
                                          
#            Sensitivity : 1.0000          
#            Specificity : 0.9825     

########### logistic

var=''
for (i in (2:ncol(emp7)-1)){
  #print(names(train_emp7)[i])
  print(i)
  print(names(train_emp7)[i])
  if (i != 1){
   var = paste(var,names(train_emp7)[i],sep='+')
  } else {var=paste(var,names(train_emp7)[i])}
}
var=paste('Attrition~',var)
fit_logit<-glm(var,data=train_emp7,family=binomial(logit))
#predict_logit_train_n <- predict(fit_logit, newdata = train_emp7, type = "response")
#predict_logit_train <- as.factor(ifelse(predict_logit_train_n > 0.5, 'Yes', 'No'))
#confusionMatrix(predict_logit_train, train_emp7$Attrition)
predict_logit_test_n <- predict(fit_logit, newdata = test_emp7, type = "response")
predict_logit_test <- as.factor(ifelse(predict_logit_test_n > 0.5, 'Yes', 'No'))
confusionMatrix(predict_logit_test, test_emp7$Attrition)

#Reference
#Prediction  No Yes
#No  281  42
#Yes  10  15

#Accuracy : 0.8506          
#95% CI : (0.8087, 0.8863)
#No Information Rate : 0.8362          
#P-Value [Acc > NIR] : 0.2603          

#Kappa : 0.2955          

#Mcnemar's Test P-Value : 1.716e-05       
                                          
#            Sensitivity : 0.9656          
#            Specificity : 0.2632  





#******************************************

############ Cleaning emp data ###########
# do not remove job level because we are predicting income. 
emp_income <-emp5[, -which(names(emp5) %in% c("EmployeeCount","StandardHours"))]
# emp_income <-emp_income[, -which(names(emp_income) %in% c("TotalWorkingYears","PerformanceRating","YearsAtCompany","YearsInCurrentRole","YearsWithCurrManager"))]
# include more columns, we will run by stepwise, forward, backward selection
#names(emp_income)

############### set sample size ###############
sample_size <- floor(0.60 * nrow(emp_income))
set.seed(123)

###### set 60/40 sample #####
train_ind <- sample(seq_len(nrow(emp_income)), size = sample_size)
train_emp_income <- emp_income[train_ind, ]
test_emp_income <- emp_income[-train_ind, ]

############## Regression: Predict Monthly Income ###################
library(MASS)
# full regression model
full.model <- lm(MonthlyIncome ~., data = train_emp_income)
summary(full.model)
# Stepwise regression model
step.model <- stepAIC(full.model, direction = "both", trace = FALSE)
forward.model <- stepAIC(full.model, direction = "forward", trace = FALSE)
backward.model <- stepAIC(full.model, direction = "backward", trace = FALSE)
summary(step.model) #Adjusted R-Squared=0.918
summary(forward.model) #Adjusted R-Squared=0.9168
summary(backward.model) #Adjusted R-Squared=0.918

step.RMSE <- sqrt(c(crossprod(step.model$residuals)) / length(step.model$residuals))
step.RMSE #1327.915
forward.RMSE <- sqrt(c(crossprod(forward.model$residuals)) / length(forward.model$residuals))
forward.RMSE #1315.878
backward.RMSE <- sqrt(c(crossprod(backward.model$residuals)) / length(backward.model$residuals))
backward.RMSE #1327.878

mean(step.model$residuals^2) #1837936
mean((test_emp6$MonthlyIncome - predict.lm(full.model, test_emp6)) ^ 2) #2260826
mean(forward.model$residuals^2) #1809130
mean((test_emp6$MonthlyIncome - predict.lm(forward.model, test_emp6)) ^ 2) #2260826
mean(backward.model$residuals^2) #1837936
mean((test_emp6$MonthlyIncome - predict.lm(backward.model, test_emp6)) ^ 2) #2185442

# use forward selection
##plotting the model fit
par(mfrow=c(2,2))
plot(forward.model, which=c(1:3))

##Store studentized residuals
res_emp <- rstudent(forward.model)
##Histogram
hist(res_emp, freq=FALSE, main="Distribution of Studentized Residuals",xlab="Studentized Residuals", ylab="Density")
##Create range of x-values for normal curve
xfit2 <- seq(min(res_emp)-1, max(res_emp)+1, length=40)

##Generate values from the normal distribution at the specified values
yfit2 <- (dnorm(xfit2))

##Add the normal curve
lines(xfit2, yfit2, ylim=c(0,0.5))

pairs(emp_income)

#################












fit3=knnreg(x= x,y= y,k= 3)
MonthlyIncome_pred3 <- predict(fit3,test_emp6[c(-10)] )
MSPE_knnreg3<-mean((test_emp6$MonthlyIncome-MonthlyIncome_pred3)^2)
MSPE_knnreg3
summary(fit3)

fit5=knnreg(x= x,y= y,k= 5)
MonthlyIncome_pred5 <- predict(fit3,test_emp6[c(-10)] )
MSPE_knnreg5<-mean((test_emp6$MonthlyIncome-MonthlyIncome_pred5)^2)
MSPE_knnreg5

emp8$Mo <- test_emp6$MonthlyIncome
emp8$pred <- MSPE_knnreg3
nrow(emp7)

fit5=knnreg(x= TrainingTX[,c(1,2)],y= TrainingTX$ABV,k= 5)
knnreg.predict5 <- predict(fit5,TestTX[,c(1,2)])

mean((test_emp6$MonthlyIncome_pred3 - predict.lm(fit3, test_emp6)) ^ 2)
confusionMatrix(pred, test_emp$MonthlyIncome)
test_emp6$MonthlyIncome_pred3

train_control <- trainControl(method = "cv", number = 10)
# train model
nb.m2 <- train(x = x, y = y,method = "nb",trControl = train_control)
# results
#confusionMatrix(nb.m1)
pred <- predict(nb.m2, newdata = test_emp)
confusionMatrix(pred, test_emp$MonthlyIncome)


##%%%%%%%%%%%%%%%% 
# set up tuning grid
search_grid <- expand.grid(
  usekernel = c(TRUE, FALSE),
  fL = 0:5,
  adjust = seq(0, 5, by = 1)
)

# train model
nb.m2 <- train(x = x,y = y,method = "nb",trControl = train_control,tuneGrid = search_grid,preProc = c("BoxCox", "center", "scale", "pca"))

# top 5 modesl
nb.m2$results %>% 
  top_n(5, wt = Accuracy) %>%
  arrange(desc(Accuracy))
##   usekernel fL adjust  Accuracy     Kappa AccuracySD   KappaSD
## 1      TRUE  1      3 0.8737864 0.4435322 0.02858175 0.1262286
## 2      TRUE  0      2 0.8689320 0.4386202 0.02903618 0.1155707
## 3      TRUE  2      3 0.8689320 0.4750282 0.02830559 0.0970368
## 4      TRUE  2      4 0.8689320 0.4008608 0.02432572 0.1234943
## 5      TRUE  4      5 0.8689320 0.4439767 0.02867321 0.1354681

# plot search grid results
plot(nb.m2)

library(plotly)




################### naive bayes:Attrition ##################
library(caret)
train_control <- trainControl(method = "cv", number = 10)
# train model
fit1 <- train(x = x, y = y,method = "nb",trControl = train_control)

# distribution of Attrition rates across train & test set
table(train_emp7$Attrition) %>% prop.table()

# results
confusionMatrix(nb.m1)
pred <- predict(nb.m1, newdata = test_emp7)
confusionMatrix(pred, test_emp7$Attrition)
warnings()
###################

library(caret)

results=class::knn(train_emp[,c(1,2)],test_emp[,c(1,2)],train_emp$Style,k=3)
test_emp$StylePred=results
confusionMatrix(table(TestTX2$Style,TestTX2$StylePred))


fit3=knnreg(x= train_emp[,c(1,10)],y= train_emp$ABV,k= 3)
knnreg.predict3 <- predict(fit3,TestTX[,c(1,2)])

fit5=knnreg(x= TrainingTX[,c(1,2)],y= TrainingTX$ABV,k= 5)
knnreg.predict5 <- predict(fit5,TestTX[,c(1,2)])