install.packages("randomForest") 
install.packages("rpart") 
install.packages("plyr") 
install.packages("lattice") 
install.packages("ggplot2")

library(randomForest) 
library(plyr) 
library(lattice)
library(ggplot2)

data = read.csv("/Users/petter/Dropbox/Documents/MSc/Advanced_Data_Mining/assignment2/churn.csv") 
names(data) 
attach(data)
table(data) 



hist(area) 
boxplot(area~churn, main = 'Area Distribution') 
tapply(area,churn,each(mean,sd)) 

table(vmail) 

hist(vmail.msgs) 
boxplot(vmail.msgs~churn, main = 'Vmail Msgs') 
tapply(vmail.msgs,churn,each(mean,sd))

hist(day.mins) 
boxplot(day.mins~churn, main = 'Day Mins') 
tapply(day.mins,churn,each(mean,sd,median))

hist(day.calls) 
boxplot(day.calls~churn, main = 'Day Calls') 
tapply(day.calls,churn,each(mean,sd))

hist(day.charge)
boxplot(day.charge~churn, main = 'Day Charge') 
tapply(day.charge,churn,each(mean,sd,median))

hist(eve.mins)
boxplot(eve.mins~churn, main = 'Eve Mins') 
tapply(eve.mins,churn,each(mean,sd))

hist(eve.calls)
boxplot(eve.calls~churn, main = 'Eve Calls') 
tapply(eve.calls,churn,each(mean,sd))

hist(eve.charge)
boxplot(eve.charge~churn, main = 'Eve Charge') 
tapply(eve.charge,churn,each(mean,sd))

hist(night.mins)
boxplot(night.mins~churn, main = 'Night Mins') 
tapply(night.mins,churn,each(mean,sd))

hist(night.calls)
boxplot(night.calls~churn, main = 'Night Calls') 
tapply(night.calls,churn,each(mean,sd))

hist(night.charge)
boxplot(night.charge~churn, main = 'Night Charge') 
tapply(night.charge,churn,each(mean,sd))

hist(intl.mins)
boxplot(intl.mins~churn, main = 'Intl Mins')
tapply(intl.mins,churn,each(mean,sd))

hist(intl.calls)
boxplot(intl.calls~churn, main = 'Intl Calls') 
tapply(intl.calls,churn,each(mean,sd))

hist(intl.charge)
boxplot(intl.charge~churn, main = 'Intl Charge') 
tapply(intl.charge,churn,each(mean,sd))

hist(svc.calls)
boxplot(svc.calls~churn, main = 'Svc Calls')
tapply(svc.calls,churn,each(mean,sd,median))

table(churn) 

splom(data[c(5,1)]) 

t.test(day.charge ~ churn) 

table(vmail)
table(churn, vmail)
prop.table(table(churn, vmail), 2) 

chisq.test(table(churn,vmail))

table(churn) * 0.1

fit=randomForest(churn~.,data=data[2:17], importance=TRUE, proximity = TRUE) 
fit
varImpPlot(fit)

fit2=randomForest(churn~.,data=data[2:17],ntree=1000, importance=TRUE, proximity = TRUE) 
fit2
varImpPlot(fit2)

fit3=randomForest(churn~.,data=data[2:17],ntree=1000, mtry=7, importance=TRUE, proximity = TRUE ) 
fit3
varImpPlot(fit3)

fit4=randomForest(churn~.,data=data[2:17],ntree=1000, mtry=7, nodesize=10, importance=TRUE, proximity = TRUE ) 
fit4
varImpPlot(fit4)




fit5=randomForest(churn~.,data=data[2:17],ntree=1000, mtry=7, importance=TRUE, proximity = TRUE, sampsize = table(churn) * 0.5) 
fit5
varImpPlot(fit5)

fit6=randomForest(churn~.,data=data[c(2:4,6,8:16)],ntree=1000, mtry=7, importance=TRUE, proximity = TRUE, sampsize = table(churn) * 0.5) 
fit6
varImpPlot(fit6)


fit2=randomForest(churn~.,data=data[2:17],ntree=500, importance=TRUE, proximity = TRUE) 
fit2

fit2=randomForest(churn~.,data=data[2:17],ntree=600, importance=TRUE, proximity = TRUE) 
fit2

fit2=randomForest(churn~.,data=data[2:17],ntree=700, importance=TRUE, proximity = TRUE) 
fit2

fit2=randomForest(churn~.,data=data[2:17],ntree=800, importance=TRUE, proximity = TRUE) 
fit2

fit2=randomForest(churn~.,data=data[2:17],ntree=900, importance=TRUE, proximity = TRUE) 
fit2

fit2=randomForest(churn~.,data=data[2:17],ntree=1000, importance=TRUE, proximity = TRUE) 
fit2

fit2=randomForest(churn~.,data=data[2:17],ntree=1100, importance=TRUE, proximity = TRUE) 
fit2

fit2=randomForest(churn~.,data=data[2:17],ntree=1200, importance=TRUE, proximity = TRUE) 
fit2

fit2=randomForest(churn~.,data=data[2:17],ntree=1300, importance=TRUE, proximity = TRUE) 
fit2

fit2=randomForest(churn~.,data=data[2:17],ntree=1400, importance=TRUE, proximity = TRUE) 
fit2

fit2=randomForest(churn~.,data=data[2:17],ntree=1500, importance=TRUE, proximity = TRUE) 
fit2

fit2=randomForest(churn~.,data=data[2:17],ntree=1600, importance=TRUE, proximity = TRUE) 
fit2




fit=randomForest(churn~.,data=data[2:17],ntree=1000, mtry=3, importance=TRUE, proximity = TRUE) 
fit
fit=randomForest(churn~.,data=data[2:17],ntree=1000, mtry=4, importance=TRUE, proximity = TRUE) 
fit
fit=randomForest(churn~.,data=data[2:17],ntree=1000, mtry=5, importance=TRUE, proximity = TRUE) 
fit
fit=randomForest(churn~.,data=data[2:17],ntree=1000, mtry=6, importance=TRUE, proximity = TRUE) 
fit
fit=randomForest(churn~.,data=data[2:17],ntree=1000, mtry=7, importance=TRUE, proximity = TRUE) 
fit
fit=randomForest(churn~.,data=data[2:17],ntree=1000, mtry=8, importance=TRUE, proximity = TRUE) 
fit
fit=randomForest(churn~.,data=data[2:17],ntree=1000, mtry=9, importance=TRUE, proximity = TRUE) 
fit
fit=randomForest(churn~.,data=data[2:17],ntree=1000, mtry=10, importance=TRUE, proximity = TRUE) 
fit
fit=randomForest(churn~.,data=data[2:17],ntree=1000, mtry=11, importance=TRUE, proximity = TRUE) 
fit
fit=randomForest(churn~.,data=data[2:17],ntree=1000, mtry=12, importance=TRUE, proximity = TRUE) 
fit
fit=randomForest(churn~.,data=data[2:17],ntree=1000, mtry=13, importance=TRUE, proximity = TRUE) 
fit
fit=randomForest(churn~.,data=data[2:17],ntree=1000, mtry=14, importance=TRUE, proximity = TRUE) 
fit




fit=randomForest(churn~.,data=data[2:17],ntree=1000, nodesize=2, mtry=7, importance=TRUE, proximity = TRUE) 
fit
fit=randomForest(churn~.,data=data[2:17],ntree=1000, nodesize=4, mtry=7, importance=TRUE, proximity = TRUE) 
fit
fit=randomForest(churn~.,data=data[2:17],ntree=1000, nodesize=6, mtry=7, importance=TRUE, proximity = TRUE) 
fit
fit=randomForest(churn~.,data=data[2:17],ntree=1000, nodesize=8, mtry=7, importance=TRUE, proximity = TRUE) 
fit
fit=randomForest(churn~.,data=data[2:17],ntree=1000, nodesize=10, mtry=7, importance=TRUE, proximity = TRUE) 
fit
fit=randomForest(churn~.,data=data[2:17],ntree=1000, nodesize=12, mtry=7, importance=TRUE, proximity = TRUE) 
fit
fit=randomForest(churn~.,data=data[2:17],ntree=1000, nodesize=14, mtry=7, importance=TRUE, proximity = TRUE) 
fit
fit=randomForest(churn~.,data=data[2:17],ntree=1000, nodesize=16, mtry=7, importance=TRUE, proximity = TRUE) 
fit
fit=randomForest(churn~.,data=data[2:17],ntree=1000, nodesize=18, mtry=7, importance=TRUE, proximity = TRUE) 
fit
fit=randomForest(churn~.,data=data[2:17],ntree=1000, nodesize=20, mtry=7, importance=TRUE, proximity = TRUE) 
fit
fit=randomForest(churn~.,data=data[2:17],ntree=1000, nodesize=22, mtry=7, importance=TRUE, proximity = TRUE) 
fit
fit=randomForest(churn~.,data=data[2:17],ntree=1000, nodesize=24, mtry=7, importance=TRUE, proximity = TRUE) 
fit







fit2=randomForest(churn~.,data=data[2:17],ntree=500, importance=TRUE, proximity = TRUE, sampsize = table(churn) * 0.5) 
fit2
fit2=randomForest(churn~.,data=data[2:17],ntree=600, importance=TRUE, proximity = TRUE, sampsize = table(churn) * 0.5) 
fit2
fit2=randomForest(churn~.,data=data[2:17],ntree=700, importance=TRUE, proximity = TRUE, sampsize = table(churn) * 0.5) 
fit2
fit2=randomForest(churn~.,data=data[2:17],ntree=800, importance=TRUE, proximity = TRUE, sampsize = table(churn) * 0.5) 
fit2
fit2=randomForest(churn~.,data=data[2:17],ntree=900, importance=TRUE, proximity = TRUE, sampsize = table(churn) * 0.5) 
fit2
fit2=randomForest(churn~.,data=data[2:17],ntree=1000, importance=TRUE, proximity = TRUE, sampsize = table(churn) * 0.5) 
fit2
fit2=randomForest(churn~.,data=data[2:17],ntree=1100, importance=TRUE, proximity = TRUE, sampsize = table(churn) * 0.5) 
fit2
fit2=randomForest(churn~.,data=data[2:17],ntree=1200, importance=TRUE, proximity = TRUE, sampsize = table(churn) * 0.5) 
fit2
fit2=randomForest(churn~.,data=data[2:17],ntree=1300, importance=TRUE, proximity = TRUE, sampsize = table(churn) * 0.5) 
fit2
fit2=randomForest(churn~.,data=data[2:17],ntree=1400, importance=TRUE, proximity = TRUE, sampsize = table(churn) * 0.5) 
fit2
fit2=randomForest(churn~.,data=data[2:17],ntree=1500, importance=TRUE, proximity = TRUE, sampsize = table(churn) * 0.5) 
fit2
fit2=randomForest(churn~.,data=data[2:17],ntree=1600, importance=TRUE, proximity = TRUE, sampsize = table(churn) * 0.5) 
fit2



fit2=randomForest(churn~.,data=data[2:17],ntree=700, mtry=3, importance=TRUE, proximity = TRUE, sampsize = table(churn) * 0.5) 
fit2
fit2=randomForest(churn~.,data=data[2:17],ntree=700, mtry=4, importance=TRUE, proximity = TRUE, sampsize = table(churn) * 0.5) 
fit2
fit2=randomForest(churn~.,data=data[2:17],ntree=700, mtry=5, importance=TRUE, proximity = TRUE, sampsize = table(churn) * 0.5) 
fit2
fit2=randomForest(churn~.,data=data[2:17],ntree=700, mtry=6, importance=TRUE, proximity = TRUE, sampsize = table(churn) * 0.5) 
fit2
fit2=randomForest(churn~.,data=data[2:17],ntree=700, mtry=7, importance=TRUE, proximity = TRUE, sampsize = table(churn) * 0.5) 
fit2
fit2=randomForest(churn~.,data=data[2:17],ntree=700, mtry=8, importance=TRUE, proximity = TRUE, sampsize = table(churn) * 0.5) 
fit2
fit2=randomForest(churn~.,data=data[2:17],ntree=700, mtry=9, importance=TRUE, proximity = TRUE, sampsize = table(churn) * 0.5) 
fit2
fit2=randomForest(churn~.,data=data[2:17],ntree=700, mtry=10, importance=TRUE, proximity = TRUE, sampsize = table(churn) * 0.5) 
fit2
fit2=randomForest(churn~.,data=data[2:17],ntree=700, mtry=11, importance=TRUE, proximity = TRUE, sampsize = table(churn) * 0.5) 
fit2
fit2=randomForest(churn~.,data=data[2:17],ntree=700, mtry=12, importance=TRUE, proximity = TRUE, sampsize = table(churn) * 0.5) 
fit2
fit2=randomForest(churn~.,data=data[2:17],ntree=700, mtry=13, importance=TRUE, proximity = TRUE, sampsize = table(churn) * 0.5) 
fit2
fit2=randomForest(churn~.,data=data[2:17],ntree=700, mtry=14, importance=TRUE, proximity = TRUE, sampsize = table(churn) * 0.5) 
fit2




fit2=randomForest(churn~.,data=data[2:17],ntree=700, mtry=9, nodesize=2,importance=TRUE, proximity = TRUE, sampsize = table(churn) * 0.5) 
fit2
fit2=randomForest(churn~.,data=data[2:17],ntree=700, mtry=9, nodesize=4,importance=TRUE, proximity = TRUE, sampsize = table(churn) * 0.5) 
fit2
fit2=randomForest(churn~.,data=data[2:17],ntree=700, mtry=9, nodesize=6,importance=TRUE, proximity = TRUE, sampsize = table(churn) * 0.5) 
fit2
fit2=randomForest(churn~.,data=data[2:17],ntree=700, mtry=9, nodesize=8,importance=TRUE, proximity = TRUE, sampsize = table(churn) * 0.5) 
fit2
fit2=randomForest(churn~.,data=data[2:17],ntree=700, mtry=9, nodesize=10,importance=TRUE, proximity = TRUE, sampsize = table(churn) * 0.5) 
fit2
fit2=randomForest(churn~.,data=data[2:17],ntree=700, mtry=9, nodesize=12,importance=TRUE, proximity = TRUE, sampsize = table(churn) * 0.5) 
fit2
fit2=randomForest(churn~.,data=data[2:17],ntree=700, mtry=9, nodesize=14,importance=TRUE, proximity = TRUE, sampsize = table(churn) * 0.5) 
fit2
fit2=randomForest(churn~.,data=data[2:17],ntree=700, mtry=9, nodesize=16,importance=TRUE, proximity = TRUE, sampsize = table(churn) * 0.5) 
fit2
fit2=randomForest(churn~.,data=data[2:17],ntree=700, mtry=9, nodesize=18,importance=TRUE, proximity = TRUE, sampsize = table(churn) * 0.5) 
fit2
fit2=randomForest(churn~.,data=data[2:17],ntree=700, mtry=9, nodesize=20,importance=TRUE, proximity = TRUE, sampsize = table(churn) * 0.5) 
fit2
fit2=randomForest(churn~.,data=data[2:17],ntree=700, mtry=9, nodesize=22,importance=TRUE, proximity = TRUE, sampsize = table(churn) * 0.5) 
fit2
fit2=randomForest(churn~.,data=data[2:17],ntree=700, mtry=9, nodesize=24,importance=TRUE, proximity = TRUE, sampsize = table(churn) * 0.5) 
fit2


fit2=randomForest(churn~.,data=data[c(4:8,10,17)],ntree=700, nodesize=6,importance=TRUE, proximity = TRUE, sampsize = table(churn) * 0.5) 
fit2
fit2=randomForest(churn~.,data=data[2:17],ntree=700, nodesize=6,importance=TRUE, proximity = TRUE, sampsize = table(churn) * 0.5) 
fit2

fit3=randomForest(churn~.,data=data[c(4:8,10,17)],ntree=1000, mtry=7, importance=TRUE, proximity = TRUE) 
fit3
fit3=randomForest(churn~.,data=data[2:17],ntree=1000, mtry=7, importance=TRUE, proximity = TRUE) 
fit3

varImpPlot(fit2)
varImpPlot(fit3)


fittmp=randomForest(churn~.,data=data[3:17],ntree=1000, mtry=7, importance=TRUE, proximity = TRUE ) 
fittmp
varImpPlot(fittmp)

data[c(2,10:17)]

varImpPlot(fit ,class ="Yes", main =" Class=Yes Importance plots")
varImpPlot(fit ,class ="No", main =" Class=No Importance plots") 
partialPlot(fit, data, svc.calls, "No",main="For the No category") 

margins.rf=margin(fit,churn)
plot(margins.rf)
hist(margins.rf,main="Margins of Random Forest for churn dataset") 
boxplot(margins.rf~data$churn, main="Margins of Random Forest for churn dataset by class")

plot(fit5 , main="Error rate over trees") 
fit3MDSplot(fit2,data$churn, k=2)


