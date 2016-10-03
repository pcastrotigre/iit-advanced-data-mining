install.packages("randomForest") 
install.packages("rpart") 
install.packages("plyr") 
install.packages("lattice") 
install.packages("car") 
install.packages("rattle")

library(randomForest) 
library(plyr) 
library(lattice)
library(rpart)
library(car)
library(rattle)

data = read.csv("/Users/petter/Dropbox/Documents/MSc/Advanced_Data_Mining/assignment2/Titanic.csv") 
names(data) 
attach(data)
table(data) 


table(Class) 

table(Survived) 

table(Sex) 

table(Age) 

table(Age,Survived) 
table(Class,Survived) 
table(Sex,Survived) 

newgroup = recode(Survived,"'Yes'=1;'No'=0")

barplot(table(Survived, Age), beside=TRUE, legend=levels(Survived), main = 'Age vs Survived') 
barplot(table(Survived, Sex), beside=TRUE, legend=levels(Survived), main = 'Sex vs Survived') 
barplot(table(Survived, Class), beside=TRUE, legend=levels(Survived), main = 'Class vs Survived') 

fit = rpart(Survived ~ Class + Age + Sex)
fit
attributes(fit)

plot(fit, compress=TRUE,uniform=TRUE) 
plot.new() 
text(fit , use.n=T, all=T, cex =.7, pretty=0, xpd=TRUE ) 

drawTreeNodes(fit,cex=.8,pch=11,size=4*.8, col=NULL,nodeinfo=TRUE, units = "", cases =" obs ", digits=getOption("digits"), decimals = 2, print.levels = TRUE, new = TRUE ) 

newdata = data.frame(Class=c("2nd"), Age=c("Child"), Sex=c("Male")) 
newdata
predict(fit, newdata) 

newdata = data.frame(Class=c("1st","2nd","Crew"), Age=c("Child","Adult","Adult"), Sex=c("Female","Male","Male")) 
predict(fit, newdata) 

newdata = subset(data, Survived=="No")
noPredictions = predict(fit, newdata) 
noPredictions  
correct = (noPredictions[,1] > 0.5) 
correct  
table(correct) 

newdata = subset(data, Survived=="Yes")
noPredictions = predict(fit, newdata) 
noPredictions  
correct = (noPredictions[,1] > 0.5) 
correct  
table(correct) 

fit=randomForest(Survived~.,data=data[1:3],ntree=1000, importance=TRUE, proximity = TRUE)
fit
names(fit)
importance(fit)
varImpPlot(fit) 

varImpPlot(fit,main=" Average Importance plots")  
partialPlot(fit, data, svc.calls, "No",main="For the No category") 




