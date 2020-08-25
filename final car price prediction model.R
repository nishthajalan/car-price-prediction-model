
library(readxl)
data=read_excel("C:/Users/lokesh/Desktop/iaqs/iaqs R/carpriceprediction.xlsx")
head(data)
summary(data)
str(data)
help("separate")
#data cleaning and preparation
#fixing invalid vaues in carName
data$CarName=as.character(data$CarName)

#splittng Carname
library(dplyr)
library(tidyr)
head(unique(data$CarName))
car=separate(data,CarName,into=c("CompanyName","carname"),sep=" ")
head(car)
unique(car$CompanyName)
#correcting spelling errors in CompanyName
car$CompanyName[car$CompanyName=="mazda"]="maxda"
car$CompanyName[car$CompanyName=="nissan"]="Nissan"
car$CompanyName[car$CompanyName=="porcshce"]="porsche"
car$CompanyName[car$CompanyName=="toyouta"]="toyota"
car$CompanyName[car$CompanyName=="vw"]="volkswagen"
car$CompanyName[car$CompanyName=="vokswagen"]="volkswagen"
car$CompanyName=as.factor(car$CompanyName)
help("filter")
#checking for corrected values
filter(car,CompanyName=="mazda"& CompanyName =="maxda")
filter(car,car$CompanyName=="vokswagen")

summary(car$carname)
car$carname
#checking for duplicates
car$car_ID[duplicated(car$car_ID)]
#checking missing values
any(is.na(car))
any(is.na(car$carname))
is.na(car$carname) #only two missing values
#removing na

car$carname[is.na(car$carname)]=0
any(is.na(car))

#Data visualization
par(mfrow=c(1,2))
plot(density(car$price,from=0),main="Car Price Distribution plot")
boxplot(car$price, main="Car Price spread")
summary(car$price)
quantile(car$price)

library(ggplot2)
car$CompanyName=as.factor(car$CompanyName)
class(car$CompanyName)
pl=ggplot(car,aes(x=CompanyName))
pl+geom_bar()

pl1=ggplot(car,aes(x=fueltype))
pl1+geom_bar(fill="blue")

pl2=ggplot(car,aes(x=carbody))
pl2+geom_bar(aes(fill=CompanyName))

car$CompanyName=as.character(car$CompanyName)
#enginetype versus price 
pl4=ggplot(car,aes(enginetype,price))
pl4+geom_boxplot(fill="brown")
par(mfrow=c(1,1))
#CompanyName versus average price
df=car%>%group_by(CompanyName)%>%summarise(mean=mean(price)) #using pipe operator 
barplot(df$mean,names.arg = df$CompanyName)

pl5=ggplot(car,aes(doornumber,price))
pl5+geom_boxplot()
#Similarly boxplot on aspiration versus price

help("countplot")
# visualizing numerical data
# checking correlation between numerical variables
plot(car$carlength,car$price)
plot(car$carwidth,car$price, col="red")
par(mfrow=c(1,2))
plot(car$carheight,car$price)
plot(car$curbweight,car$price)


#feature engineering
car$fueleconomy=(0.55*car$citympg)+(0.45*car$highwaympg)

#bivariate analysis
pl6=ggplot(car,aes(x=fueleconomy,y=price))
pl6+geom_point(aes(color=factor(drivewheel)))

library(corrgram)

library(corrplot)
#Understanding the correlation between numeric fields using visualization.
num.cols=sapply(car,is.numeric)
cor.data=cor(car[,num.cols])
round(cor.data,4)
corrplot(cor.data,method="color")

#we can notice the corelation of variables with price
#another function that visualizes the correlation between numeric
#fields with additional features
install.packages("corrgram")
library(corrgram)
corrgram(car,order=TRUE,lower.panel = panel.shade,upper.panel = panel.pie,
         text.panel = panel.txt)

#Model Building
library(caTools)
fields=c('wheelbase', 'curbweight', 'enginesize', 'boreratio',
         'horsepower','fueleconomy','carlength','carwidth','price','enginetype',
        'fueltype','carbody','aspiration','cylindernumber','drivewheel' )
use=car[,fields]
head(use)
num.cols=sapply(use,is.numeric)
cor.data=cor(use[,num.cols])
round(cor.data,4)
corrplot(cor.data,method="color")

library(caTools)
set.seed(101)
sample=sample.split(use$price,SplitRatio = 0.7)
train=subset(use,sample=TRUE)
test=subset(use,sample=FALSE)

model=lm(price~.,data=train)
summary(model)

res=residuals(model)
res=as.data.frame(res)
ggplot(res,aes(res))+geom_histogram(fill="blue",alpha=0.5)

price.predictions=predict(model,test)#this predicts the value
results=cbind(price.predictions,test$price)

colnames(results)=c('Predicted','Actual')
results=as.data.frame(results)
plot(results$Actual,results$Predicted)
head(results,10)
tail(results)
 #errors can also be calculated as follows
residuals=results$Actual-results$Predicted
hist(residuals)

#tests for assumptions
#Durbin-Watson test
install.packages("car",dependencies = TRUE)
library(car)
durbinWatsonTest(model)

#test for heteroskedasticity
#Breusch pagan test 
install.packages("lmtest")
library(lmtest)
bptest(model)


#test for normality of residuals
#shapiro-wilk test 
shapiro.test(model$residuals)

#model 2 using AIC and step()
help("step")
step.model=step(model,scale=0,direction="backward",trace=FALSE)
summary(step.model)

res1=residuals(step.model)
res1=as.data.frame(res1)
ggplot(res1,aes(res1))+geom_histogram(fill="blue",alpha=0.5)

#tests for assumptions
#Durbin-Watson test
install.packages("car",dependencies = TRUE)
library(car)
durbinWatsonTest(step.model)

#test for heteroskedasticity
#Breusch pagan test 
install.packages("lmtest")
library(lmtest)
bptest(step.model)


#test for normality of residuals
#shapiro-wilk test 
shapiro.test(step.model$residuals)

#boxcox transformation is used to change the scales of the variables
#applied to satisfy breusch-pagan test
boxcox=use[,num.cols]
colnames(boxcox)
install.packages("car",dependencies = TRUE)
library(car)
use$wheelbase=bcPower(use$wheelbase,lambda=seq(-2,2,by=0.5))
use$curbweight=bcPower(use$curbweight,lambda=seq(-2,2,by=0.5))
use$enginesize=bcPower(use$enginesize,lambda = seq(-2,2,by=0.5))
use$boreratio=bcPower(use$boreratio,lambda=seq(-2,2,by=0.5))
use$horsepower=bcPower(use$horsepower,lambda=seq(-2,2,by=0.5))
use$fueleconomy=bcPower(use$fueleconomy,lambda=seq(-2,2,by=0.5))
use$carlength=bcPower(use$carlength,lambda=seq(-2,2,by=0.5))
use$carwidth=bcPower(use$carwidth,lambda=seq(-2,2,by=0.5))
use$price=bcPower(use$price,lambda = seq(-2,2,by=0.5))
