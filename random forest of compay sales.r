##importing company data set
company=read.csv(file.choose())
View(company)
company$Sales=cut(company$Sales,c(0,5,10,15),labels = c('low','avg','high'))
View(company)
##performing some EDA tequnics
table(company$Sales)
summary(company)
plot(company)
boxplot(company)
sum(is.na(company))
##visuvalization using density plot
library(ggplot2)
ggplot(data=company,aes(x =company$Sales, fill = company$Sales)) +
  geom_density(alpha = 0.9, color = 'black')+ 
  theme(panel.background = element_rect(fill = 'peachpuff'))+
  labs(title = 'sales variable in company datas set')+
  theme(plot.title = element_text(hjust = 0.5),plot.background = 
          element_rect('aquamarine4'))
###
ggplot(data=company,aes(x=company$CompPrice,fill=company$Sales))+
  geom_density(alpha=0.9,color='black')+
  theme(panel.background = element_rect(fill = 'peachpuff'))+
  labs(title = 'company data for comprice varable')+
  theme(plot.title = element_text(hjust = 0.5),plot.background = 
          element_rect('aquamarine4'))
##
ggplot(data=company,aes(x=company$Income,fill=company$Sales))+
  geom_density(alpha=0.9,color='black')+
  theme(panel.background = element_rect(fill = 'peachpuff'))+
  labs(title = 'company data for income varable')+
  theme(plot.title = element_text(hjust = 0.5),plot.background = 
          element_rect('aquamarine4'))
##
ggplot(data=company,aes(x=company$Advertising,fill=company$Sales))+
  geom_density(alpha=0.9,color='black')+
  theme(panel.background = element_rect(fill = 'peachpuff'))+
  labs(title = 'company data for Advertising varable')+
  theme(plot.title = element_text(hjust = 0.5),plot.background = 
          element_rect('aquamarine4'))
##
ggplot(data=company,aes(x=company$Population,fill=company$Sales))+
  geom_density(alpha=0.9,color='black')+
  theme(panel.background = element_rect(fill = 'peachpuff'))+
  labs(title = 'company data for population varable')+
  theme(plot.title = element_text(hjust = 0.5),plot.background = 
          element_rect('aquamarine4'))
##
ggplot(data=company,aes(x=company$Price,fill=company$Sales))+
  geom_density(alpha=0.9,color='black')+
  theme(panel.background = element_rect(fill = 'peachpuff'))+
  labs(title = 'company data for price varable')+
  theme(plot.title = element_text(hjust = 0.5),plot.background = 
          element_rect('aquamarine4'))
##
ggplot(data=company,aes(x=company$ShelveLoc,fill=company$Sales))+
  geom_density(alpha=0.9,color='black')+
  theme(panel.background = element_rect(fill = 'peachpuff'))+
  labs(title = 'company data for Shelveloc varable')+
  theme(plot.title = element_text(hjust = 0.5),plot.background = 
          element_rect('aquamarine4'))
##
ggplot(data=company,aes(x=company$Age,fill=company$Sales))+
  geom_density(alpha=0.9,color='black')+
  theme(panel.background = element_rect(fill = 'peachpuff'))+
  labs(title = 'company data for age varable')+
  theme(plot.title = element_text(hjust = 0.5),plot.background = 
          element_rect('aquamarine4'))
##
ggplot(data=company,aes(x=company$Education,fill=company$Sales))+
  geom_density(alpha=0.9,color='black')+
  theme(panel.background = element_rect(fill = 'peachpuff'))+
  labs(title = 'company data for Education varable')+
  theme(plot.title = element_text(hjust = 0.5),plot.background = 
          element_rect('aquamarine4'))
##
ggplot(data=company,aes(x=company$Urban,fill=company$Sales))+
  geom_density(alpha=0.9,color='black')+
  theme(panel.background = element_rect(fill = 'peachpuff'))+
  labs(title = 'company data for urban varable')+
  theme(plot.title = element_text(hjust = 0.5),plot.background = 
          element_rect('aquamarine4'))
##
ggplot(data=company,aes(x=company$US,fill=company$Sales))+
  geom_density(alpha=0.9,color='black')+
  theme(panel.background = element_rect(fill = 'peachpuff'))+
  labs(title = 'company data for US varable')+
  theme(plot.title = element_text(hjust = 0.5),plot.background = 
          element_rect('aquamarine4'))
##handling missing values or  data
summary(company)
p=function(x){sum(is.na(x))/length(x)*100}
p
apply(company, 2,p)
library(mice)
library(VIM)
md.pattern(company)
md.pairs(company)
marginplot(company[,c("Sales","Price")])
##impute
impute=mice(company,m=3,seed = 123)
print(impute)
impute$imp$Sales
company[1]
summary(company$Sales)
#complete data
company1=complete(impute,1)
company1
company1[1]
#distribution of observed /imputed values
stripplot(impute,pch=20,cex=1.2)
xyplot(impute,Sales~Price,pch=20,cex=1)
View(company)
View(company1)
#spiltting data
set.seed(1234)
id=sample(2,nrow(company1),prob = c(0.8,0.2),replace = T)
training=company1[id==1,]
testing=company1[id==2,]
library(randomForest)
str(company1)
com=randomForest(Sales~.,data =company1)
com
pred=predict(com,newdata = training,type = 'class')
pred
pred1=predict(com,newdata = testing,type = 'class')
pred1
library(caret)
con1=confusionMatrix(table(pred,training$Sales))
con1
con=confusionMatrix(table(pred1,testing$Sales))
con
varImpPlot(com)
plot(com, lwd=1)
err=legend("topright", colnames(com$err.rate), col=1:4, cex=0.8,fill=1:4)
err
