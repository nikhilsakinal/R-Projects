#Final Project on movies data set
getwd()
setwd("E:\\NIKHIL\\CPS\\Intermediate Analytics\\Manseau Joseph\\Project data")
getwd()

library(tidyverse)

#Importing data into R
movies<-read_csv(file.choose())
movies

#Looking for structure of data
str(movies)

#Renaming column for better understanding of data
colnames(movies)<-c("Day_of_Week","Director","Genre","Movie_Title","Release_Date",
                    "Studio","Adjusted_Gross_millions","Budget","Gross_money_millions","IMDB_Rating","MovieLens_Rating",
                    "Overseas_money_millions","Overseas_percent","Profit_millions","Profit_Percent","Runtime","US_millions","Gross_percent")
movies

#Using required column for further analysis
mydata<-movies[,c("Day_of_Week","Director","Genre","Movie_Title","Studio","Budget","Gross_money_millions","IMDB_Rating",
                  "Overseas_money_millions","Overseas_percent","Profit_millions","Profit_Percent","Runtime","US_millions","Gross_percent")]
mydata
-------------------------------------------------------------------------------------------------------------------

attach(mydata)

#Summary of 5 numbers
summary(mydata)

------------------------------------------------------------------------------------------------------------------------
#Exploratory data analysis

#Q1 Which movie has Gross income greater than 1350 million dollars?
mydata[mydata$Gross_money_millions>1350,]

#Q2 How many movies have been released on Friday?
friday<-mydata[mydata$Day_of_Week=="Friday",]
friday

#Q3 What would be the average gross in millions releasing on Friday?
friday_mean<-mean(friday$Gross_money_millions)
friday_mean

#Q4 How many movies have a runtime more than 150 minutes?
runtime_150<-mydata[mydata$Runtime>150,]
runtime_150

#Q5 Find the movie with highest budget 
filter(mydata,Budget==max(Budget))

#Q6 What would be the average gross money for each genre?
by_genre <- group_by(mydata, Genre)
by_genre
summarize(by_genre,mean_genre=mean(Gross_money_millions))

#Q7 What would be the average overseas business in million dollars?
by_genre_overseas <- group_by(mydata, Genre)
by_genre_overseas
summarize(by_genre,mean_genre=mean(Overseas_money_millions))%>%
  ggplot(aes(y=mean_genre,x=Genre, fill=Genre))+geom_col()+
  xlab("Genre") + ylab("Overseas money in millions")+
  ggtitle("Average overseas business of all genre")+
  theme(plot.title = element_text(hjust = 0.5),
        axis.title = element_text(colour = "Blue", size=15),
        axis.title.y=element_text(color="Red",size=15),
        axis.text.x=element_text(size=15, angle = 20),
        axis.text.y=element_text(size=15),
        legend.title=element_text(size=15),
        legend.text=element_text(size=15),
        legend.position=("right"),
        legend.justification=c(1,1))


#Q8 What is the average budget of all studios?
by_studio <- group_by(mydata, Studio)
by_studio
summarize(by_studio ,mean_studio=mean(Budget)) %>%
  ggplot(aes(y=mean_studio,x=Studio, fill=Studio))+geom_col()+
  xlab("Studios") + ylab("Budget in millions")+
  ggtitle("Average budget of all studios")+
  theme(plot.title = element_text(hjust = 0.5),
        axis.title = element_text(colour = "Blue", size=15),
        axis.title.y=element_text(color="Red",size=15),
        axis.text.x=element_text(size=10, angle = 20),
        axis.text.y=element_text(size=10),
        legend.title=element_text(size=10),
        legend.text=element_text(size=10),
        legend.position=("right"),
        legend.justification=c(1,1))

#IMDB Rating of movies as per Genre
IMDB_Rate<-ggplot(data=mydata,aes(x=IMDB_Rating, fill=Genre))
IMDB_Rate
IMDB_Rate+geom_freqpoly(aes(color=Genre))+ facet_wrap(~Genre,nrow=3, scales = "free")


IMDB_Rate+geom_freqpoly(aes(color=Genre))+ facet_wrap(~Genre,nrow=3, scales = "free")+
  xlab("IMDB Rating")+ylab("Number of movies")+
  ggtitle("IMDB Rating of movies as per Genre")+
  theme(plot.title = element_text(hjust = 0.5),
        axis.title = element_text(colour = "Blue", size=15),
        axis.title.y=element_text(color="Red",size=15),
        axis.text.x=element_text(size=10, angle = 20),
        axis.text.y=element_text(size=10),
        legend.title=element_text(size=15),
        legend.text=element_text(size=15),
        legend.position=("right"),
        legend.justification=c(1,1))

-------------------------------------------------------------------------------------------------------------------------
#Boxplot of budget in millions as per day of week
z<-ggplot(data=mydata,aes(x=Day_of_Week,y=Budget, fill=Day_of_Week))
z
z+geom_boxplot()

z+geom_boxplot()+
  xlab("Day of the week")+ylab("Budget in Millions")+
  ggtitle("Movie Budget Distribution")+
  theme(plot.title = element_text(hjust = 0.5),
        axis.title = element_text(colour = "Blue", size=15),
        axis.title.y=element_text(color="Red",size=15),
        axis.text.x=element_text(size=15),
        axis.text.y=element_text(size=15),
        legend.title=element_text(size=15),
        legend.text=element_text(size=15),
        legend.position=("right"),
        legend.justification=c(1,1))


#Boxplot of profit in millions as per day of week
profit<-ggplot(data=mydata,aes(x=Day_of_Week,y=Profit_millions, fill=Day_of_Week))
profit
profit+geom_boxplot()

profit+geom_boxplot()+
  xlab("Day of the week")+ylab("Profit in Millions")+
  ggtitle("Movie Profit Distribution")+
  theme(plot.title = element_text(hjust = 0.5),
        axis.title = element_text(colour = "Blue", size=15),
        axis.title.y=element_text(color="Red",size=15),
        axis.text.x=element_text(size=15),
        axis.text.y=element_text(size=15),
        legend.title=element_text(size=15),
        legend.text=element_text(size=15),
        legend.position=("right"),
        legend.justification=c(1,1))

#Boxplot of runtime as per Genre
runtime<-ggplot(data=mydata,aes(x=Genre,y=Runtime, fill=Genre))
runtime
runtime+geom_boxplot()

runtime+geom_boxplot()+
  xlab("Genre of movie")+ylab("Runtime of movie")+
  ggtitle("Movie runtime as per genre")+
  theme(plot.title = element_text(hjust = 0.5),
        axis.title = element_text(colour = "Blue", size=15),
        axis.title.y=element_text(color="Red",size=15),
        axis.text.x=element_text(size=15, angle = 20),
        axis.text.y=element_text(size=15),
        legend.title=element_text(size=15),
        legend.text=element_text(size=15),
        legend.position=("right"),
        legend.justification=c(1,1))
  
---------------------------------------------------------------------------------------------------------

#straight line regression of overseas and Us millions
plot(Overseas_money_millions~US_millions,main="Regression overseas and US millions",
     col.main="Blue", xlab="US Millions", ylab="Overseas millions")
abline(lm(Overseas_money_millions~US_millions))
globe.lm<-lm(Overseas_money_millions~US_millions)
globe.lm

------------------------------------------------------------------------------------------------------------------------
#Normal probability plots 

qqnorm(Budget, main="Budget", col.main="red", xlab = "",
         ylab = "Budget in Million",col.axis="brown")
qqline(Budget,col="purple")

--------------------------------------------------------------------------------------------------------------
#Hypothesis testing
movies_budget<-mydata$Budget
movies_budget
#Average budget of movies igreater than 90 million
# Ho: The average budget of all movies in millions >= 90
# Ha: The average budget of all movies in millions < 90
#p-value:0.05

t.test(movies_budget,mu=90,alternative="less",conf.level=0.95)

# As p-value is greater than significance level, we will not reject null hypothesis
--------------------------------------------------------------------------------------------------------------------------
# Two sample t-test
#We want to claim movie earning in overseas and in usa is same
#Ho: profit_action_movie - profit_adventure_movie = 0
#Ha: profit_action_movie - profit_adventure_movie != 0
#p-value:0.05
  
action_movie<-mydata[mydata$Genre=="action",]
action_movie

adventure_movie<-mydata[mydata$Genre=="adventure",]
adventure_movie

t.test(action_movie$Profit_millions,adventure_movie$Profit_millions, alternative = "two.sided")
#As p-value is less than significance level, we will reject null hypothesis
-------------------------------------------------------------------------------------------------------------------------------
#F-test
#We want to test variance in action and adventure movie
#Ho: profit_action_movie^2 != profit_adventure_movie^2
#Ha: profit_action_movie^2 = profit_adventure_movie^2
#p-value:0.05
  
action_movie<-mydata[mydata$Genre=="action",]
action_movie

adventure_movie<-mydata[mydata$Genre=="adventure",]
adventure_movie

var.test(action_movie$Profit_millions,adventure_movie$Profit_millions)
# As p-value is greater than significance level, we will not reject null hypothesis

-------------------------------------------------------------------------------------------------------------------------------------------
#Regression
library(ncvreg)
library(bigmemory)
library(biglasso)
library(lars)
library(glmnet)

#ridge regression on IMDB rating
y<-mydata$IMDB_Rating
y

y.train<-y[c(1:500)]
y.train

y.test<-y[c(501:608)]
y.test

x<-as.matrix(mydata[,c("Overseas_money_millions","Profit_millions","US_millions")])
x

x.train<-x[c(1:500),c("Overseas_money_millions","Profit_millions","US_millions")]
x.train

x.test<-x[c(501:608),c("Overseas_money_millions","Profit_millions","US_millions")]
x.test

lambdas<-10^seq(3,-2,by=-0.1)
lambdas

#Training model
alpha0_mydata<-glmnet(x.train,y.train,alpha = 0)
alpha0_mydata

#summary of training model
summary(alpha0_mydata)
plot(alpha0_mydata)

#cross validation
alpha0_mydata_fit<-cv.glmnet(x.train, y.train, type.measure="mse", 
                             alpha=0, family="gaussian")
alpha0_mydata_fit

#summary of cross validation
summary(alpha0_mydata_fit)
plot(alpha0_mydata_fit)

# Minimum value of lambda
alpha0_mydata_fit$lambda.min

#prediction on test model
alpha0_mydata_predicted<-predict(alpha0_mydata_fit,s=alpha0_mydata_fit$lambda.min, newx = x.test)
alpha0_mydata_predicted

mean((y.test-alpha0_mydata_predicted)^2)

-----------------------------------------------------------------------------------------------------------------------------------
#lasso regression
# we ahve used vector y, y.train. y.test, x, x.train and x.train from above ridge regression code

#Training model
alpha1_mydata<-glmnet(x.train,y.train,alpha = 1)
alpha1_mydata

#summary of training model
summary(alpha1_mydata)
plot(alpha1_mydata)

#cross validation
alpha1_mydata_fit<-cv.glmnet(x.train, y.train, type.measure="mse", 
                             alpha=1, family="gaussian")
alpha1_mydata_fit

#summary of cross validation
summary(alpha1_mydata_fit)
plot(alpha1_mydata_fit)

#minimum lambda value
alpha1_mydata_fit$lambda.min

#prediction on test model
alpha1_mydata_predicted<-predict(alpha1_mydata_fit,s=alpha1_mydata_fit$lambda.min, newx = x.test)
alpha1_mydata_predicted

mean((y.test-alpha1_mydata_predicted)^2)

-------------------------------------------------------------------------------------------------------------------------------------------
#Ridge regression on overseas money millions

y_overseas<-mydata$Overseas_money_millions
y_overseas

y_overseas.train<-y_overseas[c(1:500)]
y_overseas.train

y_overseas.test<-y_overseas[c(501:608)]
y_overseas.test

x_overseas<-as.matrix(mydata[,c("Budget","Gross_money_millions","IMDB_Rating",
                  "Overseas_percent","Profit_millions","Profit_Percent","US_millions","Gross_percent")])
x_overseas

x_overseas.train<-x_overseas[c(1:500),c("Budget","Gross_money_millions","IMDB_Rating",
                                        "Overseas_percent","Profit_millions","Profit_Percent","US_millions","Gross_percent")]
x_overseas.train

x_overseas.test<-x_overseas[c(501:608),c("Budget","Gross_money_millions","IMDB_Rating",
                                         "Overseas_percent","Profit_millions","Profit_Percent","US_millions","Gross_percent")]
x_overseas.test

lambdas<-10^seq(3,-2,by=-0.1)
lambdas

#Training model
alpha0_mydata_overseas<-glmnet(x_overseas.train,y_overseas.train,alpha = 0)
alpha0_mydata_overseas

#Summary of training model
summary(alpha0_mydata_overseas)
plot(alpha0_mydata_overseas)

#cross validation
alpha0_mydata_fit_overseas<-cv.glmnet(x_overseas.train, y_overseas.train, type.measure="mse", 
                             alpha=0, family="gaussian")
alpha0_mydata_fit_overseas

#summary of cross validation
summary(alpha0_mydata_fit_overseas)
plot(alpha0_mydata_fit_overseas)

#minimum lambda value
alpha0_mydata_fit_overseas$lambda.min

#prediction on test model
alpha0_mydata_predicted_overseas<-predict(alpha0_mydata_fit_overseas,s=alpha0_mydata_fit_overseas$lambda.min, newx = x_overseas.test)
alpha0_mydata_predicted_overseas

mean((y_overseas.test-alpha0_mydata_predicted_overseas)^2)

-----------------------------------------------------------------------------------------------------------------------------------
#lasso regression on overseas money millions
# we have used y_overseas, y_overseas.train, y_overseas.test,x_overseas, x_overseas.train,x_overseas.test
#from above ridge regression code of overseas

#Training model
alpha1_mydata_overseas<-glmnet(x_overseas.train,y_overseas.train,alpha = 1)
alpha1_mydata_overseas

#summary  of trainig model
summary(alpha1_mydata_overseas)

plot(alpha1_mydata_overseas)

#cross validation
alpha1_mydata_fit_overseas<-cv.glmnet(x_overseas.train, y_overseas.train, type.measure="mse", 
                             alpha=1, family="gaussian")

alpha1_mydata_fit_overseas

#summary of cross validation
summary(alpha1_mydata_fit_overseas)
plot(alpha1_mydata_fit_overseas)

#minimum lambda value
alpha1_mydata_fit_overseas$lambda.min

#prediction on test model
alpha1_mydata_predicted_overseas<-predict(alpha1_mydata_fit_overseas,s=alpha1_mydata_fit_overseas$lambda.min, newx = x_overseas.test)
alpha1_mydata_predicted_overseas

mean((y_overseas.test-alpha1_mydata_predicted_overseas)^2)

---------------------------------------------------------------------------------------------------------------------------------
#Ridge regression on USA millions
y_usa<-mydata$US_millions
y_usa

y_usa.train<-y_usa[c(1:500)]
y_usa.train

y_usa.test<-y_usa[c(501:608)]
y_usa.test

x_usa<-as.matrix(mydata[,c("Budget","Gross_money_millions","IMDB_Rating","Overseas_money_millions",
                                "Overseas_percent","Profit_millions","Profit_Percent","Gross_percent")])
x_usa

x_usa.train<-x_usa[c(1:500),c("Budget","Gross_money_millions","IMDB_Rating",
                                        "Overseas_percent","Profit_millions","Profit_Percent","Gross_percent")]
x_usa.train

x_usa.test<-x_usa[c(501:608),c("Budget","Gross_money_millions","IMDB_Rating",
                                         "Overseas_percent","Profit_millions","Profit_Percent","Gross_percent")]
x_usa.test

lambdas<-10^seq(3,-2,by=-0.1)
lambdas

#Training model
alpha0_mydata_usa<-glmnet(x_usa.train,y_usa.train,alpha = 0)
alpha0_mydata_usa

#summary of training model
summary(alpha0_mydata_usa)
plot(alpha0_mydata_usa)

#cross validation
alpha0_mydata_fit_usa<-cv.glmnet(x_usa.train, y_usa.train, type.measure="mse", 
                             alpha=0, family="gaussian")
alpha0_mydata_fit_usa

#summary of cross validation
summary(alpha0_mydata_fit_usa)
plot(alpha0_mydata_fit_usa)

#minimum lambda value
alpha0_mydata_fit_usa$lambda.min

#prediction on test model
alpha0_mydata_predicted_usa<-predict(alpha0_mydata_fit_usa,s=alpha0_mydata_fit_usa$lambda.min, newx = x_usa.test)
alpha0_mydata_predicted_usa

mean((y_usa.test-alpha0_mydata_predicted_usa)^2)

#lasso regression
#we have used y_usa, y_usa.train, y_usa.test, x_usa,  x_usa.train, x_usa.test from above ridge regression

#Training model
alpha1_mydata_usa<-glmnet(x_usa.train,y_usa.train,alpha = 1)
alpha1_mydata_usa

#summary of training model
summary(alpha1_mydata_usa)
plot(alpha1_mydata_usa)

#cross validation
alpha1_mydata_fit_usa<-cv.glmnet(x_usa.train, y_usa.train, type.measure="mse", 
                             alpha=1, family="gaussian")
alpha1_mydata_fit_usa

#summary of cross validation
summary(alpha1_mydata_fit_usa)
plot(alpha1_mydata_fit_usa)

#minimum value of lambda
alpha1_mydata_fit_usa$lambda.min

#prediction on test model
alpha1_mydata_predicted_usa<-predict(alpha1_mydata_fit_usa,s=alpha1_mydata_fit_usa$lambda.min, newx = x_usa.test)
alpha1_mydata_predicted_usa

mean((y_usa.test-alpha1_mydata_predicted)^2)

detach(mydata)
