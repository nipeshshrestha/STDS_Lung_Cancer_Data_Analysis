install.packages("tidyverse")  

install.packages("dplyr") 

install.packages("lme4") 

install.packages("ggplot2")  

install.packages("corrplot") 

install.packages("tidyr") 

install.packages("Hmisc") 

library(dplyr)
library(ggplot2)
library(corrplot)
library(tidyr)
library(Hmisc)
library(lme4)
library(tidyverse)


library(readxl)
#dataset1 <- read_excel("/Users/Documents/STDS Code/survey lung cancer final.xlsx")
dataset1 <- read_excel("survey lung cancer2.xlsx")
# Display loaded file 
View(dataset1)

#Accessing variables of a data frame
attach(dataset1)
cancerdata=dataset1

#summaries of the results of various model fitting result 
summary(cancerdata)

# Finding Correlation between (Smoking/Alcohol/Age)and Lung cancer
cor(cancerdata$SMOKING,cancerdata$LUNG_CANCER)
cor(cancerdata$ALCOHOL_CONSUMING,cancerdata$LUNG_CANCER)
cor(cancerdata$AGE,cancerdata$LUNG_CANCER)

# Plotting Graph: Identify which Gender have max Smoking & Alochol consumption

# Which GENDER have high SMOKING habits? F< M
ggplot(cancerdata,aes(x=GENDER,y=SMOKING,fill=GENDER))+
  geom_bar(stat='identity',width=0.2)

# Which GENDER have high ALCOHOL CONSUMPTION habits? F< M
ggplot(cancerdata,aes(x=GENDER,y=ALCOHOL_CONSUMING,fill=GENDER))+
  geom_bar(stat='identity',width=0.2)

#Line graph 

ggplot(cancerdata,aes(x=AGE))+geom_line(aes(y=SMOKING),color="red") +xlim(20,80)+ylim(0,3)+
  geom_line(aes(y=ALCOHOL_CONSUMING),color="Blue")

#Filtering columns
cancerdf=cancerdata[,c(2,3,4,12,17,18)]
View(cancerdf)

# Multiple Linear regression lm

cancerinfo.lm<-lm(cancerdf$LUNG_CANCER ~ cancerdf$SMOKING+cancerdf$ALCOHOL_CONSUMING)
cancerinfo.lm

# Filtering data by SMOKING = YES & ALCOHOL CONSUMING = YES 
data_frame_mod <- filter(cancerdf,cancerdf$SMOKING==2,cancerdf$ALCOHOL_CONSUMING==2)
View(data_frame_mod)

#Filtering data by without SMOKING & ALCOHOL CONSUMPTION but have LUNG CANCER
dataset2<- filter(cancerdf,cancerdf$SMOKING==1,cancerdf$ALCOHOL_CONSUMING==1,cancerdf$LUNG_CANCER==1)
View(dataset2)

# Separating data with and without lung cancer
withLC<- filter(data_frame_mod,data_frame_mod$LUNG_CANCER==1)

#With LUNG CANCER Patient
View(withLC)

#Without LUNG CANCER Patient
withoutLC <- filter(data_frame_mod,data_frame_mod$LUNG_CANCER==0)
View(withoutLC)

#Display which GENDER group have Lung cancer
ggplot(withLC,aes(x=GENDER,y=LUNG_CANCER,fill=GENDER))+
  geom_bar(stat='identity',width=0.2)

#Binomial Distribution

#Finding Probability
dbinom(x=0:1,size=309,prob=1/2)
pbinom(93,size=309,prob=1/2) 

#plot Graph
plot(data_frame_mod$LUNG_CANCER,pbinom(data_frame_mod$LUNG_CANCER,size=309,prob=0.5),type='o',
     col="red",
     main='Possibilties of LUNG CANCER',
     ylab= 'Prospects',
     xlab =' IN USE of SMOKING & ALCOHOL ',
     lwd=2)

