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
survey_lung_cancer2_ <- read_excel("~/STDS/STDS_Lung_Cancer_Data_Analysis/survey lung cancer2..xlsx")

View(survey_lung_cancer2_)

attach(survey_lung_cancer2_)
cancer = survey_lung_cancer2_
str(cancer)
summary(cancer)

df<-cancer[17:309, c(2,3,4,12,17)] 
View(df)
# Convert factor to numeric
str(cancer)

#visualising the data
#coord_flip: #Cartesian coordinates with x and y flipped
#Flip cartesian coordinates so that horizontal becomes vertical, and vertical, horizontal. This is primarily useful for converting geoms and statistics which display y conditional on x, to x conditional on y. #theme_grey() :the signature ggplot2 theme with a light grey background and white gridlines #scale_*_gradient creates a two colour gradient (low-high)
ggplot(data=df,aes(y=reorder(GENDER,SMOKING),x=LUNG_CANCER)) +  
  geom_bar(stat ='identity',aes(fill=SMOKING))+ 
  coord_flip() +  
  theme_grey() +  
  scale_fill_gradient(name="CANCER COMPARISION")+ 
  labs(title = 'LUNGS CANCER COMPARISION', 
       x='LUNG_CANCER',y='GENDER')+  
  geom_hline(yintercept = mean(df$LUNG_CANCER),size = 1, color = 'blue') 


ggplot(data=df,aes(y=reorder(GENDER,ALCOHOL_CONSUMING),x=LUNG_CANCER)) +  
  geom_bar(stat ='identity',aes(fill=ALCOHOL_CONSUMING))+ 
  coord_flip() +  
  theme_grey() +  
  scale_fill_gradient(name="CANCER COMPARISION")+ 
  labs(title = 'LUNGS CANCER COMPARISION', 
       x='LUNG_CANCER',y='GENDER')+  
  geom_hline(yintercept = mean(df$LUNG_CANCER),size = 1, color = 'blue') 

#EXTRACTING TWO COLUMNS FROM df AND RENAMING THEM AND ASSINING THEM TO NEW DATAFRAME df2

##selecting Age and AgeofSmokers variables
df2 = df[,c(2,3)] %>%
  pivot_longer(cols=c('AGE'),
               names_to='Agecriteria',
               values_to='AgeofSmokers')
df2

view(df2)

###VISUALISING THE RELATION BETWEEN GENDER, AGE,SMOKING AND ALCOHOL_CONSUMING##

#BOX PLOT:

ggplot(data = df2, aes(x=GENDER  , y=AgeofSmokers, color=Agecriteria)) + 
  geom_boxplot()+
  scale_color_brewer(palette="Dark2") + 
  geom_jitter(shape=16, position=position_jitter(0.2))+
  labs(title = 'Which gender smokes more?',
       y='AgeofSmokers',x='GENDER')

#BAR PLOT:
ggplot(data = df2, aes(x=GENDER  , y=AgeofSmokers)) + 
  geom_bar(stat='identity', width=0.5)+
  labs(title = 'Lungs Cancer Comparision of people Smokes',
       x='GENDER',y='AgeofSmokers')




##EXTRACTING TWO COLUMNS FROM df AND RENAMING THEM AND ASSINING THEM TO NEW DATAFRAME df3
#selecting Age and AgeofAlcoholConsumer variables
df3 = df[,c(2,3)] %>%
  pivot_longer(cols=c('AGE'),
               names_to='Agecriteria',
               values_to='AgeofAlcoholConsumer')
df3


view(df3)

#BOX PLOT:
#checking which gender drinks more by age criteria.

ggplot(data = df3, aes(x=GENDER  , y=AgeofAlcoholConsumer, color= Agecriteria)) + 
  geom_boxplot()+
  scale_fill_brewer(palette="red") + 
  geom_jitter(shape=16, position=position_jitter(0.2))+
  labs(title = 'Which gender Drinks more?',
       y='AgeofAlcoholConsumer',x='GENDER')

#BAR-PLOT:
ggplot(data = df3, aes(x=GENDER  , y=AgeofAlcoholConsumer, fill = 'GENDER')) + 
  geom_bar(stat='identity', width=0.5)+
  labs(title = 'Lungs Cancer Comparision of people Consuming Alcohol',
       x='GENDER',y='AgeofAlcoholConsumer')


# Correlation Plot: FINDING THE CORRELATION BETWEEN AGE, SMOKING, ALCOHOL_CONSUMING AND LUNG_CANCER
#selecting relevant columns from entire dataset.
df8=df[,c(3,4,5,6)]
df

#CORRELATION #To create correlatio, n plot, simply use cor(): # -1 here means we look at all columns except the first column
res=cor(df8)
res



#To visualise using “corrplot package” #The stronger the color and the bigger the size, the higher the correlation. All the variables are intercorrelated.
corrplot(res,type="upper", order="hclust",
         tl.col="purple",tl.srt=50)

#REGRESSION #Regression models describe the relationship between variables by fitting a line to the observed data. Linear regression models use a straight line, while logistic and nonlinear regression models use a curved line. Regression allows you to estimate how a dependent variable changes as the independent variable(s) change.
#Multiple Regression
#Multiple linear regression is used to estimate the relationship between two or more independent variables and one dependent variable.


cancer.data.lm1<-lm(LUNG_CANCER~SMOKING, data = cancer)
cancer.data.lm1 
summary(cancer.data.lm1)
plot(cancer.data.lm1)
deviance(cancer.data.lm1)


cancer.data.lm2<-lm(LUNG_CANCER~ALCOHOL_CONSUMING)
cancer.data.lm2 
summary(cancer.data.lm2)
plot(cancer.data.lm2)
deviance(cancer.data.lm2)



cancer.data.lm3<-lm(LUNG_CANCER~SMOKING+ALCOHOL_CONSUMING)
cancer.data.lm3
summary(cancer.data.lm3)
plot(cancer.data.lm3)
deviance(cancer.data.lm3)




