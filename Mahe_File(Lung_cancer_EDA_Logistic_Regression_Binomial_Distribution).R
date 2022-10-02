
install.packages("tidyverse")
install.packages("dplyr")
install.packages("ggplot2")
install.packages("lme4") 
install.packages("corrplot") 
install.packages("tidyr") 
install.packages("Hmisc") 
install.packages("Amelia")


library(dplyr)
library(ggplot2)
library(corrplot)
library(tidyr)
library(Hmisc)
library(lme4)
library(tidyverse)

library(readxl)

Survey_Lung_Cancer2 <- read_excel("D:/Statistical Thinking and DataScience/AT2/Survey_Lung_Cancer2.xlsx")
View(Survey_Lung_Cancer2)

attach(Survey_Lung_Cancer2)
cancer<-Survey_Lung_Cancer2
str(cancer)

summary(cancer)

df<-cancer[1:308, c(1,2,3,4,12,17)] 
View(df)


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
df8
#CORRELATION #To create correlatio, n plot, simply use cor(): # -1 here means we look at all columns except the first column

res=cor(df8[,-1])
res

res=cor(df8)
res
#CORRELATION #To create correlatio, n plot, simply use cor(): # -1 here means we look at all columns except the first column
res=cor(df[,-1])
res

## METHOD 2: VISUALISING AND FINDING CORRELATION "RESPOND VARIABLE AND ONE EXPLANATORY VARIABLE'###

#VISUALISING THE RELATION BETWEEN "ALCOHOL_CONSUMING AND LUNG_CANCER"
p <- ggplot(data=cancer, aes(x=ALCOHOL_CONSUMING, y=LUNG_CANCER, fill=ALCOHOL_CONSUMING)) +
  geom_bar(stat="identity")

p

#FINDING THE CORRELATION BETWEEN ALCOHOL_CONSUMING AND LUNG_CANCER
cor(cancer$ALCOHOL_CONSUMING, cancer$LUNG_CANCER)


##VISUALISING THE RELATION BETWEEN "SMOKING AND LUNG_cANCER"
p <- ggplot(data=cancer, aes(x=SMOKING, y=LUNG_CANCER, fill=SMOKING)) +
  geom_bar(stat="identity")

p

#FINDING CORRELATION BETWEEN "SMOKING AND LUNG_cANCER"

cor(cancer$SMOKING, cancer$LUNG_CANCER)


##VISUALISING THE RELATION BETWEEN "AGE AND LUNG_CANCER"
p <- ggplot(data=cancer, aes(x=AGE, y=LUNG_CANCER, fill=AGE)) +
  geom_bar(stat="identity")

p

#FINDING THE CORRELATION BETWEEN "AGE AND LUNG_CANCER"
cor(cancer$AGE, cancer$LUNG_CANCER)

##VISUALISING THE RELATION BETWEEN "AGE AND LUNG_CANCER"
p <- ggplot(data=cancer, aes(x=GENDER, y=LUNG_CANCER, fill=AGE)) +
  geom_bar(stat="identity")

p

#To visualise using “corrplot package” #The stronger the color and the bigger the size, the higher the correlation. All the variables are intercorrelated.
corrplot(res,type="upper", order="hclust",
         tl.col="purple",tl.srt=50)



####LOGISTIC LINEAR REGRESSION  ####


summary(cancer)
nrow(cancer)



#make this example reproducible
set.seed(1)

#Use 70% of dataset as training set and remaining 30% as testing set
cancer$S_NO <- 1:nrow(cancer)
cancer.trainData <- cancer %>% dplyr::sample_frac(0.70,0.30)
cancer.testData  <- dplyr::anti_join(cancer, cancer.trainData, by = 'S_NO')

ggplot(cancer) + 
  geom_bar(aes(x = SMOKING)) +
  theme_bw()

ggplot(cancer) + 
  geom_bar(aes(x = ALCOHOL_CONSUMING)) +
  theme_bw()


glimpse(cancer)



library(Amelia)
missmap(cancer.trainData, main = "Missing values vs observed")

#fit logistic regression model
glm1 <- glm(LUNG_CANCER~SMOKING, family=binomial(logit), data=cancer.trainData)
summary(glm1) 
glm2 <- glm(LUNG_CANCER~ALCOHOL_CONSUMING, family=binomial(logit), data=cancer.trainData)
summary(glm2) 

glm3 <- glm(LUNG_CANCER~GENDER+AGE+SMOKING, family=binomial(logit), data=cancer.trainData)
summary(glm3)

glm4 <- glm(LUNG_CANCER~GENDER+AGE+ALCOHOL_CONSUMING, family=binomial(logit), data=cancer.trainData)
summary(glm4)

glm5 <- glm(LUNG_CANCER~GENDER+AGE+SMOKING+ALCOHOL_CONSUMING, family=binomial(logit), data=cancer.trainData)
summary(glm5)


#Finding probability of Male getting Luncg cancer due to Smoking and drinking with respect to age as per their level of consumption.

(probM3 <- predict(glm5, newdata = data.frame(GENDER = "M", AGE= 54, SMOKING = 2, ALCOHOL_CONSUMING= 2), type = "response"))
(probM3 <- predict(glm5, newdata = data.frame(GENDER = "M", AGE= 53, SMOKING = 2, ALCOHOL_CONSUMING= 1), type = "response"))
(probM3 <- predict(glm5, newdata = data.frame(GENDER = "M", AGE= 63, SMOKING = 1, ALCOHOL_CONSUMING= 1), type = "response"))
(probM3 <- predict(glm5, newdata = data.frame(GENDER = "M", AGE= 63, SMOKING = 2, ALCOHOL_CONSUMING= 2), type = "response"))
(probM3 <- predict(glm5, newdata = data.frame(GENDER = "M", AGE= 74, SMOKING = 2, ALCOHOL_CONSUMING= 1), type = "response"))



#Finding probability of Female getting Luncg cancer due to Smoking and drinking with respect to age as per their level of consumption.
(probM3 <- predict(glm5, newdata = data.frame(GENDER = "F", AGE= 48, SMOKING = 1, ALCOHOL_CONSUMING= 2), type = "response"))
(probM3 <- predict(glm5, newdata = data.frame(GENDER = "F", AGE= 61, SMOKING = 2, ALCOHOL_CONSUMING= 2), type = "response"))



##Model validation

##if we try to test our new model using that hold out test data? We can make use of the predict() function for this purpose.
pred <- predict(glm1, newdata = cancer.testData)
head(pred)   ## many of them lie outside the 0-1 range.

# 1st way to find  out the probabiliy which gender got lung cancer
prob <- 1/(1 + exp(-pred))
head(prob)
#another way to find probability of which gender got lung cancer
probability <- predict(glm1, newdata = cancer.testData, type="response")

head(probability)



#More validation


prediction <- ifelse(probability > 0.5, 1, 0) 
prediction


# building a contingency table of the counts at each combination of factor levels
confusion  <- table(cancer.testData$LUNG_CANCER, prediction) 
confusion 

#More complex logistic models

glm6 = glm(LUNG_CANCER ~ SMOKING*GENDER+AGE, family = binomial(logit), data = cancer.trainData)
summary(glm6)

glm7 = glm(LUNG_CANCER ~ ALCOHOL_CONSUMING*GENDER+AGE, family = binomial(logit), data = cancer.trainData)
summary(glm7)


###BINOMIAL DISTRIBUTIONS##

cancer = rbinom(309, size = 13, prob = 1 / 2)
 hist(rbinom(309, size = 13, prob = 1 / 2))
 
 



