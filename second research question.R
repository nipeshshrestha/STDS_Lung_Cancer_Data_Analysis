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
library(plyr)


Lungcancer_master <- read.csv("~/STDS/STDS_Lung_Cancer_Data_Analysis/Prediction lung cancer.csv")

#To find if there is any missing data in the data set
is.na(Lungcancer_master)

#To identify the location or the number of NAs we can leverage the which() and sum() functions
# identify location of NAs in vector
which(is.na(Lungcancer_master))

# identify count of NAs in data frame
sum(is.na(Lungcancer_master))


#For data frames, a convenient shortcut to compute the total missing values in each column is to use colSums()
colSums(is.na(Lungcancer_master))

#Since there are no missing data, no action needed

#To view the column names
View(Lungcancer_master)
colnames(Lungcancer_master)

#LUNG_CANCER and GENDER are not numeric fields
#convert the text field values to numeric values for two columns LUNG_CANCER and GENDER
Lungcancer_master$GENDER <- revalue(Lungcancer_master$GENDER, c("M"=1))
Lungcancer_master$GENDER <- revalue(Lungcancer_master$GENDER, c("F"=0))

Lungcancer_master$LUNG_CANCER <- revalue(Lungcancer_master$LUNG_CANCER, c("YES"=1))
Lungcancer_master$LUNG_CANCER <- revalue(Lungcancer_master$LUNG_CANCER, c("NO"=0))

#View if the GENDER column is now changed to numeric value
typeof(Lungcancer_master$GENDER)
typeof(Lungcancer_master$LUNG_CANCER)

#The values are changed to Numeric but the field data type is still character. So we cannot do the correlation matrix

#The data type of GENDER and LUNG_CANCER columns has to be changed to numeric




#Heatmap of the dataset to find what is in the data
Lungcancer_Correlation <- cor(Lungcancer_master)
