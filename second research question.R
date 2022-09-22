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

Lungcancer_master <- read_excel("~/STDS/STDS_Lung_Cancer_Data_Analysis/survey lung cancer2..xlsx")

#To find if there is any missing data in the data set
is.na(Lungcancer_master)

#To identify the location or the number of NAs we can leverage the which() and sum() functions
# identify location of NAs in vector
which(is.na(Lungcancer_master))

# identify count of NAs in data frame
sum(is.na(Lungcancer_master))

#Result is 309 missing values

#For data frames, a convenient shortcut to compute the total missing values in each column is to use colSums()
colSums(is.na(Lungcancer_master))
