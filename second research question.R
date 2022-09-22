install.packages("tidyverse")  
install.packages("dplyr") 
install.packages("lme4") 
install.packages("ggplot2")  
install.packages("corrplot") 
install.packages("tidyr") 
install.packages("Hmisc") 
install.packages("ggGally")


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
Lungcancer_original <- read.csv("~/STDS/STDS_Lung_Cancer_Data_Analysis/Prediction lung cancer.csv")

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
Lungcancer_master$GENDER <- as.numeric(Lungcancer_master$GENDER)
Lungcancer_master$LUNG_CANCER <- as.numeric(Lungcancer_master$LUNG_CANCER)

#View if the GENDER column is now changed to numeric value
typeof(Lungcancer_master$GENDER)
typeof(Lungcancer_master$LUNG_CANCER)

# Success! Now both the columns have the datatype double, which is a numeric


# Come up with the Correlation matrix
Lungcancer_Correlation <- cor(Lungcancer_master)
View(Lungcancer_Correlation) #Correlation matrix is displayed in a tabular format

#Heatmap of the dataset to find what is in the data
rc <- rainbow(nrow(Lungcancer_Correlation), start = 0, end = .3)
cc <- rainbow(ncol(Lungcancer_Correlation), start = 0, end = .3)
heatmap(Lungcancer_Correlation,col = cm.colors(256), scale = "column",
        RowSideColors = rc, ColSideColors = cc, margins = c(5,10))

#no column dendrogram
heatmap(Lungcancer_Correlation,Colv = NA, col = cm.colors(256), scale = "column",
        RowSideColors = rc, margins = c(5,10))

#pairs to visualize data
pairs(Lungcancer_SmokingDrinking)

                                
#Filtering the smoking and Alcohol consumption columns along with age, sex, Lung cancer and Chronic disease coulumns
Lungcancer_SmokingDrinking <- subset(Lungcancer_original, select = c(1, 2, 3, 7, 11, 16))
View(Lungcancer_SmokingDrinking)

head(Lungcancer_SmokingDrinking)

#Visualize the spread of the relation ship between these values
## pending ####


#Regression model starts here
#(binomial) Logistic regression is chosen, since we are trying to find the probabilty of a person getting lung cancer with given age, sex and smoking  drinking habit

#Splitting our data into a train and a test set for validation:

# Adding an id number to each row for ease of tracking
Lungcancer_SmokingDrinking$id <- 1:nrow(Lungcancer_SmokingDrinking)   
Lungcancer.trainData <- Lungcancer_SmokingDrinking %>% dplyr::sample_frac(0.75)
Lungcancer.testData  <- dplyr::anti_join(Lungcancer_SmokingDrinking, Lungcancer.trainData, by = 'id')

#Finding a bit more out about the dataset.
glimpse(Lungcancer_SmokingDrinking)

#Now let’s try a simple glm() call. 
#What is the variable we are trying to predict? 
#The thing we are interested in is LUNG_CANCER variable. Normally we care much more about whether a person gets Lung Cancer or not than we do about other things, so we are going to try to predict that (i.e. classify our Patients) according to this outcome variable:

#  in this case a family = binomia(logit) term which tells the glm to run a (binomial) logistic regression using the logistic link function.
glm1 = glm(LUNG_CANCER ~ SMOKING + GENDER + AGE, family = binomial(logit), data = Lungcancer.trainData)
summary(glm1)

#Interpretation



