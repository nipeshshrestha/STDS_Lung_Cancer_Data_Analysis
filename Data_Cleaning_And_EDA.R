#START
install.packages("tidyverse")         # Install tidyverse package
install.packages("corrplot")
library(tidyverse)                    # Load tidyverse package
library(corrplot)

# Reading datasets
country_code_mapping <- read.csv("Metadata_Country.csv")
data1 <- read.csv("smoking.csv", header = FALSE) # Smoking datasets
data2 <- read.csv("alcohol.csv", header = FALSE) # Alcohol Consumption datasets
data3 <- read.csv("pollution.csv", header = FALSE) # Pollution dataseets
data4 <- read.csv("population.csv", header = FALSE) # Population datasets
data5 <- read.csv("total-cancer-deaths-by-type.csv") # Cancer deaths datasets


# Removing the top 4 rows and making row 4 as header for Smoking
colnames(data1) <- data1[3, ] 
data1 <- data1 %>% slice(4:n())

# Removing the top 4 rows and making row 4 as header for Alcohol Consumption
colnames(data2) <- data2[3, ] 
data2 <- data2 %>% slice(4:n())

# Removing the top 4 rows and making row 4 as header for Pollution
colnames(data3) <- data3[3, ] 
data3 <- data3 %>% slice(4:n())

# Removing the top 4 rows and making row 4 as header for Population
colnames(data4) <- data4[3, ] 
data4 <- data4 %>% slice(4:n())

# Replacing Missing value of Smoking
smoking <- data1                        # Duplicate data frame
smoking$`2017`[is.na(smoking$`2017`)] <- rowMeans(smoking[,c(45:61)], na.rm = TRUE)[is.na(smoking$`2017`)]  # Replace by row means
smoking  

# Replacing Missing value of alcohol
alcohol <- data2                   # Duplicate data frame
alcohol$`2017`[is.na(alcohol$`2017`)] <- rowMeans(alcohol[,c(45:61)], na.rm = TRUE)[is.na(alcohol$`2017`)]  # Replace by row means
alcohol                                  # Print new data frame

# Replacing Missing value of pollution
pollution <- data3                       # Duplicate data frame
pollution$`2017`[is.na(pollution$`2017`)] <- rowMeans(pollution[,c(45:61)], na.rm = TRUE)[is.na(pollution$`2017`)]  # Replace by row means
pollution     

# Replacing Missing value of population
population <- data4                         # Duplicate data frame
population$`2017`[is.na(population$`2017`)] <- rowMeans(population[,c(45:61)], na.rm = TRUE)[is.na(population$`2017`)]  # Replace by row means
population  

# Replacing Missing value of cancer_deaths
data5 <- data5 %>% filter(data5$Year == 2017)
cancer_deaths <- data5[,c(1,2,3,7)] 

# Renaming column
names(country_code_mapping)[names(country_code_mapping) == 'Country.Code'] <- 'Country_Code'
names(country_code_mapping)[names(country_code_mapping) == 'TableName'] <- 'Country_Name'
names(smoking)[names(smoking) == '2017'] <- 'Smoking'
names(alcohol)[names(alcohol) == '2017'] <- 'Alcohol_consumption'
names(pollution)[names(pollution) == '2017'] <- 'Air_pollution'
names(population)[names(population) == '2017'] <- 'Population'
names(cancer_deaths)[names(cancer_deaths) == 'Lung_cancer'] <- 'Lung_cancer_death'


# Merging all datasets
m1 = select(merge(x = country_code_mapping, y = pollution, by.x = "Country_Code", by.y = "Country Code", all.x = TRUE),c(1,2,3,5,'Air_pollution'))
m2 = select(merge(x=m1, y=alcohol, by.x = "Country_Code", by.y = "Country Code", all.x = TRUE), c(1:5,'Alcohol_consumption'))
m3 = select(merge(x=m2, y=smoking, by.x = "Country_Code", by.y = "Country Code", all.x = TRUE), c(1:6,'Smoking'))
m4 = select(merge(x=m3, y=cancer_deaths, by.x = "Country_Code", by.y = "Code", all.x = TRUE), c(1:7,'Lung_cancer_death'))
final_data = select(merge(x=m4, y=population, by.x = "Country_Code", by.y = "Country Code", all.x = TRUE),c(1:8,"Population"))


# EDA

# Finding top 6 rows
head(final_data)

# Summarizing the descriptive statistics
summary(final_data)

# Checking number of rows and columns
dim(final_data)

# Removing NA values
final_data <- na.omit(final_data)

# Checking final data after removing NA values
str(final_data)

# Plot b/w Lung_cancer_death vs Smoking
ggplot(final_data) +
  aes(x = Lung_cancer_death, y = Smoking) +
  geom_point(shape = "circle", size = 1.5, colour = "Red") +
  theme_minimal()

# Plot b/w Lung_cancer_death vs Alcohol_consumption
ggplot(final_data) +
  aes(x = Lung_cancer_death, y = Alcohol_consumption) +
  geom_point(shape = "circle", size = 1.5, colour = "Green") +
  theme_minimal()

# Plot b/w Lung_cancer_death vs Air_pollution
ggplot(final_data) +
  aes(x = Lung_cancer_death, y = Air_pollution) +
  geom_point(shape = "circle", size = 1.5, colour = "Blue") +
  theme_minimal()

# Removing the outliners
final_data <- final_data %>% filter(final_data$Lung_cancer_death <60000)

# Calculating the Lung_cancer_death_rate in standardize form
final_data$Lung_cancer_death_rate <- final_data$Lung_cancer_death*100000/final_data$Population
head(final_data)

# Plot b/w Lung_cancer_death_rate vs Smoking
ggplot(final_data) +
  aes(x = Lung_cancer_death_rate, y = Smoking) +
  geom_point(shape = "circle", size = 1.5, colour = "Red") +
  theme_minimal()

# Plot b/w Lung_cancer_death_rate vs Alcohol_consumption
ggplot(final_data) +
  aes(x = Lung_cancer_death_rate, y = Alcohol_consumption) +
  geom_point(shape = "circle", size = 1.5, colour = "Green") +
  theme_minimal()

# Plot b/w Lung_cancer_death_rate vs Air_pollution
ggplot(final_data) +
  aes(x = Lung_cancer_death_rate, y = Air_pollution) +
  geom_point(shape = "circle", size = 1.5, colour = "Blue") +
  theme_minimal()



# Finding correlation among all variables
cor(final_data[c(5,6,7,10)])

# Plotting correlation matrix graph
corrplot(cor(final_data[c(5,6,7,10)]), method = 'color')   #Method 1: By showing shading
corrplot(cor(final_data[c(5,6,7,10)]), method = 'number')  #Method 2: By showing correlation number

#END
