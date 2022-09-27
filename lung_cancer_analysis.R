
library(tidyverse)
library(ggplot2)
library(GGally)


library (readr)

#Loading Lung cancer dataset
urlfile="https://raw.githubusercontent.com/nipeshshrestha/STDS_Lung_Cancer_Data_Analysis/main/lung_cancer_2017.csv"
lung_cancer_data<-read_csv(url(urlfile))
lung_cancer_data %>% head() %>% knitr::kable()

## Reading cancer data
str(lung_cancer_data)

## We then find summary of the dataset 
summary(lung_cancer_data)

#Let’s start by plotting two variables, Smoking and Lung Cancer. Are they correlated in the way that we would expect?

#Finding the correlation between Smoking and lung cancer death rate 
cor(lung_cancer_data$Smoking, lung_cancer_data$Lung_cancer_death_rate)

#Scatter plot between Smoking and lung cancer death rate with a linear relationship
ggplot(lung_cancer_data, aes(x=Smoking, y=Lung_cancer_death_rate)) +geom_point()+geom_smooth(method=lm)


#Finding the correlation between Alcohol_consumption and lung cancer death rate 
cor(lung_cancer_data$Alcohol_consumption, lung_cancer_data$Lung_cancer_death_rate)

#Scatter plot between Alcohol_consumption and lung cancer death rate with a linear relationship
ggplot(lung_cancer_data, aes(x=Alcohol_consumption, y=Lung_cancer_death_rate)) +geom_point()+geom_smooth(method=lm)

#Finding the correlation between Air_pollution and lung cancer death rate 
cor(lung_cancer_data$Air_pollution, lung_cancer_data$Lung_cancer_death_rate)

#Scatter plot between Air_pollution and lung cancer death rate with a linear relationship
ggplot(lung_cancer_data, aes(x=Air_pollution, y=Lung_cancer_death_rate)) +geom_point()+geom_smooth(method=lm)



#LINEAR REGRESSION
plot <- ggplot(data=lung_cancer_data, aes(x=Smoking,y=Lung_cancer_death_rate)) + 
  geom_point(size = 2) +
  
  # Let's add three guesses.
  geom_abline(slope = 0.5, intercept = 1.5, 
              colour = 'darkred', size = 1.5, alpha = 0.7, linetype = 'dashed') +
  geom_abline(slope = 0.75, intercept = 1.5, 
              colour = 'darkblue', size = 1.5, alpha = 0.7, linetype = 'dashed') +
  geom_abline(slope = 0.75, intercept = 1, 
              colour = 'orange', size = 1.5, alpha = 0.7, linetype = 'dashed') +
  theme_bw()

plot


# Let's copy our data
lung_cancer_data_with_model <- lung_cancer_data
slm <- lm(formula = Lung_cancer_death_rate~Smoking, 
          data = lung_cancer_data)

coef(slm)


# Next, lets add some columns to our tibble to represent the predicted values and the resulting residuals (we will need these to draw the plot)
lung_cancer_data_with_model$predicted <- predict(slm)   # Save the predicted values
lung_cancer_data_with_model$residuals <- residuals(slm) # Save the residual values


# the same old ggplot base function we started with
predicted_vs_actuals_plot <- ggplot(data = lung_cancer_data_with_model, 
                                    aes(x = Smoking, y = Lung_cancer_death_rate)) + 
  
# plot regression slope
geom_smooth(aes(color = "predicted"),
            method = "lm", se = FALSE, size = 1) +  

# add lines between our predicted and actual values
geom_segment(aes(xend = Smoking, yend = predicted), alpha = 0.2) +  

# add our actual values
geom_point(aes(colour = 'actual'), size = 1.5) + 

# add our predicted values
geom_point(aes(y = predicted, colour = 'predicted'), size = 1.5) + 

# add our sample means for both variables
geom_point(aes(x = mean(Smoking), y = mean(Lung_cancer_death_rate)), 
           color="darkred", size = 5, shape = 18, alpha = 0.7) +

scale_colour_manual(values = c('darkred', 'darkblue')) +

labs(colour = 'data') +
theme_bw()

predicted_vs_actuals_plot


sum(lung_cancer_data_with_model$residuals^2)

summary(slm)

ggpairs() + theme_bw()




#CURVE FITTING
# fit polynomial regression models up to degree 5
linear_model1 <- lm(Lung_cancer_death_rate~Smoking, data=lung_cancer_data)
linear_model2 <- lm(Lung_cancer_death_rate~poly(Smoking,2,raw=TRUE), data=lung_cancer_data)
linear_model3 <- lm(Lung_cancer_death_rate~poly(Smoking,3,raw=TRUE), data=lung_cancer_data)
linear_model4 <- lm(Lung_cancer_death_rate~poly(Smoking,4,raw=TRUE), data=lung_cancer_data)
linear_model5 <- lm(Lung_cancer_death_rate~poly(Smoking,5,raw=TRUE), data=lung_cancer_data)

# create a basic scatterplot 
plot(lung_cancer_data$Smoking, lung_cancer_data$Lung_cancer_death_rate)

# define x-axis values
x_axis <- seq(1, 60, length=60)

# add curve of each model to plot
lines(x_axis, predict(linear_model1, data.frame(Smoking=x_axis)), col='green')
lines(x_axis, predict(linear_model2, data.frame(Smoking=x_axis)), col='red')
lines(x_axis, predict(linear_model3, data.frame(Smoking=x_axis)), col='purple')
lines(x_axis, predict(linear_model4, data.frame(Smoking=x_axis)), col='blue')
lines(x_axis, predict(linear_model5, data.frame(Smoking=x_axis)), col='orange')


# calculated adjusted R-squared of each model
summary(linear_model1)$adj.r.squared
summary(linear_model2)$adj.r.squared
summary(linear_model3)$adj.r.squared
summary(linear_model4)$adj.r.squared
summary(linear_model5)$adj.r.squared


#TESTING ANOTHER MODEL

lm.cancer <- lm(Lung_cancer_death_rate ~ Smoking + Alcohol_consumption, data = lung_cancer_data)
summary(lm.cancer)


plot(lm.cancer)

# Draw histogram & density
smoking_density<-density(lung_cancer_data$Smoking)
plot(smoking_density, main="Smoking Density", col="blue")

alcohol_density<-density(lung_cancer_data$Alcohol_consumption)
plot(alcohol_density, main="Alcohol Consumption Density",col="red")


