#################
###HOMEWORK 10###
#################

#Authors: Aubrey Wendorff
#Packages: readxl, ggfortify, tidyverse, ggplot2, dplyr

# Objective 1 -------------------------------------------------------------

###PART A###----
#Read in dataset
library(readxl)
gauge <- read.csv("Data/Gauge_EC_Data.xlsx.csv")


###PART B###---
#Create a linear model: X = Specific conductivity, Y = Chloride concentration
gauge_model <- lm(IC..mg.L. ~ SPC, data = gauge)


###PART C###---
#Run Autoplots for assumption checking
library(ggfortify)
library(ggplot2)
c_plot <- autoplot(gauge_model) #gives Q-Q, residuals, leverage
print(c_plot)

#Histogram of residuals
hist(gauge_model$residuals)

#Residuals vs Fitted plot, looks for linearity
  #My residuals plot is check-marked shape, rather than the random scatter around 0 we are looking for.
  #Based on this, I would say that my data violates the linearity assumption quite a bit, as there is no scattering data around zero

#Q-Q PLot, looks for normality
  #Points follow the line closely, except for a few higher leverage or outlier points.
  #Because of this, I would say that my data does not violate the normality assumption at all

#Histogram of residuals, looks for normality
  #To double-check the assumption above, I looked at a histogram as well
  #My data showed a norma-distribution (bell-curve), showing again that my data doesn't violate the normality assumption at all

#Scale-location, homoscedasiticty
  #My data shows a stepwise upward trend, rather the roughly horizontal line we are looking for
  #Based on this plot, as the residuals vs fit plot (which also shows heteroscedasticity), this assumption is heavily violated

###PART D###---
#Calculate median and 95th percentile
median_spc <- median(gauge$SPC, na.rm = TRUE) #calcuate median, remove NA's
spc_95th <- quantile(gauge$SPC, 0.95, na.rm = TRUE) #calculate 95th percentile
         
#Create a new df with the median and 95th percentile
gauge_new <- data.frame (SPC = c(median_spc, spc_9th)) #needed for lm model

#Generate predicted y values
predictions <- predict(gauge_model, #predict() uses lm model to generate predicted values for y based on x
                       newdata = gauge_new, #which df to predict with
                       interval = "prediction", #gives you 95% prediction interval
                       level = 0.95) #confidence level for prediction interval
    #Based on these results, the prediction interval for the 95th percentile is much wider than at the median
        #median: –12.0–140.6 mg/L
        #95th: 93.9–251.2 mg/L

# Objective 2 -------------------------------------------------------------

###PART A###---
#Set parameters for uniform dataset
set.seed(123) #reproducibility of random generation
n <- 100 #sample size of 100 (100 x and y pairs)\
x <- runif(n, 0, 10) #random uniform predictor from 0-10
intercept_real <- 3
slope_real <- 0.5

#Add lognormal error to data, making it right-skewed
error <- rlnorm(n, #rlnorm() generates lognormal dist.
                meanlog = 0, #mean
                sdlog = 0.35) #stdev

error_sim <- error -  exp(0.35^2 / 2) #subtract the theoretical mean of the dist so that the error has an average near 0

#Check sim data distribution
hist(error_sim)

###PART B###---
#Calculate new y 
y_error <- intercept_real + slope_real * x + error_sim

#Create regression for lognormal model
lognormal_model <- lm (y_error ~ x)

###PART C/D/E###---
#Set parameters to run simulation 100 times
n_sims <- 100

#Create an empty data.frame for results, with numeric est intercept and slope column
results <- data.frame(intercept_est = numeric(n_sims),
                      slope_est = numeric(n_sims))

#For loop to run simulations
for (i in 1:n_sims) {
  x_i <- runif(n, 0, 10) #generate random x #'s
  error_i <- rlnorm(n, meanlog = 0, sdlog = 0.35) - exp(0.35^2 / 2) #generate lognormal error
  y_i <- intercept_real + slope_real * x_i + error_i #generate y values 
  model_i <- lm(y_i ~ x_i) #fit the linear model for the xi and yi
  
  #Store slope and intercept estimates
    results$intercept_est[i] <- coef(model_i)[1] #extract the fitted coefficients for 1 (Intercept) and 2 (slope)
    results$slope_est[i] <- coef(model_i)[2]  #store values in results df for each i   
    
  #Generate 95% prediction intervals
    pred_i <- predict(model_i, newdata = data.frame(x_i = x_i),
                      interval = "prediction", level = 0.95)
  #Calculate coverage  
    coverage <- mean(between(y_i, pred_i[, "lwr"], pred_i[, "upr"])) #returns true if y is >= lower and <= upper
    results$coverage[i] <- coverage #store coverage
}

###PART D###---
#Check results of estimation and compare to true values
mean(results$intercept_est)
mean(results$slope_est)
intercept_real
slope_real

  #The estimates and true slopes and intercepts match very well with only negligable differences
      #est slope = 0.49, true slope = 0.5
      #est int = 3.004, true int = 3

###PART F###---
  #coverage = 0.94 of data

###PART G###---
  #My estimation are very close to 95% being at the 95% prediction interval
  #But, it is slightly below. This means that the intervals are slightly underestimating the true varaibility in the data
