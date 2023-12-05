## Quantitative Agroecology Exercise for APBI360
# Written by Jens Ulrich, December 2023
# Press ctrl+enter to run each line of code

##------------------------------------------------------------------------------
# Part 1: Simulate slug density and soybean plant density and fit a linear regression model to the data

##------------------------------------------------------------------------------
# 1.1 Simulate an association between the independent and dependent variables

# set the random number generator so that we all get the same results
set.seed(11) 

# specify a sample size (how many plots are included in the field experiment)
n <- 12
# specify a minimum number of slugs we might expect to see in a trap (value taken from paper)
min_slugs_observed = 2
# specify a maximum number of slugs we might expect to see in a trap (value taken from paper)
max_slugs_observed = 9

# specify an intercept term (density of soybean plants when there are 0 slugs in traps)
intercept <- 25 # units are in 10,000 plants / hectare

# specify an effect size of slug density on soybean density
# an increase in 1 slug/trap should result in decrease in '_' number of soybean plants (10,000/ha)
# e.g., effect_of_slug_increase = -2 indicates that an increase in 1 slug/trap is associated with a decrease in 20,000 soybean plants/ha
effect_of_slug_increase <- -2 # negative values indicate a decrease in dependent variable conditional on increase in independent variable

# specify precision (how much does the response vary irrespective of the association)
sd <- 3

# simulate some slug trap data (independent variable), for n plots, ranging from lows to highs seen in the field experiment
# runif()
slugs_per_trap <- runif(n=n, min=min_slugs_observed, max=max_slugs_observed)

# simulate associated outcome of soybean plant density conditional (dependent variable) on slugs per trape
# rnorm()
linear_model <- (intercept + (effect_of_slug_increase * slugs_per_trap))
soybean_density <- rnorm(n=n, mean=linear_model, sd=sd)

# join the data into a 'data frame' structure
mydata <- data.frame(slugs_per_trap, soybean_density)

# visualize the data
# fix the axis labels here
# plot(mydata$slugs_per_trap, mydata$soybean_density)

##------------------------------------------------------------------------------
# 1.2 Quantify the association using a linear regression model

# fit a linear regression model to our data
# lm() creates a linear model 
summary(fit1 <- lm(soybean_density ~ slugs_per_trap))

##------------------------------------------------------------------------------
# 1.3 Visualize the association

# now plot the fit (with confidence intervals)
# first we need to create some new data
# we will make predictions for the mean and confidence across the same range of slugs that we "observed" in our simulation
min_slugs_observed <- min_slugs_observed
max_slugs_observed <- max_slugs_observed
# we will create a sequence of 'slugs_per_trap' for which we can predict an expected outcome
slug_interval <- 0.5 # predict the values for every 0.5 slugs added from minimum to maximum value

# now create some new independent data (slugs_per_trap)
newdata <- data.frame(slugs_per_trap = seq(min_slugs_observed, max_slugs_observed, slug_interval))
# View(newdata) # you can view the new data set

# now predict the mean and standard error for each value of slugs
# Confidence intervals - range in which the 'true' regression line lies given a certain level of confidence (default is 95% confidence)
# What is the mean soybean density of a plot given a particular slug density? 
pred <- predict(fit1, newdata, interval = 'confidence')

# create a plot using base R
# plot our simulated data
plot(x = mydata$slugs_per_trap, # independent variable
     y = mydata$soybean_density, # dependent variable
     cex = 1.75, pch = 21, bg = 'gray', # size, shape, and colour of the data points
     xlab = "Slugs (#/trap)",
     ylab = "Soybean plants (10,000/ha)", 
     frame = FALSE,
     xlim = c(min_slugs_observed, max_slugs_observed),
     ylim = c(0, 30)
)

# plot the predicted mean response for a given number of slugs per trap
lines(pred[,1] ~ newdata$slugs_per_trap, col = 'black', lwd = 2)
# plot the lower and upper 95% cI for the mean response for a given number of slugs per trap
lines(pred[,2] ~ newdata$slugs_per_trap, col = 'blue', lty = 2, lwd = 2)
lines(pred[,3] ~ newdata$slugs_per_trap, col = 'blue', lty = 2, lwd = 2)

# now let's add the 'true' regression line and see how close our model got
expected_means <- intercept + effect_of_slug_increase * newdata$slugs_per_trap
lines(expected_means ~ newdata$slugs_per_trap, col = 'red', lty = 2, lwd = 2)

# You could also remake the plot using the newer, popular ggplot tools
# the confidence bands are automatically generated for us and we don't have to do it ourselves
#library("ggplot2")
#(p <- ggplot(mydata, aes(slugs_per_trap, soybean_density)) +
#  geom_point() +
#  stat_smooth(method = lm))


##------------------------------------------------------------------------------
# Part 2: vary the slope, sample size, and random variability and see how the results change

##------------------------------------------------------------------------------
# 2.1 Point our environment towards a function that wraps all of the above code.
# This enables us to easily rerun the simulation with the option of tweaking the input settings 

# source() points the environment to a different file
# the "simulation_function.R" file holds the simulation function.
source("./day1_simulation/simulation_function.R")

# Now we can just run the following line of code to regenerate our original plot!
set.seed(11)
my_simulated_data <- simulate_slugs_and_soybeans(n=12, min_slugs_observed=2, max_slugs_observed=9, 
                                   intercept=25, effect_of_slug_increase=-2, sd=3,
                                   slug_interval=0.5)

##------------------------------------------------------------------------------
# 2.2 Decrease the slope (b increased from -2 to -0.5)

set.seed(11)
my_simulated_data <- simulate_slugs_and_soybeans(n=12, min_slugs_observed=2, max_slugs_observed=9, 
                                   intercept=25, effect_of_slug_increase=-0.5, sd=3,
                                   slug_interval=0.5)

# questions

##------------------------------------------------------------------------------
# 2.3 Increase the sample size (n increased from 12 to 100 plots)

set.seed(11)
my_simulated_data <- simulate_slugs_and_soybeans(n=100, min_slugs_observed=2, max_slugs_observed=9, 
                                   intercept=25, effect_of_slug_increase=-2, sd=3,
                                   slug_interval=0.5)

# questions: confidence interval changes.. does the R2 change? (it shouldn't)

#
##------------------------------------------------------------------------------
# 2.4 Decrease the sample size (n increased from 12 to 6 plots)

set.seed(11)
my_simulated_data <- simulate_slugs_and_soybeans(n=6, min_slugs_observed=2, max_slugs_observed=9, 
                                                 intercept=25, effect_of_slug_increase=-2, sd=3,
                                                 slug_interval=0.5)

# questions: confidence interval changes.. does the R2 change? (it shouldn't)

##------------------------------------------------------------------------------
# 2.5 Increase random variation in the outcome (sd increased from 3 to 4)

set.seed(11)
my_simulated_data <- simulate_slugs_and_soybeans(n=12, min_slugs_observed=2, max_slugs_observed=9, 
                                   intercept=25, effect_of_slug_increase=-2, sd=4,
                                   slug_interval=0.5)
