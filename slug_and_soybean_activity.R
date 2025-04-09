new_object <- 2 + 2
new_object

rm(new_object)

# initialize random number generator
set.seed(19)

# specify a sample size (how many plots are included in the field experiment)
n <- 12

# specify a minimum number of slugs we might expect to see in a trap
min_slugs_observed = 2
# specify a maximum number of slugs we might expect to see in a trap
max_slugs_observed = 9
# simulate some slug trap data (independent variable), for n plots,
# ranging from minimum to maximum values seen in the field experiment
slugs_per_trap <- runif(n=n, min=min_slugs_observed, max=max_slugs_observed)

slugs_per_trap
hist(slugs_per_trap,
     main = "",
     xlab = "slugs per trap",
     ylab = "frequency",
     xlim = c(0, 12))

# specify an intercept term, i.e.,
# density of soybean plants when there are zero slugs in traps
intercept <- 25 # units are in 10,000 plants / hectare
# specify a slope term, i.e.,
# a '_' change in soybean plant density associated with
# every increase of 1 slug per trap in the plot
slope <- -2

#specify precision
# (how much does the response vary irrespective of the association)
sd <- 3 # standard deviation # units are in 10,000 plants / hectare

# use rnorm() to simulate soybean plant densities
# for "n" plots with 2 to 9 "slugs_per_trap slug"
# an effect size of "slope"
# an intercept of "intercept"
# and a standard deviation of "sd"
linear_model <- (intercept + (slope * slugs_per_trap)) 
soybean_density <- rnorm(n=n, mean=linear_model, sd=sd)
# join the independent and dependent data into a single 'data frame' structure
mydata <- data.frame(slugs_per_trap, soybean_density)

# create a plot using base R plotting tools
par(mar = c(5, 5, 2, 5)) # Set the margin on the sides to 5
plot(x = mydata$slugs_per_trap, # independent variable
     y = mydata$soybean_density, # dependent variable
     # size, shape, and colour of the data points
     cex = 1.75, pch = 21, bg = 'gray',
     xlab = "Slugs (#/trap)", # x-axis title
     ylab = "Soybean plants (10,000/ha)", # y-axis title
     frame = FALSE, # remove frame
     xlim = c(min_slugs_observed, max_slugs_observed), # x-axis limits
     ylim = c(0, 30) # y-axis limits
)

# fit a linear regression model to our data
# lm() fits a linear model
summary(fit1 <- lm(formula = soybean_density ~ slugs_per_trap,
                   data = mydata))
# save important outputs
# intercept term
(estimate_intercept <- summary(fit1)$coefficients[1,1])
# effect of slug increase
(estimate_slope <- summary(fit1)$coefficients[2,1])

# now plot the fit (with confidence intervals)
# first we need to create some new data
# we will make predictions for the mean and confidence across the same range
# of slugs that we "observed" in our simulation
min_slugs_observed <- min_slugs_observed
max_slugs_observed <- max_slugs_observed
# now create some new independent data (slugs_per_trap)
# ranging from min to max and stepping up by equal intervals
newdata <- data.frame(slugs_per_trap = seq(
  min_slugs_observed, max_slugs_observed, length.out=nrow(mydata)))
# View(newdata) # you can view the new data set
# now predict the expected outcome for each value of slugs
# What is the expected soybean density of a plot
# given a particular slug density?
pred <- predict(object=fit1, newdata, interval = 'confidence')
# create a plot using base R
# plot our simulated data
{
  par(mar = c(5, 5, 2, 5)) # Set the margin on the sides to 5
  plot(x = mydata$slugs_per_trap, # independent variable
       y = mydata$soybean_density, # dependent variable
       # size, shape, and colour of the data points
       cex = 1.75, pch = 21, bg = 'gray',
       xlab = "Slugs (#/trap)", # x-axis title
       ylab = "Soybean plants (10,000/ha)", # y-axis title
       frame = FALSE, # remove frame
       xlim = c(min_slugs_observed, max_slugs_observed), # x-axis limits
       ylim = c(0, 30) # y-axis limits
  )
  # plot the predicted mean response for a given number of slugs per trap
  lines(pred[,1] ~ newdata$slugs_per_trap, col = 'black', lwd = 2)  
  # Confidence intervals - range in which the 'true' regression line lies
  # given a certain level of confidence (default is 95% confidence).
  # Plot the 95% CI for the mean response across range of slugs per trap
  lines(pred[,2] ~ newdata$slugs_per_trap, col = 'blue', lty = 2, lwd = 2)
  lines(pred[,3] ~ newdata$slugs_per_trap, col = 'blue', lty = 2, lwd = 2)
  # Now let's add the 'true' regression line and see how close our model got
  expected_means <- intercept + slope * newdata$slugs_per_trap
  lines(expected_means ~ newdata$slugs_per_trap, col = 'red', lty = 2, lwd = 2)
  legend("topright",
         legend = c("expected mean", "model-based estimate for mean",
                    "95% confidence interval"),
         pch = "|", col = c("red", "black", "blue"))
}  


#-------------------------------------------------------------------------------
# DAY 2

getwd() # get current working directory
# setwd("C:/Users/my_name/Documents/R/APBI360") # set working directory
# ˆ your working directory may look something like what I've pasted above.
# ˆ replace the above with the path that you are using on your own computer.
# ˆ remove the hastag at the front of the above line.
# ˆ make sure that you use forward slashes.
list.files() # print out the files in your working directory.

# the "simulation_function.R" file holds the simulation function.
source(file="./simulation_function.R")

# Now we can just run the following line of code to resimulate
# our original data and regenerate our original plot!
set.seed(19)

my_simulated_data <- simulate_slugs_and_soybeans(n=12,
                                                 min_slugs_observed=2, max_slugs_observed=9,
                                                 intercept=25, slope=-2, sd=3,
                                                 n_reps=1)
print(paste0("intercept = ", signif(
  my_simulated_data$estimate_intercept, digits=3)))
print(paste0("slope = ", signif(my_simulated_data$estimate_slope, digits=3)))

# simulate data with a new intercept and/or slope
my_simulated_data <- simulate_slugs_and_soybeans(n=12,
                                                 min_slugs_observed=2, max_slugs_observed=9,
                                                 intercept=10, slope=1, sd=3,
                                                 n_reps=1)

# simulate data with a new sd
my_simulated_data <- simulate_slugs_and_soybeans(n=12,
                                                 min_slugs_observed=2, max_slugs_observed=9,
                                                 intercept=25, slope=-2, sd=6,
                                                 n_reps = 1)

print(paste0("intercept = ", signif(
  my_simulated_data$estimate_intercept, digits=3)))
print(paste0("slope = ", signif(my_simulated_data$estimate_slope, digits=3)))


#compare parameter estimates for relatively low sd
set.seed(19)
my_simulated_data <- simulate_slugs_and_soybeans(n=12,
                                                 min_slugs_observed=2, max_slugs_observed=9,
                                                 intercept=25, slope=-2, sd=3,
                                                 n_reps = 100)
# versus for relatively high sd
my_simulated_data <- simulate_slugs_and_soybeans(n=12,
                                                 min_slugs_observed=2, max_slugs_observed=9,
                                                 intercept=25, slope=-2, sd=6,
                                                 n_reps = 100)

# compare parameter estimates for relatively low sample size
my_simulated_data <- simulate_slugs_and_soybeans(n=12,
                                                 min_slugs_observed=2, max_slugs_observed=9,
                                                 intercept=25, slope=-2, sd=3,
                                                 n_reps = 100)
# versus relatively high sample size
my_simulated_data <- simulate_slugs_and_soybeans(n=100,
                                                 min_slugs_observed=2, max_slugs_observed=9,
                                                 intercept=25, slope=-2, sd=3,
                                                 n_reps = 100)

# read in the spreadsheet (after saving it in your working directory folder)
my_real_data <- read.csv("./new_slug_and_soybean_data.csv")
# view the data
(my_real_data)
# how many plots did we collect data from
print(paste0("data were collected from ", nrow(my_real_data), " field plots."))
# mean slugs per trap measurement
mean(my_real_data$slugs_per_trap)
# mean soybean density
mean(my_real_data$soybean_density)

# fit a linear regression model to our data
# lm() fits a linear model
summary(fit2 <- lm(formula = soybean_density ~ slugs_per_trap,
                   data = my_real_data))
# save important outputs
# intercept term
(estimate_intercept <- summary(fit2)$coefficients[1,1])
# effect of slug increase
(estimate_slope <- summary(fit2)$coefficients[2,1])
# now plot the fit (with confidence intervals)
# first we need to create some new data
# we will make predictions for the mean and confidence across the same range
# of slugs that we "observed" in our simulation
min_slugs_observed <- floor(min(my_real_data$slugs_per_trap)) # round down
max_slugs_observed <- ceiling(max(my_real_data$slugs_per_trap)) # round up
# now create some new independent data (slugs_per_trap)
# ranging from min to max and stepping up by interval
newdata2 <- data.frame(slugs_per_trap = seq(
  min_slugs_observed, max_slugs_observed, length.out=nrow(my_real_data)))

# View(newdata) # you can view the new data set
# now predict the expected outcome for each value of slugs
# What is the expected soybean density of a plot given a particular slug density?
pred2 <- predict(object=fit2, newdata2, interval = 'confidence')
# create a plot using base R
# plot our simulated data
{
  par()
  par(mfrow=c(1,1), # return to 1 row and 1 column in the plot viewer
      mar = c(5, 5, 2, 5)) # Set the margin on the sides to 5
  plot(x = my_real_data$slugs_per_trap, # independent variable
       y = my_real_data$soybean_density, # dependent variable
       cex = 1.75, pch = 21, bg = 'gray', # size, shape, and colour of the data points
       xlab = "Slugs (#/trap)", # x-axis title
       ylab = "Soybean plants (10,000/ha)", # y-axis title
       frame = FALSE, # remove frame
       xlim = c(min_slugs_observed, max_slugs_observed), # x-axis limits
       ylim = c(0, 40) # y-axis limits
  )
  # plot the predicted mean response for a given number of slugs per trap
  lines(pred2[,1] ~ newdata2$slugs_per_trap, col = 'black', lwd = 2)
  # Confidenceintervals - range in which the 'true' regression line lies
  # given a certain level of confidence (default is 95% confidence).
  # Plot the 95% CI for the mean response across range of slugs per trap
  lines(pred2[,2] ~ newdata2$slugs_per_trap, col = 'blue', lty = 2, lwd = 2)
  lines(pred2[,3] ~ newdata2$slugs_per_trap, col = 'blue', lty = 2, lwd = 2)
  legend("topright",
         legend = c("model-based estimate for mean", "95% CI"),
         pch = "|", col = c("black", "blue"))
}

