# first create a function to easily reproduce new data sets
# press ctrl+enter at the first line of the function to save the function in your environment

# this is all of our code pasted in from above
# we will also embed some extra lines of code here to save critical things that we want
# to keep track of: estimate for effect of slugs R^2
simulate_slugs_and_soybeans <- function(n, min_slugs_observed, max_slugs_observed, 
                                        intercept, slope, sd, 
                                        n_reps){
  
  if(n_reps > 1){
    
    intercepts <- vector(length = n_reps)
    slopes <- vector(length = n_reps)
    R2 <- vector(length = n_reps)

    for(i in 1:n_reps){
      
      # simulate some slug trap data (independent variable), for n plots, ranging from lows to highs seen in the field experiment
      # runif()
      slugs_per_trap <- runif(n=n, min=min_slugs_observed, max=max_slugs_observed)
      
      # simulate associated outcome of soybean plant density conditional (dependent variable) on slugs per trape
      # rnorm()
      linear_model <- (intercept + (slope * slugs_per_trap))
      soybean_density <- rnorm(n=n, mean=linear_model, sd=sd)
      
      # join the data into a 'data frame' structure
      mydata <- data.frame(slugs_per_trap, soybean_density)
      
      ##------------------------------------------------------------------------------
      # model the association
      
      # fit a linear regression model to our data
      # lm() creates a linear model 
      summary(fit1 <- lm(soybean_density ~ slugs_per_trap))
      
      # save important outputs
      # R-squared
      R2[i] <- summary(fit1)$r.squared
      # intercept term
      intercepts[i] <- summary(fit1)$coefficients[1,1]
      # effect of slug increase
      slopes[i] <- summary(fit1)$coefficients[2,1]
      
    }
    
    par(mfrow=c(1,2))
    
    # create a plot using classic r tools
    # plot our simulated data
    hist(intercepts, 
         xlab = "Intercept estimate",
         main = paste0(n_reps, " simulation experiments with \nn = ", n," and sd = ", sd),
         cex.main=0.75)
    abline(v=intercept, col = 'red', lwd = 2)
    abline(v=mean(intercepts), col = 'blue', lwd = 2, lty = 2)
    legend("topright", legend = c("true value", "mean estimate"), pch = "|", col = c("red", "blue"),
           cex=0.75)
    
    hist(slopes, 
         xlab = "Slope estimate",
         main = paste0(n_reps, " simulation experiments with \nn = ", n," and sd = ", sd),
         cex.main=0.75)
    abline(v=slope, col = 'red', lwd = 2)
    abline(v=mean(slopes), col = 'blue', lwd = 2, lty = 2)
    #legend("topright", legend = c("true value", "mean estimate"), pch = "|", col = c("red", "blue"))
    
    R2 = mean(R2)
    estimate_intercept = mean(intercepts)
    estimate_slope = mean(slopes)
    sd_intercept = sd(intercepts)
    sd_slope = sd(slopes)
    
  } else {
    
    # simulate some slug trap data (independent variable), for n plots, ranging from lows to highs seen in the field experiment
    # runif()
    slugs_per_trap <- runif(n=n, min=min_slugs_observed, max=max_slugs_observed)
    
    # simulate associated outcome of soybean plant density conditional (dependent variable) on slugs per trape
    # rnorm()
    linear_model <- (intercept + (slope * slugs_per_trap))
    soybean_density <- rnorm(n=n, mean=linear_model, sd=sd)
    
    # join the data into a 'data frame' structure
    mydata <- data.frame(slugs_per_trap, soybean_density)
    
    ##------------------------------------------------------------------------------
    # model the association
    
    # fit a linear regression model to our data
    # lm() creates a linear model 
    summary(fit1 <- lm(soybean_density ~ slugs_per_trap))
    
    # save important outputs
    # R-squared
    R2 <- summary(fit1)$r.squared
    # intercept term
    intercept_estimate <- summary(fit1)$coefficients[1,1]
    # effect of slug increase
    slope_estimate <- summary(fit1)$coefficients[2,1]
    
    ##------------------------------------------------------------------------------
    # visualize the model
    
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
    pred <- as.matrix(predict(fit1, newdata, interval = 'confidence'))
    
    par(mfrow=c(1,1), mar = c(2, 5, 2, 5)) # Go back to 1 fig column; set the margin on the sides to 5
    
    # create a plot using classic r tools
    # plot our simulated data
    plot(x = mydata$slugs_per_trap, # independent variable
         y = mydata$soybean_density, # dependent variable
         cex = 1.75, pch = 21, bg = 'gray', # size, shape, and colour of the data points
         xlab = "Slugs (#/trap)",
         ylab = "Soybean plants (10,000/ha)", 
         frame = FALSE,
         xlim = c(min_slugs_observed, max_slugs_observed),
         ylim = c(0, (max(mydata$soybean_density)+5)),
         main = paste0("simulation experiment with intercept = ", intercept, 
                       ",\nslope = ", slope, " and sd = ", sd),
         cex.main = 0.8
    )
    
    # plot the predicted mean response for a given number of slugs per trap
    lines(pred[,1] ~ newdata$slugs_per_trap, col = 'black', lwd = 2)
    # plot the lower and upper 95% cI for the mean response for a given number of slugs per trap
    lines(pred[,2] ~ newdata$slugs_per_trap, col = 'blue', lty = 2, lwd = 2)
    lines(pred[,3] ~ newdata$slugs_per_trap, col = 'blue', lty = 2, lwd = 2)
    
    # now let's add the 'true' regression line and see how close our model got
    expected_means <- intercept + slope * newdata$slugs_per_trap
    lines(expected_means ~ newdata$slugs_per_trap, col = 'red', lty = 2, lwd = 2)
    
    legend("topright", legend = c("expected mean", "model-based estimate for mean", "95% CI"), 
           pch = "|", col = c("red", "black", "blue"))
    
    estimate_intercept = intercept_estimate
    estimate_slope = slope_estimate
    sd_intercept = NULL
    sd_slope = NULL
    
  }
  
  
  
  ## --------------------------------------------------
  # Return stuff
  return(list(
    R2 = R2,
    estimate_intercept = estimate_intercept,
    estimate_slope = estimate_slope,
    sd_intercept = sd_intercept,
    sd_slope = sd_slope
  ))
}
