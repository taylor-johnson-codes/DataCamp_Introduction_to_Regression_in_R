# Coefficient of determination (sometimes called "r-squared"): the proportion of the variance in the response variable that is predictable from the explanatory variable
# 1 means perfect fit; 0 means the worst possible fit
# The coefficient of determination is a measure of how well the linear regression line fits the observed values. For simple linear regression, it is equal to the square of the correlation between the explanatory and response variables.

# Residual standard error (RSE): a "typical" difference between a prediction and an observed response; has the same unit as the response variable
# Residual standard error (RSE) is a measure of the typical size of the residuals. Equivalently, it's a measure of how badly wrong you can expect predictions to be. Smaller numbers are better, with zero being a perfect fit to the data.

# Degrees of freedom equals the number of observations minus the number of model coefficients

library(broom)
library(ggplot2)
library(ggfortify)
library(dplyr)

# the dataset is NOT loaded here so the code won't work

# Print a summary of mdl_click_vs_impression_orig
summary(mdl_click_vs_impression_orig)

# Print a summary of mdl_click_vs_impression_trans
summary(mdl_click_vs_impression_trans)

# Get coeff of determination for mdl_click_vs_impression_orig
mdl_click_vs_impression_orig %>% 
  # Get the model-level details
  glance() %>% 
  # Pull out r.squared
  pull(r.squared)

# Do the same for the transformed model
mdl_click_vs_impression_trans %>% 
  # Get the model-level details
  glance() %>% 
  # Pull out r.squared
  pull(r.squared)

# Get RSE for mdl_click_vs_impression_orig
mdl_click_vs_impression_orig %>% 
  # Get the model-level details
  glance() %>% 
  # Pull out sigma
  pull(sigma)

# Do the same for the transformed model
mdl_click_vs_impression_trans %>% 
  # Get the model-level details
  glance() %>% 
  # Pull out sigma
  pull(sigma)

# autoplot() lets you specify which diagnostic plots you are interested in.
# 1 residuals vs. fitted values
# 2 Q-Q plot
# 3 scale-location

# Plot the three diagnostics for mdl_price_vs_conv
autoplot(mdl_price_vs_conv, which = 1:3, nrow = 3, ncol = 1)

# Leverage measures how unusual or extreme the explanatory variables are for each observation. 
# Very roughly, a high leverage means that the explanatory variable has values that are different to other points in the dataset. 
# In the case of simple linear regression, where there is only one explanatory value, this typically means values with a very high or very low explanatory value.

# Influence measures how much a model would change if each observation was left out of the model calculations, one at a time. 
# That is, it measures how different the prediction line would look if you ran a linear regression on all data points except that point, compared to running a linear regression on the whole dataset.

# Augment mdl_price_vs_dist, then arrange observations by descending influence (.hat), and get the head of the results.
mdl_price_vs_dist %>% 
  # Augment the model
  augment() %>% 
  # Arrange rows by descending leverage
  arrange(desc(.hat)) %>% 
  # Get the head of the dataset
  head()

# Augment mdl_price_vs_dist, then arrange observations by descending influence (.cooksd), and get the head of the results.
mdl_price_vs_dist %>% 
  # Augment the model
  augment() %>% 
  # Arrange rows by descending Cook's distance
  arrange(desc(.cooksd)) %>% 
  # Get the head of the dataset
  head()

# Plot the three outlier diagnostic plots (numbered 4 to 6) for mdl_price_vs_dist. Use a layout of three rows and one column.
# Plot the three outlier diagnostics for mdl_price_vs_conv
autoplot(mdl_price_vs_dist, which = 4:6, nrow = 3, ncol = 1)