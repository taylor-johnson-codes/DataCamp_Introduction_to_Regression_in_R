# Perhaps the most useful feature of statistical models like linear regression is that you can make predictions. 
# That is, you specify values for each of the explanatory variables, feed them to the model, and you get a prediction for the corresponding response variable.
# The code flow is as follows:
# explanatory_data <- tibble(explanatory_var = some_values)
# explanatory_data %>% mutate(response_var = predict(model, explanatory_data))

library(dplyr)
library(ggplot2)
library(broom)
library(tibble)

# datasets are NOT loaded here so the code won't work

# Create a tibble with n_convenience column from zero to ten
explanatory_data <- tibble(n_convenience = 0:10)

# Use mdl_price_vs_conv to predict with explanatory_data
predict(mdl_price_vs_conv, explanatory_data)

# Edit this, so predictions are stored in prediction_data
prediction_data <- predict(mdl_price_vs_conv, explanatory_data)

# See the result
prediction_data <- explanatory_data %>%
  mutate(price_twd_msq = prediction_data)

# Add to the plot
ggplot(taiwan_real_estate, aes(n_convenience, price_twd_msq)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  # Add a point layer of prediction data, colored yellow
  geom_point(data = prediction_data, color = "yellow")

# To test the limits of the model's ability to predict, try some impossible situations.
# Define a tibble where n_convenience is -1
minus_one <- tibble(n_convenience = -1)

# Define a tibble where n_convenience is 2.5
two_pt_five <- tibble(n_convenience = 2.5)

# The variable returned by lm() that contains the model object has many elements.
# In order to perform further analysis on the model results, you need to extract the useful bits of it. 
# The model coefficients, the fitted values, and the residuals are perhaps the most important bits of the linear model object.

# Get the model coefficients of mdl_price_vs_conv
coefficients(mdl_price_vs_conv)

# Get the fitted values of mdl_price_vs_conv
fitted(mdl_price_vs_conv)

# Get the residuals of mdl_price_vs_conv
residuals(mdl_price_vs_conv)

# Print a summary of mdl_price_vs_conv
summary(mdl_price_vs_conv)

# You can manually calculate the predictions from the model coefficients.
# When making predictions in real life, it is better to use predict(), but doing this manually is helpful to reassure yourself that predictions aren't magic—they are simply arithmetic.
# In fact, for a simple linear regression, the predicted value is just the intercept plus the slope times the explanatory variable.
# response = intercept + slope * explanatory

# Get the coefficients of mdl_price_vs_conv
coeffs <- coefficients(mdl_price_vs_conv)

# Get the intercept
intercept <- coeffs[1]

# Get the slope
slope <- coeffs[2]

explanatory_data %>% 
  mutate(
    # Manually calculate the predictions
    price_twd_msq = intercept + slope * n_convenience )

# Compare to the results from predict()
predict(mdl_price_vs_conv, explanatory_data)

# Many programming tasks are easier if you keep all your data inside data frames. This is particularly true if you are a tidyverse fan, where dplyr and ggplot2 require you to use data frames.
# The broom package contains functions that decompose models into three data frames: one for the coefficient-level elements (the coefficients themselves, as well as p-values for each coefficient), the observation-level elements (like fitted values and residuals), and the model-level elements (mostly performance metrics).
# The functions in broom are generic. That is, they work with many model types, not just linear regression model objects. They also work with logistic regression model objects, and many other types of model.

# Get the coefficient-level elements of the model
tidy(mdl_price_vs_conv)

# Get the observation-level elements of the model
augment(mdl_price_vs_conv)

# Get the model-level elements of the model
glance(mdl_price_vs_conv)

# Regression to the mean is an important concept in investing.
# Using sp500_yearly_returns, plot return_2019 vs. return_2018
ggplot(sp500_yearly_returns, aes(return_2018, return_2019)) +
  # Make it a scatter plot
  geom_point() +
  # Add a line at y = x, colored green, size 1
  geom_abline(color = "green", size = 1) +
  # Add a linear regression trend line, no std. error ribbon
  geom_smooth(method = "lm", se = FALSE) +
  # Fix the coordinate ratio
  coord_fixed()

# Run a linear regression on return_2019 vs. return_2018 using sp500_yearly_returns
mdl_returns <- lm(return_2019 ~ return_2018, sp500_yearly_returns)

# See the result
mdl_returns

# Run a linear regression on return_2019 vs. return_2018 using sp500_yearly_returns
mdl_returns <- lm(return_2019 ~ return_2018, data = sp500_yearly_returns))

# Create a data frame with return_2018 at -1, 0, and 1 
explanatory_data <- tibble(return_2018 = c(-1, 0, 1))

# Use mdl_returns to predict with explanatory_data
predict(mdl_returns, explanatory_data)

# If there is no straight line relationship between the response variable and the explanatory variable, it is sometimes possible to create one by transforming one or both of the variables. 

# Run the code to see the plot
# Edit so x-axis is square root of dist_to_mrt_m
ggplot(taiwan_real_estate, aes(sqrt(dist_to_mrt_m), price_twd_msq)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)

# Run a linear regression of price_twd_msq vs. square root of dist_to_mrt_m using taiwan_real_estate
mdl_price_vs_dist <- lm(price_twd_msq ~ sqrt(dist_to_mrt_m), taiwan_real_estate)

# See the result
mdl_price_vs_dist

# Run a linear regression of price_twd_msq vs. square root of dist_to_mrt_m using taiwan_real_estate
mdl_price_vs_dist <- lm(price_twd_msq ~ sqrt(dist_to_mrt_m), data = taiwan_real_estate)

explanatory_data <- tibble(dist_to_mrt_m = seq(0, 80, 10) ^ 2)

# Use mdl_price_vs_dist to predict explanatory_data
prediction_data <- explanatory_data %>%
  mutate(price_twd_msq = predict(mdl_price_vs_dist, explanatory_data))

# See the result
prediction_data

# From previous steps
mdl_price_vs_dist <- lm(price_twd_msq ~ sqrt(dist_to_mrt_m), data = taiwan_real_estate)

explanatory_data <- tibble(dist_to_mrt_m = seq(0, 80, 10) ^ 2)

prediction_data <- explanatory_data %>% 
  mutate(price_twd_msq = predict(mdl_price_vs_dist, explanatory_data))

ggplot(taiwan_real_estate, aes(sqrt(dist_to_mrt_m), price_twd_msq)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  # Add points from prediction_data, colored green, size 5
  geom_point(data = prediction_data, color = "green", size = 5)

# The response variable can be transformed too, but this means you need an extra step at the end to undo that transformation. That is, you "back transform" the predictions.

# Run the code to see the plot
# Edit to raise x, y aesthetics to power 0.25
ggplot(ad_conversion, aes(n_impressions ^ 0.25, n_clicks ^ 0.25)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)

# Run a linear regression of n_clicks to the power 0.25 vs. n_impressions to the power 0.25 using ad_conversion
mdl_click_vs_impression <- lm(I(n_clicks^0.25) ~ I(n_impressions^0.25), data = ad_conversion)

# Run a linear regression of n_clicks to the power 0.25 vs. n_impressions to the power 0.25 using ad_conversion
mdl_click_vs_impression <- lm(I(n_clicks ^ 0.25) ~ I(n_impressions ^ 0.25), data = ad_conversion)

explanatory_data <- tibble(n_impressions = seq(0, 3e6, 5e5))

prediction_data <- explanatory_data %>% 
  mutate(
    # Use mdl_click_vs_impression to predict n_clicks ^ 0.25
    n_clicks_025 = predict(mdl_click_vs_impression, explanatory_data),
    # Back transform to get n_clicks
    n_clicks = n_clicks_025 ^ 4 )