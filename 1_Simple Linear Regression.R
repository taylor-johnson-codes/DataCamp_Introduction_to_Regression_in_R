library(ggplot2)
library(dplyr)

# the dataset is NOT loaded here so the code won't work

# Draw a scatter plot of n_convenience vs. price_twd_msq
ggplot(taiwan_real_estate, aes(n_convenience, price_twd_msq)) + 
  geom_point()

# Make points 50% transparent
ggplot(taiwan_real_estate, aes(n_convenience, price_twd_msq)) +
  geom_point(alpha = 0.5)

# Add a linear trend line without a confidence ribbon
ggplot(taiwan_real_estate, aes(n_convenience, price_twd_msq)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE)

# While ggplot can display a linear regression trend line using geom_smooth(), it doesn't give you access to the intercept and slope as variables, or allow you to work with the model results as variables. 
# That means that sometimes you'll need to run a linear regression yourself.

# Run a linear regression of price_twd_msq vs. n_convenience
lm(price_twd_msq ~ n_convenience, data = taiwan_real_estate)

# If the explanatory variable is categorical, the scatter plot that you used before to visualize the data doesn't make sense.
# Instead, a good option is to draw a histogram for each category.

# Using taiwan_real_estate, plot price_twd_msq
ggplot(taiwan_real_estate, aes(price_twd_msq)) +
  # Make it a histogram with 10 bins
  geom_histogram(bins = 10) +
  # Facet the plot so each house age group gets its own panel
  facet_wrap(vars(house_age_years))

# A good way to explore categorical variables is to calculate summary statistics such as the mean for each category. 

summary_stats <- taiwan_real_estate %>% 
  # Group by house age
  group_by(house_age_years) %>% 
  # Summarize to calculate the mean house price/area
  summarize(mean_by_group = mean(price_twd_msq))

# See the result
summary_stats

# Linear regressions also work with categorical explanatory variables. In this case, the code to run the model is the same, but the coefficients returned by the model are different. 

# Run a linear regression of price_twd_msq vs. house_age_years
mdl_price_vs_age <- lm(price_twd_msq ~ house_age_years, data = taiwan_real_estate)

# See the result
mdl_price_vs_age

# Update the model formula to remove the intercept
mdl_price_vs_age_no_intercept <- lm(price_twd_msq ~ house_age_years + 0, data = taiwan_real_estate)

# See the result
mdl_price_vs_age_no_intercept