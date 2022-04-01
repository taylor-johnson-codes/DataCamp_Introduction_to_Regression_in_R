# When the response variable is logical, all the points lie on the y equals zero and y equals one lines, making it difficult to see what is happening. 
# It's not clear how the explanatory variable is distributed on each line. This can be solved with a histogram of the explanatory variable, faceted on the response.

library(ggplot2)
library(dplyr)
library(yardstick)

# the dataset is NOT loaded here so the code won't work

# Using churn, plot time_since_last_purchase
ggplot(churn, aes(time_since_last_purchase)) +
  # as a histogram with binwidth 0.25
  geom_histogram(binwidth = 0.25) +
  # faceted in a grid with has_churned on each row
  facet_grid(rows = vars(has_churned))

# Redraw the plot with time_since_first_purchase
ggplot(churn, aes(time_since_first_purchase)) +
  # as a histogram with binwidth 0.25
  geom_histogram(binwidth = 0.25) +
  # faceted in a grid with has_churned on each row
  facet_grid(rows = vars(has_churned))

# Using churn plot has_churned vs. time_since_first_purchase
ggplot(churn, aes(time_since_first_purchase, has_churned)) +
  # Make it a scatter plot
  geom_point() +
  # Add an lm trend line, no std error ribbon, colored red
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  # Add a glm trend line, no std error ribbon, binomial family
  geom_smooth(method = "glm", se = FALSE, method.args = list(family = binomial))

# Linear regression and logistic regression are special cases of a broader type of models called generalized linear models ("GLMs").
# A linear regression makes the assumption that the residuals follow a Gaussian (normal) distribution. By contrast, a logistic regression assumes that residuals follow a binomial distribution.

# Fit a logistic regression of churn vs. length of relationship using the churn dataset
mdl_churn_vs_relationship <- glm(has_churned ~ time_since_first_purchase, data = churn, family = binomial)

# See the result
mdl_churn_vs_relationship

# There are four main ways of expressing the prediction from a logistic regression model.
# Firstly, since the response variable is either "yes" or "no", you can make a prediction of the probability of a "yes". 

# Make a data frame of predicted probabilities
prediction_data <- explanatory_data %>% 
  mutate(has_churned = predict(mdl_churn_vs_relationship, explanatory_data, type = "response"))

# See the result
prediction_data

# Update the plot
plt_churn_vs_relationship +
  # Add points from prediction_data, colored yellow, size 2
  geom_point(data = prediction_data, color = "yellow", size = 2)

# When explaining your results to a non-technical audience, you may wish to side-step talking about probabilities and simply explain the most likely outcome. 
# That is, rather than saying there is a 60% chance of a customer churning, you say that the most likely outcome is that the customer will churn. The tradeoff here is easier interpretation at the cost of nuance.

# Update the data frame
prediction_data <- explanatory_data %>% 
  mutate(has_churned = predict(mdl_churn_vs_relationship, explanatory_data, type = "response"),
    # Add the most likely churn outcome
    most_likely_outcome = round(has_churned))

# See the result
prediction_data

# Update the plot
plt_churn_vs_relationship +
  # Add most likely outcome points from prediction_data, colored yellow, size 2
  geom_point(data = prediction_data, aes(y = most_likely_outcome), color = "yellow", size = 2)

# Update the data frame
prediction_data <- explanatory_data %>% 
  mutate(has_churned = predict(mdl_churn_vs_relationship, explanatory_data, type = "response"),
    # Add the odds ratio
    odds_ratio = has_churned / (1 - has_churned))

# See the result
prediction_data

# Using prediction_data, plot odds_ratio vs. time_since_first_purchase
ggplot(prediction_data, (aes(time_since_first_purchase, odds_ratio))) +
  # Make it a line plot
  geom_line() +
  # Add a dotted horizontal line at y = 1
  geom_hline(yintercept = 1, linetype = "dotted")

# One downside to probabilities and odds ratios for logistic regression predictions is that the prediction lines for each are curved. 
# This makes it harder to reason about what happens to the prediction when you make a change to the explanatory variable. 
# The logarithm of the odds ratio (the "log odds ratio") does have a linear relationship between predicted response and explanatory variable. 
# That means that as the explanatory variable changes, you don't see dramatic changes in the response metric - only linear changes.
# Since the actual values of log odds ratio are less intuitive than (linear) odds ratio, for visualization purposes it's usually better to plot the odds ratio and apply a log transformation to the y-axis scale.

# Update the data frame
prediction_data <- explanatory_data %>% 
  mutate(has_churned = predict(mdl_churn_vs_relationship, explanatory_data, type = "response"),
    odds_ratio = has_churned / (1 - has_churned),
    # Add the log odds ratio from odds_ratio
    log_odds_ratio = log(odds_ratio),
    # Add the log odds ratio using predict()
    log_odds_ratio2 = predict(mdl_churn_vs_relationship, explanatory_data))

# See the result
prediction_data

# Update the plot
ggplot(prediction_data, aes(time_since_first_purchase, odds_ratio)) +
  geom_line() +
  geom_hline(yintercept = 1, linetype = "dotted") +
  # Use a logarithmic y-scale
  scale_y_log10()

# A confusion matrix (occasionally called a confusion table) is the basis of all performance metrics for models with a categorical response (such as a logistic regression).
# It contains the counts of each actual response-predicted response pair. In this case, where there are two possible responses (churn or not churn), there are four overall outcomes.
# The customer churned and the model predicted that.
# The customer churned but the model didn't predict that.
# The customer didn't churn but the model predicted they did.
# The customer didn't churn and the model predicted that.

# Get the actual responses from the dataset
actual_response <- churn$has_churned

# Get the "most likely" responses from the model
predicted_response <- round(fitted(mdl_churn_vs_relationship))

# Create a table of counts
outcomes <- table(predicted_response, actual_response)

# See the result
outcomes

# Having the confusion matrix as a table object is OK, but a little hard to program with. By converting this to a yardstick confusion matrix object, you get methods for plotting and extracting performance metrics.

# Convert outcomes to a yardstick confusion matrix
confusion <- conf_mat(outcomes)

# Plot the confusion matrix
autoplot(confusion)

# Get performance metrics for the confusion matrix
summary(confusion, event_level = "second")

# yardstick can compute lots of performance metrics from a confusion matrix. For logistic regression, three of them in particular are important: accuracy, sensitivity, and specificity.