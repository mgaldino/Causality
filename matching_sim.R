library(ggplot2)
library(tidyverse)
library(MatchIt)
set.seed(123)  # For reproducibility

# Sample size
n <- 100

# Covariate X
x1 <- rnorm(n, mean = 0, sd = 2)
# Treatment assignment D, biased by X
D <- rbinom(n, 1, prob = plogis(-0.3*X^2 + rnorm(n,.8,1)))

# Outcome Y, non-linear in X and different intercept for treatment
Y <- 1 + X - X^2 + 2 * D + rnorm(n)

#plot
df <- data.frame(y=Y, x=X, t=D, x2=x2) %>%
  mutate(t = ifelse(t==1, "T", "C"))

ggplot(df, aes(y=y, x=x, label = t)) + geom_text()

# Simple linear model (OLS)
ols_model <- lm(Y ~ X + D)
summary(ols_model)

ols_model <- lm(Y ~ X + I(X^2) + D)
summary(ols_model)


# Matching using the MatchIt package
library(MatchIt)
match_model <- matchit(D ~ X, method = "nearest", data = data.frame(X, D, Y))
matched_data <- match.data(match_model)

summary(match_model)
plot(match_model, type = "jitter", interactive = FALSE)

# OLS on matched data
matched_ols_model <- lm(Y ~ X + D, data = matched_data, weights = weights)
summary(matched_ols_model)

# need replacement
match_model_rep <- matchit(D ~ X, method = "nearest", data = data.frame(X, D, Y),
                       replace=TRUE)
summary(match_model_rep)
plot(match_model_rep, type = "jitter", interactive = FALSE)


match_model_rep_data <- match.data(match_model_rep)

plot(summary(match_model_rep))

# matched_ols_model_rep <- lm(Y ~ X + D, data = match_model_rep_data,
#                             weights = weights)
# summary(matched_ols_model_rep)

matched_ols_model_rep <- lm(Y ~ D, data = match_model_rep_data,
                            weights = weights)
summary(matched_ols_model_rep)
# marginal effects
library(marginaleffects)

avg_comparisons(matched_ols_model_rep,
                variables = "D",
                vcov = "HC1",
                newdata = subset(match_model_rep_data, D == 1),
                wts = "weights")
