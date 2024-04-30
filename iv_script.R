
n <- 10000

set.seed(123)
z <- rnorm(n)
u <- rnorm(n, 0 , 3)
treatment <- rbinom(n, 1, plogis(z - u + rnorm(n, 0, 2)))

y <- 2*treatment + u

reg_vies <- lm(y ~ treatment)
summary(reg_vies)

reg_1s <- lm(treatment ~ z)
reg_2s <- lm(y ~ predict(reg_1s))
library(stargazer)
stargazer(reg_vies, reg_1s, reg_2s, type = "latex",  title="OLS e 2SLS")

library(ivreg)

iv_model <- ivreg(y ~ treatment|z)
summary(iv_model) # erro padrão é diferente

library(lmtest)
coeftest(iv_model, vcov = vcovHC, type = "HC1")

## causalidade reversa

n <- 10000
y <- rnorm(n)
z <- rnorm(n)
treatment <- z + y
reg_reversa <- lm(y ~ treatment)
summary(reg_reversa)

iv_model_rev <- ivreg(y ~ treatment|z)
summary(iv_model_rev)


## múltiplos instrumentos

z1 <- rnorm(n)
z2 <- rnorm(n)
z3 <- rnorm(n)
u <- rnorm(n)
w <- rnorm(n)
treatment <- z1 - z2 + u + w
y <- treatment - u + z3 + w

reg_ols <- lm(y ~ treatment + z1 + z2 + z3 + w)
summary(reg_ols)

iv_model_mult <- ivreg(y ~ treatment + w | w + z1 + z2 + z3)
summary(iv_model_mult)

# testing for weak instruments
library(car)

linearHypothesis(reg_1s)


# gather robust standard errors in a list
rob_se <- list(sqrt(diag(vcovHC(cig_ivreg_diff1, type = "HC1"))),
               sqrt(diag(vcovHC(cig_ivreg_diff2, type = "HC1"))),
               sqrt(diag(vcovHC(cig_ivreg_diff3, type = "HC1"))))


# testing instruments
# first-stage regressions
mod_relevance1 <- lm(treatment ~ w + z1)
mod_relevance2 <- lm(treatment ~ w + z2)
mod_relevance3 <- lm(treatment ~ w + z3)
mod_relevance4 <- lm(treatment ~ w + z1 + z2 + z3)

linearHypothesis(mod_relevance1, 
                 "z1 = 0", 
                 vcov = vcovHC, type = "HC1")

linearHypothesis(mod_relevance2, 
                 "z3 = 0", 
                 vcov = vcovHC, type = "HC1")
# generate table
stargazer(cig_ivreg_diff1, cig_ivreg_diff2,cig_ivreg_diff3,
          header = FALSE, 
          type = "html",
          omit.table.layout = "n",
          digits = 3, 
          column.labels = c("IV: salestax", "IV: cigtax", "IVs: salestax, cigtax"),
          dep.var.labels.include = FALSE,
          dep.var.caption = "Dependent Variable: 1985-1995 Difference in Log per Pack Price",
          se = rob_se)