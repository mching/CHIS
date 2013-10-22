# Simulate a polynomial interaction
# In group 1, there is a quadratic relationship between the exposure and the outcome
# In group 2, there is no relationship between the exposure and the outcome

library(ggplot2)
set.seed(1)

# Group 1
x1 <- rep(0:5, 20)
y1 <- (x1-2)^2 + rnorm(length(x1))
qplot(x1, y1)

# Group 2
x2 <- rep(0:5, 20)
y2 <- rnorm(length(x2))
qplot(x2, y2)

# Combined Group
age <- c(x1, x2)
group <- as.factor(c(rep(1, length(x1)), rep(2, length(x1))))
y <- c(y1, y2)

dat <- data.frame(outcome = y, age = age, group = group)

# Models
model1 <- lm(outcome ~ age + group, data = dat)
summary(model1)

model2 <- lm(outcome ~ age + group + age*group, data = dat)
summary(model2)

model3 <- lm(outcome ~ age + group + I(age^2), data = dat)
summary(model3)

model4 <- lm(outcome ~ age + group + I(age^2)*group, data = dat)
summary(model4)

# Plots

ggplot(dat, aes(x = age, y = outcome, color = group)) + 
  geom_point(size = 3) +
  geom_smooth(method = lm) +
  geom_smooth(method = lm, formula = y ~ x + I(x^2))

# Maybe what we need to do is stratify by age and get age specific estimates
# or stratify by PEDS and get PEDS specific estimates