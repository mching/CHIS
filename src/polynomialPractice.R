# Practice with polynomial regression

set.seed(1)
x <- 1:20
y <- (x-8)^2 + x + rnorm(20)

head(x)
head(y)

plot(x, y)

# Center the independent variable
x.centered <- x - mean(x)

# Center the predicted variable
y.centered <- y - mean(y)

plot(x.centered, y.centered)

# Create higher order variable (squared)
x.centered2 <- x.centered^2

# Create models
model.linear <- lm(y.centered ~ x.centered)
summary(model.linear)

model.polynomial <- lm(y.centered ~ x.centered + x.centered2)
summary(model.polynomial)

# Compare with ANOVA
anova(model.linear, model.polynomial)

# What if you don't center first?
model.linear.uncentered <- lm(y ~ x)
summary(model.linear.uncentered)

# Other ways of making polynomial model
x2 <- x^2
model.polynomial.uncentered <- lm(y ~ x + I(x^2))
model.polynomial.uncentered2 <- lm(y ~ x + x2)
model.polynomial.uncentered3 <- lm(y ~ poly(x, 2)) # What is an "orthogonal polynomial"?

summary(model.polynomial.uncentered)
summary(model.polynomial.uncentered2)
summary(model.polynomial.uncentered3)