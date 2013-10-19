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