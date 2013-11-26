library(stargazer)

# Table without interaction terms

# Present as Odds Ratios
# see http://stackoverflow.com/questions/16236560/odds-ratios-instead-of-logits-in-stargazer-latex-output

coef.list <- list(exp(mvreg.dev$coef), exp(mvreg.speech$coef), exp(mvreg.either$coef))
CI.list <- list(exp(confint(mvreg.dev)), exp(confint(mvreg.speech)), exp(confint((mvreg.either))))
model.list <- list(mvreg.dev, mvreg.speech, mvreg.either)
p.values <- lapply(model.list, function(x) summary(x)$coef[ ,4])

stargazer(mvreg.dev, mvreg.speech, mvreg.either, 
          type = "text",
          title = "Table: Multivariable Logistic Regression Models",
          dep.var.labels = c("DS Only", "SLH Only", "Either DS or SLH"), 
          coef = coef.list,
          ci.custom = CI.list,
          p = p.values,
          digits = 2,
          ci = T,
          covariate.labels = c("Male",
                               "Age",
                               "Birthweight (kg)",
                               "Latino",
                               "Asian",
                               "African American",
                               "Pacific Islander/Other",
                               "Non-Hispanic",
                               "Below 100% Federal Poverty Level",
                               "Uninsured in Past Year"                               
                               )
)

# Interaction table. More recent version in ./Tables/Table3.R