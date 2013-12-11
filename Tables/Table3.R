# Table 3 multivariable

stargazer(mvreg.dev, mvreg.dev.int, mvreg.speech, mvreg.speech.int, mvreg.either, mvreg.either.int, 
          type = "text",
          title = "Table: Multivariable Logistic Regression Models",
          out = "./Tables/Table3.txt",
          ci = F,
          digits = 2,
          single.row = F, 
          star.cutoffs = c(0.05, 0.01, 0.001),
          dep.var.labels = c("DS", "SLH", "Either DS or SLH"), 
           covariate.labels = c("Male",
                                "Age",
                                "Birthweight (kg)",
                                "Latino",
                                "Asian",
                                "African American",
                                "Pacific Islander/Other",
                                "Non-Hispanic",
                                "Below 100% Federal Poverty Level",
                                "Uninsured in Past Year", 
                                "PEDS High Risk",
                                "PEDS High Risk * Age",
                                "PEDS High Risk * Birthweight")
)

# Table with just the interaction term models, confidence intervals
stargazer(mvreg.dev.int, mvreg.speech.int, mvreg.either.int, 
          type = "text",
          title = "Table: Multivariable Logistic Regression Models",
          out = "./Tables/Table3.txt",
          ci = T,
          digits = 2,
          single.row = F,
          star.cutoffs = c(0.05, 0.01, 0.001),
          dep.var.labels = c("DS", "SLH", "Either DS or SLH")
)