# Table 3 multivariable

stargazer(mvreg.dev, mvreg.dev.int, mvreg.speech, mvreg.speech.int, mvreg.either, mvreg.either.int, 
          type = "text",
          title = "Table: Multivariable Logistic Regression Models",
          out = "./Tables/Table3.txt",
          digits = 2,
          single.row = T,
          dep.var.labels = c("DS Only", "SLH Only", "Either DS or SLH")
#           covariate.labels = c("Male",
#                                "Age",
#                                "Birthweight (kg)",
#                                "Latino",
#                                "Asian",
#                                "African American",
#                                "Pacific Islander/Other",
#                                "Non-Hispanic",
#                                "Below 100% Federal Poverty Level",
#                                "Uninsured in Past Year",                               
#                                "PEDS High Risk * Age",
#                                "PEDS High Risk * Birthweight")
)