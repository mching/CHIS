# Table 2

library(stargazer)

# Stargazer output function
outputTable <- function() {
  coef.list <- list(exp(model1$coef), exp(model2$coef), exp(model3$coef))
  CI.list <- list(exp(confint(model1)), exp(confint(model2)), exp(confint((model3))))
  model.list <- list(model1, model2, model3)
  p.values <- lapply(model.list, function(x) summary(x)$coef[ ,4])
  
  stargazer(model1, model2, model3, 
            type = "text",
            dep.var.labels = c("DS Only", "SLH Only", "Either DS or SLH"), 
            coef = coef.list,
            ci.custom = CI.list,
            p = p.values,
            digits = 2,
            ci = T,
            single.row = T
  )
}

# Male
model1 <- svyglm(cf46 ~ male, design = rchis05, family = quasibinomial)
model2 <- svyglm(cf47 ~ male, design = rchis05, family = quasibinomial)
model3 <- svyglm(referred ~ male, design = rchis05, family = quasibinomial)
outputTable()

# Age
model1 <- svyglm(cf46 ~ srage.p, design = rchis05, family = quasibinomial)
model2 <- svyglm(cf47 ~ srage.p, design = rchis05, family = quasibinomial)
model3 <- svyglm(referred ~ srage.p, design = rchis05, family = quasibinomial)
outputTable()

### Birthweight
```{r}
model1 <- svyglm(cf46 ~ brthwk.p.i, design = rchis05, family = quasibinomial)
model2 <- svyglm(cf47 ~ brthwk.p.i, design = rchis05, family = quasibinomial)
model3 <- svyglm(referred ~ brthwk.p.i, design = rchis05, family = quasibinomial)
outputTable()

### Race
```{r}
model1 <- svyglm(cf46 ~ racehp2p, design = rchis05, family = quasibinomial)
model2 <- svyglm(cf47 ~ racehp2p, design = rchis05, family = quasibinomial)
model3 <- svyglm(referred ~ racehp2p, design = rchis05, family = quasibinomial)
outputTable()

### Hispanic Ethnicity
model1 <- svyglm(cf46 ~ srh.a.i, design = rchis05, family = quasibinomial)
model2 <- svyglm(cf47 ~ srh.a.i, design = rchis05, family = quasibinomial)
model3 <- svyglm(referred ~ srh.a.i, design = rchis05, family = quasibinomial)
outputTable()

### Below Poverty Level
model1 <- svyglm(cf46 ~ belowpovl, design = rchis05, family = quasibinomial)
model2 <- svyglm(cf47 ~ belowpovl, design = rchis05, family = quasibinomial)
model3 <- svyglm(referred ~ belowpovl, design = rchis05, family = quasibinomial)
outputTable()

### Uninsured Ever in Last Year
model1 <- svyglm(cf46 ~ unins.ever, design = rchis05, family = quasibinomial)
model2 <- svyglm(cf47 ~ unins.ever, design = rchis05, family = quasibinomial)
model3 <- svyglm(referred ~ unins.ever, design = rchis05, family = quasibinomial)
outputTable()

### PEDS High Risk
model1 <- svyglm(cf46 ~ pedsHiRisk, design = rchis05, family = quasibinomial)
model2 <- svyglm(cf47 ~ pedsHiRisk, design = rchis05, family = quasibinomial)
model3 <- svyglm(referred ~ pedsHiRisk, design = rchis05, family = quasibinomial)
outputTable()
