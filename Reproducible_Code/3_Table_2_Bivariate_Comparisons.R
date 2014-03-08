####################################################################################
# This file contains code to generate the bivariate estimates for Table 2 using
# simple logistic regression.
# 
# 
# Michael Ching, MD, MPH
#
#
# note: cf46 is Developmental Specialist referral
# cf47 is Speech, language, hearing referral
# referred is the combined DS or SLH referral
####################################################################################

### PEDS High Risk
model1 <- svyglm(cf46 ~ pedsHiRisk, design = rchis05, family = quasibinomial)
model2 <- svyglm(cf47 ~ pedsHiRisk, design = rchis05, family = quasibinomial)
model3 <- svyglm(referred ~ pedsHiRisk, design = rchis05, family = quasibinomial)

stargazer(model1, model2, model3, type = "text")

cbind(OddsRatio = exp(model1$coef), exp(confint(model1)))
cbind(OddsRatio = exp(model2$coef), exp(confint(model2)))
cbind(OddsRatio = exp(model3$coef), exp(confint(model3)))

### Gender
model1 <- svyglm(cf46 ~ male, design = rchis05, family = quasibinomial)
model2 <- svyglm(cf47 ~ male, design = rchis05, family = quasibinomial)
model3 <- svyglm(referred ~ male, design = rchis05, family = quasibinomial)

cbind(OddsRatio = exp(model1$coef), exp(confint(model1)))
cbind(OddsRatio = exp(model2$coef), exp(confint(model2)))
cbind(OddsRatio = exp(model3$coef), exp(confint(model3)))

stargazer(model1, model2, model3, type = "text",
          dep.var.labels = c("DS Only", "SLH Only", "Either DS or SLH"))

### Age
model1 <- svyglm(cf46 ~ srage.p, design = rchis05, family = quasibinomial)
model2 <- svyglm(cf47 ~ srage.p, design = rchis05, family = quasibinomial)
model3 <- svyglm(referred ~ srage.p, design = rchis05, family = quasibinomial)

cbind(OddsRatio = exp(model1$coef), exp(confint(model1)))
cbind(OddsRatio = exp(model2$coef), exp(confint(model2)))
cbind(OddsRatio = exp(model3$coef), exp(confint(model3)))

stargazer(model1, model2, model3, type = "text",
          dep.var.labels = c("DS Only", "SLH Only", "Either DS or SLH"))

### Birthweight
model1 <- svyglm(cf46 ~ brthwk.p.i, design = rchis05, family = quasibinomial)
model2 <- svyglm(cf47 ~ brthwk.p.i, design = rchis05, family = quasibinomial)
model3 <- svyglm(referred ~ brthwk.p.i, design = rchis05, family = quasibinomial)

cbind(OddsRatio = exp(model1$coef), exp(confint(model1)))
cbind(OddsRatio = exp(model2$coef), exp(confint(model2)))
cbind(OddsRatio = exp(model3$coef), exp(confint(model3)))

stargazer(model1, model2, model3, type = "text",
          dep.var.labels = c("DS Only", "SLH Only", "Either DS or SLH"))

### Race
model1 <- svyglm(cf46 ~ racehp2p, design = rchis05, family = quasibinomial)
model2 <- svyglm(cf47 ~ racehp2p, design = rchis05, family = quasibinomial)
model3 <- svyglm(referred ~ racehp2p, design = rchis05, family = quasibinomial)

stargazer(model1, model2, model3, type = "text", 
          dep.var.labels = c("DS Only", "SLH Only", "Either DS or SLH"))

cbind(OddsRatio = exp(model1$coef), exp(confint(model1)))
cbind(OddsRatio = exp(model2$coef), exp(confint(model2)))
cbind(OddsRatio = exp(model3$coef), exp(confint(model3)))

### Hispanic Ethnicity
model1 <- svyglm(cf46 ~ srh.a.i, design = rchis05, family = quasibinomial)
model2 <- svyglm(cf47 ~ srh.a.i, design = rchis05, family = quasibinomial)
model3 <- svyglm(referred ~ srh.a.i, design = rchis05, family = quasibinomial)

stargazer(model1, model2, model3, type = "text",
          dep.var.labels = c("DS Only", "SLH Only", "Either DS or SLH"))

cbind(OddsRatio = exp(model1$coef), exp(confint(model1)))
cbind(OddsRatio = exp(model2$coef), exp(confint(model2)))
cbind(OddsRatio = exp(model3$coef), exp(confint(model3)))

### Below Poverty Level
model1 <- svyglm(cf46 ~ belowpovl, design = rchis05, family = quasibinomial)
model2 <- svyglm(cf47 ~ belowpovl, design = rchis05, family = quasibinomial)
model3 <- svyglm(referred ~ belowpovl, design = rchis05, family = quasibinomial)

stargazer(model1, model2, model3, type = "text",
          dep.var.labels = c("DS Only", "SLH Only", "Either DS or SLH"))

cbind(OddsRatio = exp(model1$coef), exp(confint(model1)))
cbind(OddsRatio = exp(model2$coef), exp(confint(model2)))
cbind(OddsRatio = exp(model3$coef), exp(confint(model3)))

### Uninsured Ever in Last Year
model1 <- svyglm(cf46 ~ unins.ever, design = rchis05, family = quasibinomial)
model2 <- svyglm(cf47 ~ unins.ever, design = rchis05, family = quasibinomial)
model3 <- svyglm(referred ~ unins.ever, design = rchis05, family = quasibinomial)

stargazer(model1, model2, model3, type = "text")

cbind(OddsRatio = exp(model1$coef), exp(confint(model1)))
cbind(OddsRatio = exp(model2$coef), exp(confint(model2)))
cbind(OddsRatio = exp(model3$coef), exp(confint(model3)))

