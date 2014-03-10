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


############
# Section 1: Multivariable Logistic Regression without interaction terms
############

# Logistic regression of developmental specialist referral only
mvreg.dev <- svyglm(cf46 ~ 
                      male +
                      srage.p +
                      brthwk.p.i +
                      racehp2p +
                      srh.a.i +
                      belowpovl +
                      unins.ever +
                      pedsHiRisk
                    ,
                    design = rchis05, family = quasibinomial)
summary(mvreg.dev)
cbind(OddsRatio = exp(mvreg.dev$coef), exp(confint(mvreg.dev)))


# Logistic regression of speech-language-hearing referral only
mvreg.speech <- svyglm(cf47 ~ 
                         male +
                         srage.p +
                         brthwk.p.i +
                         racehp2p +
                         srh.a.i +
                         belowpovl +
                         unins.ever +
                         pedsHiRisk
                       ,
                       design = rchis05, family = quasibinomial)
summary(mvreg.speech)
cbind(OddsRatio = exp(mvreg.speech$coef), exp(confint(mvreg.speech)))


# Logistic regression of referral either to developmental specialist or speech-language-hearing
mvreg.either <- svyglm(referred ~ 
                         male +
                         srage.p +
                         brthwk.p.i +
                         racehp2p +
                         srh.a.i +
                         belowpovl +
                         unins.ever +
                         pedsHiRisk
                       ,
                       design = rchis05, family = quasibinomial)
summary(mvreg.either)
cbind(OddsRatio = exp(mvreg.either$coef), exp(confint(mvreg.either)))

##################
# Generate pretty tables using stargazer
##################
# DS Table with ORs and CIs

OR.vector <- exp(mvreg.dev$coef)
CI.vector <- exp(confint(mvreg.dev))
p.values <- summary(mvreg.dev)$coefficients[, 4]

stargazer(mvreg.dev, coef = list(OR.vector), ci = T, 
          ci.custom = list(CI.vector), p = list(p.values), 
          single.row = F, type = "text")

# Speech Table with ORs and CIs
OR.vector <- exp(mvreg.speech$coef)
CI.vector <- exp(confint(mvreg.speech))
p.values <- summary(mvreg.speech)$coefficients[, 4]

stargazer(mvreg.speech, coef = list(OR.vector), ci = T, 
          ci.custom = list(CI.vector), p = list(p.values), 
          single.row = F, type = "text")

# Either Table with ORs and CIs
OR.vector <- exp(mvreg.either$coef)
CI.vector <- exp(confint(mvreg.either))
p.values <- summary(mvreg.either)$coefficients[, 4]

stargazer(mvreg.either, coef = list(OR.vector), ci = T, 
          ci.custom = list(CI.vector), p = list(p.values), 
          single.row = F, type = "text")

############
# Section 2: Multivariable Logistic Regression WITH interaction terms (Table 3)
############

# Developmental Specialist Only
mvreg.dev.int <- svyglm(cf46 ~ 
                          male +
                          srage.p +
                          brthwk.p.i +
                          racehp2p +
                          srh.a.i +
                          belowpovl +
                          unins.ever +
                          pedsHiRisk +
                          pedsHiRisk*srage.p +
                          pedsHiRisk*brthwk.p.i
                        ,
                        design = rchis05, family = quasibinomial)
summary(mvreg.dev.int)

# Speech-language-hearing only
mvreg.speech.int <- svyglm(cf47 ~  
                             male +
                             srage.p +
                             brthwk.p.i +
                             racehp2p +
                             srh.a.i +
                             belowpovl +
                             unins.ever +
                             pedsHiRisk +
                             pedsHiRisk*srage.p +
                             pedsHiRisk*brthwk.p.i
                           ,
                           design = rchis05, family = quasibinomial)
summary(mvreg.speech.int)


# Either referral
mvreg.either.int <- svyglm(referred ~ 
                             male +
                             srage.p +
                             brthwk.p.i +
                             racehp2p +
                             srh.a.i +
                             belowpovl +
                             unins.ever +
                             pedsHiRisk +
                             pedsHiRisk*srage.p +
                             pedsHiRisk*brthwk.p.i
                           ,
                           design = rchis05, family = quasibinomial)
summary(mvreg.either.int)


##################
# Generate pretty tables using stargazer
##################
# Only interaction models
stargazer(mvreg.dev.int, mvreg.speech.int, mvreg.either.int, type = "text")

# All models
stargazer(mvreg.dev, mvreg.dev.int, mvreg.speech, mvreg.speech.int, mvreg.either, mvreg.either.int, 
          type = "text",
          title = "Table: Multivariable Logistic Regression Models",
          # out = "./Tables/Table3.txt",
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