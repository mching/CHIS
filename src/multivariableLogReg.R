# Table 3: Regression Analyses
library(survey)
library(ggplot2)
# Logistic regression of developmental specialist referral only
mvreg.dev <- svyglm(cf46 ~ pedsHiRisk 
                 + male 
                 + srage.p 
                 + belowpovl 
                 + unins.ever 
                 + racehp2p 
                 + srh.a.i
                 + brthwk.p.i,
                 design = rchis05, family = quasibinomial)
summary(mvreg.dev)
cbind(OddsRatio = exp(mvreg.dev$coef), exp(confint(mvreg.dev)))


# Logistic regression of speech-language-hearing referral only
mvreg.speech <- svyglm(cf47 ~ pedsHiRisk 
                 + male 
                 + srage.p 
                 + belowpovl 
                 + unins.ever 
                 + racehp2p 
                 + srh.a.i
                 + brthwk.p.i,
                 design = rchis05, family = quasibinomial)
summary(mvreg.speech)
cbind(OddsRatio = exp(mvreg.speech$coef), exp(confint(mvreg.speech)))


# Logistic regression of referral either to developmental specialist or speech-language-hearing
mvreg.either <- svyglm(referred ~ pedsHiRisk
                 + male 
                 + srage.p 
                 + belowpovl 
                 + unins.ever 
                 + racehp2p 
                 + srh.a.i
                 + brthwk.p.i,
                 design = rchis05, family = quasibinomial)
summary(mvreg.either)
cbind(OddsRatio = exp(mvreg.either$coef), exp(confint(mvreg.either)))

#### Interactions

# Developmental Specialist Only
mvreg.dev.int <- svyglm(cf46 ~ pedsHiRisk 
                    + male 
                    + srage.p 
                    + belowpovl 
                    + unins.ever 
                    + racehp2p 
                    + srh.a.i
                    + brthwk.p.i
                    + pedsHiRisk*srage.p
                    + pedsHiRisk*brthwk.p.i,
                    design = rchis05, family = quasibinomial)
summary(mvreg.dev.int)
cbind(OddsRatio = exp(mvreg.dev.int$coef), exp(confint(mvreg.dev.int)))

# Speech-language-hearing only
mvreg.speech.int <- svyglm(cf47 ~ pedsHiRisk 
                    + male 
                    + srage.p 
                    + belowpovl 
                    + unins.ever 
                    + racehp2p 
                    + srh.a.i
                    + brthwk.p.i
                    + pedsHiRisk*srage.p
                    + pedsHiRisk*brthwk.p.i,
                    design = rchis05, family = quasibinomial)
summary(mvreg.speech.int)
cbind(OddsRatio = exp(mvreg.speech.int$coef), exp(confint(mvreg.speech.int)))

# Either referral
mvreg.either.int <- svyglm(referred ~ pedsHiRisk
                    + male 
                    + srage.p 
                    + belowpovl 
                    + unins.ever 
                    + racehp2p 
                    + srh.a.i
                    + brthwk.p.i
                    + pedsHiRisk*srage.p
                    + pedsHiRisk*brthwk.p.i,
                    design = rchis05, family = quasibinomial)
summary(mvreg.either.int)

# Odds ratios at different ages

regTermTest(mvreg.either.int, test.terms = ~ racehp2p + srh.a.i)


# Quadratic
# Take out some covariates to simplify model. Only interaction
mvreg.either.0 <- svyglm(referred ~ pedsHiRisk +
                                   # male +
                                   srage.p +
                                   # belowpovl +
                                   # unins.ever +
                                   # racehp2p +
                                   # srh.a.i +
                                   # brthwk.p.i +
                                   pedsHiRisk*srage.p 
                                   # pedsHiRisk*brthwk.p.i +
                                   # I((srage.p)^2) 
                                   # I((brthwk.p.i)^2)
                                 ,
                                 design = rchis05, family = quasibinomial)
summary(mvreg.either.0)

# Quadratic age interaction
mvreg.either.1 <- svyglm(referred ~ pedsHiRisk +
                           # male +
                           srage.p +
                           # belowpovl +
                           # unins.ever +
                           # racehp2p +
                           # srh.a.i +
                           # brthwk.p.i +
                           pedsHiRisk*srage.p +
                         # pedsHiRisk*brthwk.p.i +
                           I((srage.p)^2)
                         # I((brthwk.p.i)^2)
                         ,
                         design = rchis05, family = quasibinomial)
summary(mvreg.either.1)

regTermTest(mvreg.either.quadratic, test.terms = ~ pedsHiRisk * srage.p)
