# Table 3: Regression Analyses
library(survey)
library(ggplot2)
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

#### Interactions

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
cbind(OddsRatio = exp(mvreg.speech.int$coef), exp(confint(mvreg.speech.int)))

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

# Using AIC to compare models does not work because svrepglm is not fitted by
# maximum likelihood. See:
# https://stat.ethz.ch/pipermail/r-help/2012-July/319508.html

regTermTest(model2int, test.terms = ~ pedsHiRisk * srage.p 
            + pedsHiRisk * brthwk.p.i 
            #            + pedsHiRisk * unins.ever
)

regTermTest(model3int, test.terms = ~ pedsHiRisk * srage.p 
            + pedsHiRisk * brthwk.p.i 
            #            + pedsHiRisk * unins.ever
)

regTermTest(model1int, test.terms = ~ pedsHiRisk * srage.p 
            + pedsHiRisk * brthwk.p.i 
            #            + pedsHiRisk * unins.ever
)

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
