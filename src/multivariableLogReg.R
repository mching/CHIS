# Table 3: Regression Analyses

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
cbind(OddsRatio = exp(mvreg.either.int$coef), exp(confint(mvreg.either.int)))

# Quadratic
# Take out some covariates
mvreg.either.quadratic <- svyglm(referred ~ pedsHiRisk +
                                   # male +
                                   srage.p +
                                   # belowpovl +
                                   # unins.ever +
                                   # racehp2p +
                                   # srh.a.i +
                                   brthwk.p.i +
                                   pedsHiRisk*srage.p +
                                   pedsHiRisk*brthwk.p.i
                                   # I(pedsHiRisk^2*srage.p) +
                                   # I(pedsHiRisk^2*brthwk.p.i)
                                   ,
                                 design = rchis05, family = quasibinomial)
summary(mvreg.either.quadratic)

# Add quadratic terms
mvreg.either.quadratic2 <- svyglm(referred ~ pedsHiRisk +
                                   # male +
                                   srage.p +
                                   # belowpovl +
                                   # unins.ever +
                                   # racehp2p +
                                   # srh.a.i +
                                   brthwk.p.i +
                                   pedsHiRisk*srage.p +
                                   pedsHiRisk*brthwk.p.i +
                                   I(pedsHiRisk^2)*srage.p +
                                   I(pedsHiRisk^2)*brthwk.p.i
                                 ,
                                 design = rchis05, family = quasibinomial)
summary(mvreg.either.quadratic2)
table(chis$srage.p)