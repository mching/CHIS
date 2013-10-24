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

# 
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