# Table 3: Regression Analyses

# Logistic regression of developmental specialist referral only
model2 <- svyglm(cf46 ~ pedsHiRisk 
                 + male 
                 + srage.p 
                 + belowpovl 
                 + unins.ever 
                 + racehp2p 
                 + srh.a.i
                 + brthwk.p.i
                 ,
                 design = rchis05, family = quasibinomial)
summary(model2)
cbind(OddsRatio = exp(model2$coef), exp(confint(model2)))


# Logistic regression of speech-language-hearing referral only
model3 <- svyglm(cf47 ~ pedsHiRisk 
                 + male 
                 #                 + cd1.2 
                 + srage.p 
                 + belowpovl 
                 + unins.ever 
                 + racehp2p 
                 # + white
                 + srh.a.i
                 + brthwk.p.i
                 ,
                 design = rchis05, family = quasibinomial)
summary(model3)
cbind(OddsRatio = exp(model3$coef), exp(confint(model3)))


# Logistic regression of referral either to developmental specialist or speech-language-hearing
model1 <- svyglm(referred ~ pedsHiRisk
                 # + peds
                 + male 
                 #                + cd1.2 
                 + srage.p 
                 + belowpovl 
                 + unins.ever 
                 + racehp2p 
                 #               + white
                 + srh.a.i
                 + brthwk.p.i
                 ,
                 design = rchis05, family = quasibinomial)
summary(model1)
cbind(OddsRatio = exp(model1$coef), exp(confint(model1)))
