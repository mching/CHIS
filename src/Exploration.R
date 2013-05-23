# This gets the same output as in AskCHIS but inverted layout of table
svytotal(~peds, rchis, na.rm = T)
pedsBySex <- svyby(~peds, ~male, svymean, design = rchis, na.rm = T)
Table <- svytable(~peds + male, design = rchis)
summary(Table)

ftable(pedsBySex)
confint(pedsBySex)

# Is there an interaction between peds risk level and gender? No.
model0 <- svyglm(referred ~ pedsHiRisk 
                + male 
                + pedsHiRisk*male,
                design = rchis, family = quasibinomial)
summary(model0)


# GLM with univariates from univariates.R
model <- svyglm(referred ~ pedsHiRisk 
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
                  design = rchis, family = quasibinomial)
summary(model)
cbind(OddsRatio = exp(model$coef), exp(confint(model)))


# GLM with univariates from univariates_referral_devo.R
model2 <- svyglm(cf46 ~ pedsHiRisk 
                + male 
#                + cd1.2 
                + srage.p 
                + belowpovl 
                + unins.ever 
                + racehp2p 
               # + white
                + srh.a.i
                + brthwk.p.i
                ,
                design = rchis, family = quasibinomial)
summary(model2)
cbind(OddsRatio = exp(model2$coef), exp(confint(model2)))

# GLM with univariates from univariates_referral_speech-only.R
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
                 design = rchis, family = quasibinomial)
summary(model3)
cbind(OddsRatio = exp(model3$coef), exp(confint(model3)))

#################################################################
# Subgroup analysis on pedsHiRisk yes and Referred vs not Referred
#################################################################

# Create a subgroup using subset on rchis
pedsHiPop <- subset(rchis, pedsHiRisk == "Yes")

# Sample subset tabulation
svytotal(~male, pedsHiPop)

# The other method from Lumley suggests using svyby, giving same result
svyby(~male, ~pedsHiRisk, svytotal, design = rchis)

# Very different results obtained from the original 
svytable(~male+pedsHiRisk, rchis)


# Referred to either devo or speech

model4 <- svyglm(referred ~ # pedsHiRisk 
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
                design = pedsHiPop, family = quasibinomial)
summary(model4)
cbind(OddsRatio = exp(model4$coef), exp(confint(model4)))

# GLM with univariates from univariates_referral_devo.R
model5 <- svyglm(cf46 ~ 
                 + male 
                 #                + cd1.2 
                 + srage.p 
                 + belowpovl 
                 + unins.ever 
                 + racehp2p 
                 # + white
                 + srh.a.i
                 + brthwk.p.i
                 ,
                 design = pedsHiPop, family = quasibinomial)
summary(model5)
cbind(OddsRatio = exp(model2$coef), exp(confint(model2)))

# GLM with univariates from univariates_referral_speech-only.R
model6 <- svyglm(cf47 ~ 
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
                 design = pedsHiPop, family = quasibinomial)
summary(model6)
cbind(OddsRatio = exp(model6$coef), exp(confint(model6)))

