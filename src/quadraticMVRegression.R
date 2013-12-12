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
