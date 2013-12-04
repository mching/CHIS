# Practice calculating OR for play with interactions by recentering continuous variable Age

model <- svyglm(referred ~ 
                             srage.p +
                             pedsHiRisk +
                             pedsHiRisk*srage.p 
                           ,
                           design = rchis05, family = quasibinomial)
summary(model)
cbind(OR = exp(model$coefficients), exp(confint(model)))

vcov.table<- vcov(model)
var.pedsHiRisk <-vcov.table[3,3]
var.pHR.age <-vcov.table[2,2]
cov.pHR.age <-vcov.table[3,2]

a <- 0:5
se.int.coef <- (var.pedsHiRisk + a^2 * var.pHR.age + 2 * a * cov.pHR.age)^0.5

beta.peds <- summary(model)$coefficients[3, 1]
beta.pHR.age <- summary(model)$coefficients[4, 1]

log.odds.ratio <- beta.peds + beta.pHR.age * a 
log.odds.ratio.lower <- log.odds.ratio + qnorm(0.025) * se.int.coef
log.odds.ratio.upper <- log.odds.ratio + qnorm(0.975) * se.int.coef
log.odds.ratio.table <- cbind(log.odds.ratio, log.odds.ratio.lower, log.odds.ratio.upper)
odds.ratio.table <- exp(log.odds.ratio.table)
dimnames(odds.ratio.table)[[1]] <- 0:5
dimnames(odds.ratio.table)[[2]] <- c("OR", "lower", "upper")
odds.ratio.table

model <- svyglm(referred ~ 
                  I(srage.p - 1) +
                  pedsHiRisk +
                  pedsHiRisk*I(srage.p - 1) 
                ,
                design = rchis05, family = quasibinomial)
summary(model)
cbind(OR = exp(model$coefficients), exp(confint(model)))

# This result is not quite the same because of the confidence intervals. Try again with brand new recentered age variable.

chis$age1 <- chis$srage.p - 1
rchis <- svrepdesign(chis[ , -( 212 + ( 1 : 80 ))], repweights = chis[ , ( 212 + ( 1 : 80 ))], weights = chis$rakedw0, combined.weights = TRUE, scale = 1, rscales = rep(1,80), type="other")
summary(rchis)

# Create subset design object
rchis05 <- subset(rchis, srage.p < 6)

model <- svyglm(referred ~ 
                  age1 +
                  pedsHiRisk +
                  pedsHiRisk*age1
                ,
                design = rchis05, family = quasibinomial)
summary(model)
cbind(OR = exp(model$coefficients), exp(confint(model)))
