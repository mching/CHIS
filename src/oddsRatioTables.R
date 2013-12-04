# Calculate the odds ratios of PEDS high risk vs no-moderate risk at
# different ages given significant interaction term

vcov.either.int <- vcov(mvreg.either.int)
var.pedsHiRisk <- vcov.either.int[2,2]
var.pHR.age <- vcov.either.int[13,13]
cov.pHR.age <- vcov.either.int[13,2]

a <- 0:5
se.int.coef <- (var.pedsHiRisk + a^2 * var.pHR.age + 2 * a * cov.pHR.age)^0.5

beta.peds <- summary(mvreg.either.int)$coefficients[2, 1]
beta.pHR.age <- summary(mvreg.either.int)$coefficients[13, 1]

log.odds.ratio <- beta.peds + beta.pHR.age * a 
log.odds.ratio.lower <- log.odds.ratio + qnorm(0.025) * se.int.coef
log.odds.ratio.upper <- log.odds.ratio + qnorm(0.975) * se.int.coef
log.odds.ratio.table <- cbind(log.odds.ratio, log.odds.ratio.lower, log.odds.ratio.upper)
odds.ratio.table <- exp(log.odds.ratio.table)
dimnames(odds.ratio.table)[[1]] <- 0:5
dimnames(odds.ratio.table)[[2]] <- c("OR", "lower", "upper")
odds.ratio.table

plot(odds.ratio.table[, 1])