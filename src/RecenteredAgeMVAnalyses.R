# recentering of variables to get ORs
library(survey)
library(stargazer)
library(ggplot2)

# recenter brthwk.p.i to median
svyquantile(~brthwk.p.i, design = rchis05, quant = 0.5)
chis$brthwk.p.i.recenter <- chis$brthwk.p.i - 3.35

# attach to survey design object
rchis <- svrepdesign(chis[ , -( 212 + ( 1 : 80 ))], repweights = chis[ , ( 212 + ( 1 : 80 ))], weights = chis$rakedw0, combined.weights = TRUE, scale = 1, rscales = rep(1,80), type="other")

# Create subset design object
rchis05 <- subset(rchis, srage.p < 6)

####
# Calculate OR table with median birthweight and method of Hosmer & Lemeshow
####

mvreg.dev.int <- svyglm(cf46 ~ 
                          male +
                          srage.p +
                          brthwk.p.i.recenter +
                          racehp2p +
                          srh.a.i +
                          belowpovl +
                          unins.ever +
                          pedsHiRisk +
                          pedsHiRisk*srage.p +
                          pedsHiRisk*brthwk.p.i.recenter
                        ,
                        design = rchis05, family = quasibinomial)

vcov.table <- vcov(mvreg.dev.int)
var.pedsHiRisk <-vcov.table[12,12]
var.pHR.age <-vcov.table[13,13]
cov.pHR.age <-vcov.table[13,12]

a <- 0:5
se.int.coef <- (var.pedsHiRisk + a^2 * var.pHR.age + 2 * a * cov.pHR.age)^0.5

beta.peds <- summary(mvreg.dev.int)$coefficients[12, 1]
beta.pHR.age <- summary(mvreg.dev.int)$coefficients[13, 1]

log.odds.ratio <- beta.peds + beta.pHR.age * a 
log.odds.ratio.lower <- log.odds.ratio + qnorm(0.025) * se.int.coef
log.odds.ratio.upper <- log.odds.ratio + qnorm(0.975) * se.int.coef
log.odds.ratio.table <- cbind(log.odds.ratio, log.odds.ratio.lower, log.odds.ratio.upper)
odds.ratio.table <- exp(log.odds.ratio.table)
dimnames(odds.ratio.table)[[1]] <- 0:5
dimnames(odds.ratio.table)[[2]] <- c("OR", "lower", "upper")
odds.ratio.table

###############################
###############################
# Recenter method from Jaccard
###############################
###############################

###################
# DS referred, Age
###################

modelRecenterAge.DS <- function(age = 0) {
  # Recenter age
  chis$srage.p.recenter <- chis$srage.p - age
  
  # Attach to survey design object
  rchis <- svrepdesign(chis[ , -( 212 + ( 1 : 80 ))], repweights = chis[ , ( 212 + ( 1 : 80 ))], weights = chis$rakedw0, combined.weights = TRUE, scale = 1, rscales = rep(1,80), type="other")
  
  # Create subset design object
  rchis05.recenter <- subset(rchis, srage.p < 6)
  
  # Create survey logistic regression model
  model <- svyglm(cf46 ~ 
                            male +
                            srage.p.recenter +
                            brthwk.p.i.recenter +
                            racehp2p +
                            srh.a.i +
                            belowpovl +
                            unins.ever +
                            pedsHiRisk +
                            pedsHiRisk*srage.p.recenter +
                            pedsHiRisk*brthwk.p.i.recenter
                          ,
                          design = rchis05.recenter, family = quasibinomial)
  # Return model
  model
}

a <- modelRecenterAge.DS(age = 5)
summary(a)

c(exp(a$coef[12]), exp(confint(a)[12,]))

modelList <- lapply(as.list(0:5), modelRecenterAge.DS)

OR_CI_DS <- data.frame(age = 0:5, OR = NA, lower = NA, upper = NA)
OR_CI_DS[2] <- sapply(modelList, function(x) exp(x$coef[12]))
OR_CI_DS[3] <- sapply(modelList, function(x) exp(confint(x)[12,1]))
OR_CI_DS[4] <- sapply(modelList, function(x) exp(confint(x)[12,2]))

OR_CI_DS

sePEDS <- rep(NA, 6)

for(i in 1:6) {
  sePEDS[i] <- summary(modelList[[i]])$coef[12,2]
}

sePEDS

# There is something about how confint.svyglm calculates the confidence 
# intervals that is different from the Hosmer & Lemeshow model. I would use the 
# confint.svyglm method for now but it is risky since I don't really know what
# the computer is doing to get the CI while I know exactly how I am getting the
# Hosmer/Lemeshow CIs.

###################
# SLH referred, Age
###################

modelRecenterAge.SLH <- function(age = 0) {
  # Recenter age
  chis$srage.p.recenter <- chis$srage.p - age
  
  # Attach to survey design object
  rchis <- svrepdesign(chis[ , -( 212 + ( 1 : 80 ))], repweights = chis[ , ( 212 + ( 1 : 80 ))], weights = chis$rakedw0, combined.weights = TRUE, scale = 1, rscales = rep(1,80), type="other")
  
  # Create subset design object
  rchis05.recenter <- subset(rchis, srage.p < 6)
  
  # Create survey logistic regression model
  model <- svyglm(cf47 ~ 
                    male +
                    srage.p.recenter +
                    brthwk.p.i.recenter +
                    racehp2p +
                    srh.a.i +
                    belowpovl +
                    unins.ever +
                    pedsHiRisk +
                    pedsHiRisk*srage.p.recenter +
                    pedsHiRisk*brthwk.p.i.recenter
                  ,
                  design = rchis05.recenter, family = quasibinomial)
  # Return model
  model
}

modelList <- lapply(as.list(0:5), modelRecenterAge.SLH)

OR_CI_SLH <- data.frame(age = 0:5, OR = NA, lower = NA, upper = NA)
OR_CI_SLH[2] <- sapply(modelList, function(x) exp(x$coef[12]))
OR_CI_SLH[3] <- sapply(modelList, function(x) exp(confint(x)[12,1]))
OR_CI_SLH[4] <- sapply(modelList, function(x) exp(confint(x)[12,2]))

OR_CI_SLH
#######################
# referred interaction, age
#######################

modelRecenterAge.ref <- function(age = 0) {
  # Recenter age
  chis$srage.p.recenter <- chis$srage.p - age
  
  # Attach to survey design object
  rchis <- svrepdesign(chis[ , -( 212 + ( 1 : 80 ))], repweights = chis[ , ( 212 + ( 1 : 80 ))], weights = chis$rakedw0, combined.weights = TRUE, scale = 1, rscales = rep(1,80), type="other")
  
  # Create subset design object
  rchis05.recenter <- subset(rchis, srage.p < 6)
  
  # Create survey logistic regression model
  model <- svyglm(referred ~ 
                    male +
                    srage.p.recenter +
                    brthwk.p.i.recenter +
                    racehp2p +
                    srh.a.i +
                    belowpovl +
                    unins.ever +
                    pedsHiRisk +
                    pedsHiRisk*srage.p.recenter +
                    pedsHiRisk*brthwk.p.i.recenter
                  ,
                  design = rchis05.recenter, family = quasibinomial)
  # Return model
  model
}

modelList.referred <- lapply(as.list(0:5), modelRecenterAge.ref)

OR_CI.ref <- data.frame(age = 0:5, OR = NA, lower = NA, upper = NA)
OR_CI.ref[2] <- sapply(modelList.referred, function(x) exp(x$coef[12]))
OR_CI.ref[3] <- sapply(modelList.referred, function(x) exp(confint(x)[12,1]))
OR_CI.ref[4] <- sapply(modelList.referred, function(x) exp(confint(x)[12,2]))

OR_CI.ref

######################
# Birthweight*PEDS interaction for speech (cf47)
######################

# recenter srage.p to median 2 years old
svyquantile(~srage.p, design = rchis05, quant = 0.5)
chis$srage.p.recenter <- chis$srage.p - 2

# attach to survey design object
rchis <- svrepdesign(chis[ , -( 212 + ( 1 : 80 ))], repweights = chis[ , ( 212 + ( 1 : 80 ))], weights = chis$rakedw0, combined.weights = TRUE, scale = 1, rscales = rep(1,80), type="other")

# Create subset design object
rchis05 <- subset(rchis, srage.p < 6)

modelRecenterBW.speech <- function(bw = 0) {
  # Recenter age
  chis$brthwk.p.i.recenter <- chis$brthwk.p.i - bw
  
  # Attach to survey design object
  rchis <- svrepdesign(chis[ , -( 212 + ( 1 : 80 ))], repweights = chis[ , ( 212 + ( 1 : 80 ))], weights = chis$rakedw0, combined.weights = TRUE, scale = 1, rscales = rep(1,80), type="other")
  
  # Create subset design object
  rchis05.recenter <- subset(rchis, srage.p < 6)
  
  # Create survey logistic regression model
  model <- svyglm(cf47 ~ 
                    male +
                    srage.p.recenter +
                    brthwk.p.i.recenter +
                    racehp2p +
                    srh.a.i +
                    belowpovl +
                    unins.ever +
                    pedsHiRisk +
                    pedsHiRisk*srage.p.recenter +
                    pedsHiRisk*brthwk.p.i.recenter
                  ,
                  design = rchis05.recenter, family = quasibinomial)
  # Return model
  model
}

modelList.speech <- lapply(as.list(1:5), modelRecenterBW.speech)
summary(modelList.speech[[1]])

OR_CI.speech.bw <- data.frame(BW = 1:5, OR = NA, lower = NA, upper = NA)
OR_CI.speech.bw[2] <- sapply(modelList.speech, function(x) exp(x$coef[12]))
OR_CI.speech.bw[3] <- sapply(modelList.speech, function(x) exp(confint(x)[12,1]))
OR_CI.speech.bw[4] <- sapply(modelList.speech, function(x) exp(confint(x)[12,2]))

OR_CI.speech.bw

######################
# Birthweight*PEDS interaction for DS/SLH (referred)
######################

# recenter srage.p to median 2 years old
svyquantile(~srage.p, design = rchis05, quant = 0.5)
chis$srage.p.recenter <- chis$srage.p - 2

# attach to survey design object
rchis <- svrepdesign(chis[ , -( 212 + ( 1 : 80 ))], repweights = chis[ , ( 212 + ( 1 : 80 ))], weights = chis$rakedw0, combined.weights = TRUE, scale = 1, rscales = rep(1,80), type="other")

# Create subset design object
rchis05 <- subset(rchis, srage.p < 6)

modelRecenterBW.ref <- function(bw = 0) {
  # Recenter age
  chis$brthwk.p.i.recenter <- chis$brthwk.p.i - bw
  
  # Attach to survey design object
  rchis <- svrepdesign(chis[ , -( 212 + ( 1 : 80 ))], repweights = chis[ , ( 212 + ( 1 : 80 ))], weights = chis$rakedw0, combined.weights = TRUE, scale = 1, rscales = rep(1,80), type="other")
  
  # Create subset design object
  rchis05.recenter <- subset(rchis, srage.p < 6)
  
  # Create survey logistic regression model
  model <- svyglm(referred ~ 
                    male +
                    srage.p.recenter +
                    brthwk.p.i.recenter +
                    racehp2p +
                    srh.a.i +
                    belowpovl +
                    unins.ever +
                    pedsHiRisk +
                    pedsHiRisk*srage.p.recenter +
                    pedsHiRisk*brthwk.p.i.recenter
                  ,
                  design = rchis05.recenter, family = quasibinomial)
  # Return model
  model
}

modelList.ref.bw <- lapply(as.list(1:5), modelRecenterBW.ref)
summary(modelList.ref.bw[[3]])

OR_CI.ref.bw <- data.frame(BW = 1:5, OR = NA, lower = NA, upper = NA)
OR_CI.ref.bw[2] <- sapply(modelList.ref.bw, function(x) exp(x$coef[12]))
OR_CI.ref.bw[3] <- sapply(modelList.ref.bw, function(x) exp(confint(x)[12,1]))
OR_CI.ref.bw[4] <- sapply(modelList.ref.bw, function(x) exp(confint(x)[12,2]))

OR_CI.ref.bw
