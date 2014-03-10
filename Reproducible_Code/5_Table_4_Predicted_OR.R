####################################################################################
# This file contains code to generate the predicted odds ratios for Table 4
# 
# 
# Michael Ching, MD, MPH
#
#
# note: cf46 is Developmental Specialist referral
# cf47 is Speech, language, hearing referral
# referred is the combined DS or SLH referral
####################################################################################

library(survey)
library(stargazer)
library(ggplot2)

# recenter brthwk.p.i to median
median.birthweight <- svyquantile(~brthwk.p.i, design = rchis05, quant = 0.5)
chis$brthwk.p.i.recenter <- chis$brthwk.p.i - median.birthweight
rm(median.birthweight)

# attach to survey design object
rchis <- svrepdesign(chis[ , -( 212 + ( 1 : 80 ))], repweights = chis[ , ( 212 + ( 1 : 80 ))], weights = chis$rakedw0, combined.weights = TRUE, scale = 1, rscales = rep(1,80), type="other")

# Create subset design object
rchis05 <- subset(rchis, srage.p < 6)

###################
# DS referred, Age
###################

# Create a helper function that recenters age at a given argument (default is age = 0) and returns the logistic regression model

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

# For example, this is how it works for age = 5 (recentered age is 0, birthweight recentered at median). This allows evaluation of 
# the model at the recentered age of 0, which makes the interaction terms 0.

a <- modelRecenterAge.DS(age = 5)
summary(a)
c(exp(a$coef[12]), exp(confint(a)[12,]))

# Perform recentered analysis at each age
modelList <- lapply(as.list(0:5), modelRecenterAge.DS)

## Extract the odds ratios
# Create a data frame to hold the results
OR_CI_DS <- data.frame(age = 0:5, OR = NA, lower = NA, upper = NA)

# OR estimates
OR_CI_DS[2] <- sapply(modelList, function(x) exp(x$coef[12]))

# Lower CI
OR_CI_DS[3] <- sapply(modelList, function(x) exp(confint(x)[12,1]))

# Upper CI
OR_CI_DS[4] <- sapply(modelList, function(x) exp(confint(x)[12,2]))

OR_CI_DS

###################
# SLH referred, Age
###################

# Helper function to recenter age for SLH only referrals (cf47).

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

# Run this function on each age group and return the model parameters
modelList <- lapply(as.list(0:5), modelRecenterAge.SLH)

## Calculate OR and confidence intervals
OR_CI_SLH <- data.frame(age = 0:5, OR = NA, lower = NA, upper = NA)
OR_CI_SLH[2] <- sapply(modelList, function(x) exp(x$coef[12]))
OR_CI_SLH[3] <- sapply(modelList, function(x) exp(confint(x)[12,1]))
OR_CI_SLH[4] <- sapply(modelList, function(x) exp(confint(x)[12,2]))

OR_CI_SLH

#######################
# referred interaction, age
#######################

# Helper function for recentering age for combined outcome (cf46 or cf47)

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

# Apply function to recenter at each age and calculate parameter estimates
modelList.referred <- lapply(as.list(0:5), modelRecenterAge.ref)

# Calculate Odds ratios and confidence intervals for combined outcome
OR_CI.ref <- data.frame(age = 0:5, OR = NA, lower = NA, upper = NA)
OR_CI.ref[2] <- sapply(modelList.referred, function(x) exp(x$coef[12]))
OR_CI.ref[3] <- sapply(modelList.referred, function(x) exp(confint(x)[12,1]))
OR_CI.ref[4] <- sapply(modelList.referred, function(x) exp(confint(x)[12,2]))

OR_CI.ref

######################
# Birthweight*PEDS interaction for dev (cf46)
######################

# recenter srage.p to median 2 years old
svyquantile(~srage.p, design = rchis05, quant = 0.5)
chis$srage.p.recenter <- chis$srage.p - 2

# attach to survey design object
rchis <- svrepdesign(chis[ , -( 212 + ( 1 : 80 ))], repweights = chis[ , ( 212 + ( 1 : 80 ))], weights = chis$rakedw0, combined.weights = TRUE, scale = 1, rscales = rep(1,80), type="other")

# Create subset design object
rchis05 <- subset(rchis, srage.p < 6)

# Helper function to recenter birthweight for DS outcome

modelRecenterBW.DS <- function(bw = 0) {
  # Recenter birthweight
  chis$brthwk.p.i.recenter <- chis$brthwk.p.i - bw
  
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

modelList.DS <- lapply(as.list(1:5), modelRecenterBW.DS)
summary(modelList.speech[[1]])

OR_CI.DS.bw <- data.frame(BW = 1:5, OR = NA, lower = NA, upper = NA)
OR_CI.DS.bw[2] <- sapply(modelList.DS, function(x) exp(x$coef[12]))
OR_CI.DS.bw[3] <- sapply(modelList.DS, function(x) exp(confint(x)[12,1]))
OR_CI.DS.bw[4] <- sapply(modelList.DS, function(x) exp(confint(x)[12,2]))

OR_CI.DS.bw

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

# Helper function

modelRecenterBW.speech <- function(bw = 0) {
  # Recenter birthweight
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

# Helper function

modelRecenterBW.ref <- function(bw = 0) {
  # Recenter birthweight
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
