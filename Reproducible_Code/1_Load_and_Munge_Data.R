# Reproducible Code for paper
# Michael Ching, MD, MPH

# Dataset is the 2009 Public Use File of the CHIS, available from www.chis.ucla.edu
# Read in data using usual read.dta or similar method

setwd("~/Dropbox/Mike/CHIS")

library(ProjectTemplate)
library(Hmisc)
library(survey)
library(binom)
library(ggplot2)
library(scales)
library(stargazer)

samplesize <- length(chis[,1])

############
# Generate a pedsHiRisk variable
# No need to impute peds Inapplicables, this is done by Westat
############

chis$peds[chis$peds == "INAPPLICABLE"] <- NA
chis$peds <- factor(chis$peds)  ## get rid of INAPPLICABLE category since there are 0 left
peds.sample.size <- sum(table(chis$peds))


# Generate pedsHiRisk variable that corresponds to high risk on the PEDS
pedsHiRisk <- rep(NA, samplesize)

pedsHiRisk[chis$peds == "HIGH RISK"] <- 1
pedsHiRisk[chis$peds != "HIGH RISK"] <- 0
table(pedsHiRisk, useNA = "ifany")

pedsHiRisk <- factor(pedsHiRisk, levels = c(0, 1), labels = c("No", "Yes"))
chis$pedsHiRisk <- pedsHiRisk  ## attach to the chis dataframe
rm(pedsHiRisk)

############
# Generate a pedsModHiRisk variable
# No need to impute peds Inapplicables, this is done by Westat
############

# Generate pedsModHiRisk variable that corresponds to high risk on the PEDS
pedsModHiRisk <- rep(NA, samplesize)

pedsModHiRisk[chis$peds == "HIGH RISK"] <- 1
pedsModHiRisk[chis$peds == "MODERATE RISK"] <- 1
pedsModHiRisk[chis$peds != "HIGH RISK" & chis$peds != "MODERATE RISK"] <- 0
table(pedsModHiRisk, useNA = "ifany")

pedsModHiRisk <- factor(pedsModHiRisk, levels = c(0, 1), labels = c("No-Low Risk", "Mod-High Risk"))
chis$pedsModHiRisk <- pedsModHiRisk  ## attach to the chis dataframe
rm(pedsModHiRisk)

###############
# cf46 Referral to developmental specialist
###############

devo.refer <- chis$cf46
table(devo.refer)
devo.refer[devo.refer == "INAPPLICABLE"] <- NA
devo.refer <- factor(devo.refer)
devo.refer <- factor(as.numeric(devo.refer), levels = 2:1, labels = c("Not referred", "Referred"))
chis$cf46 <- devo.refer
rm(devo.refer)

devo.referred.sample.size <- sum(table(chis$cf46))

###############
# Generate male variable
###############

male <- 2 - (as.integer(chis$srsex))
male <- factor(male, levels = c(0, 1), labels = c("Female", "Male"))
chis$male <- male
rm(male)

###############
# Recode usual source of care variable
###############

chis$cd1 <- factor(chis$cd1)
cd1.2 <- rep(NA, samplesize)
cd1.2[chis$cd1 == "NO"] <- 0
cd1.2[chis$cd1 != "NO"] <- 1
cd1.2 <- factor(cd1.2, levels = c(0, 1), labels = c("No Usual Source", "Has Usual Source"))
chis$cd1.2 <- cd1.2
rm(cd1.2)

###############
# Recode referral to speech, hearing, etc.
###############

speech.refer <- chis$cf47
speech.refer[speech.refer == "INAPPLICABLE"] <- NA
speech.refer <- factor(speech.refer)
speech.refer <- factor(as.numeric(speech.refer), levels = 2:1, labels = c("Not referred", "Referred"))
chis$cf47 <- speech.refer
speech.sample.size <- sum(table(chis$cf47))
rm(speech.refer)

###############
# Generate "referred" variable for any referral for development
###############

referred <- rep(NA, samplesize)
referred[chis$cf47 == "Referred" | chis$cf46 == "Referred"] <- 1
referred[chis$cf47 == "Not referred" & chis$cf46 == "Not referred"] <- 0
referred <- factor(referred, levels = c(0, 1), labels = c("No", "Yes"))
chis$referred <- referred 
rm(referred)

study.sample.size <- sum(table(chis$referred, chis$pedsHiRisk))

###############
# Generate race variable
###############

# Lots of choices are available in CHIS. We used the UCLA CHPR definition.

# UCLA CHPR definition
chis$racehp2p <- factor(chis$racehp2p)
racehp2p <- chis$racehp2p
racehp2p[chis$racehp2p == "AIAN"] <- "PI/OTHER SINGLE/MULTIPLE RACE"
chis$racehp2p <- racehp2p
chis$racehp2p <- factor(chis$racehp2p)
chis$racehp2p <- relevel(chis$racehp2p, ref = 4)
rm(racehp2p)

# Also can make a white vs. non-white variable
white <- rep(NA, samplesize)
white[chis$racehp2p == "WHITE"] <- 1
white[chis$racehp2p != "WHITE"] <- 0
white <- factor(white, levels = c(0, 1), labels = c("Not White", "White"))
chis$white <- white
rm(white)

###############
# Generate ethnicity (Hispanic) variable
###############

# table(chis$srh.a)
hispanic <- chis$srh.a
hispanic <- factor(hispanic)
table(hispanic, useNA = "if")

# Need to impute missing data here since 11 were not collected
hispanic[hispanic == levels(hispanic)[1]] <- NA
hispanic <- factor(hispanic)
hispanic <- impute(hispanic)
chis$srh.a.i <- hispanic
rm(hispanic)

###############
# Generate poverty variable
###############

chis$povll <- factor(chis$povll)
belowpovl <- rep(NA, samplesize)
belowpovl[as.numeric(chis$povll) == 1] <- 1
belowpovl[as.numeric(chis$povll) > 1] <- 0
belowpovl <- factor(belowpovl, levels = 0:1, labels = c("100% FPL or higher", "Below 100% FPL"))
chis$belowpovl <- belowpovl
rm(belowpovl)

###############
# Clean birthweight variable
###############

# 179 individuals have -9 value
chis$brthwk.p[chis$brthwk.p == -9] <- NA

# IMPUTE birthweight by median
brthwk.p.impute <- impute(chis$brthwk.p)
chis$brthwk.p.i <- brthwk.p.impute
rm(brthwk.p.impute)

###############
# Clean uninsured variable
# uninsany
###############

# table(chis$uninsany)
chis$uninsany <- factor(chis$uninsany) # get rid of empty categories
# table(chis$uninsany)
# Collapse all and part into one category
unins.ever <- rep(NA, samplesize)
unins.ever[chis$uninsany == levels(chis$uninsany)[1] | chis$uninsany == levels(chis$uninsany)[2]] <- 1
unins.ever[chis$uninsany == levels(chis$uninsany)[3]] <- 0
unins.ever <- factor(unins.ever, levels = c(0,1), labels = c("Never uninsured", "Some or All Year uninsured"))
chis$unins.ever <- unins.ever
rm(unins.ever)


###############
# survey specification of design
###############
rchis <- svrepdesign(chis[ , -( 212 + ( 1 : 80 ))], repweights = chis[ , ( 212 + ( 1 : 80 ))], weights = chis$rakedw0, combined.weights = TRUE, scale = 1, rscales = rep(1,80), type="other")
summary(rchis)

# Create subset design object to ensure proper standard errors
rchis05 <- subset(rchis, srage.p < 6)
