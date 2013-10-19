# Munge for building variables

library(Hmisc)
library(survey)

# Try not dropping kids then subsetting after
# Drop all kids older than 5 
# chis <- chis[chis$srage.p < 6, ]

# chis <- chis[chis$srage.p > 0, ] ## Keep this line if you want to drop 0 yos
samplesize <- length(chis[,1])

############
# Generate a pedsHiRisk variable
# No need to impute peds Inapplicables, this is done by Westat
# Inapplicables
############

# table(chis$peds, useNA = "ifany")
chis$peds[chis$peds == "INAPPLICABLE"] <- NA
chis$peds <- factor(chis$peds)  ## get rid of INAPPLICABLE category since there are 0 left
peds.sample.size <- sum(table(chis$peds))

# Some commands commented out for testing out imputed data
# table(chis$peds)
# table(peds.i)
# table(chis$peds, peds.i, useNA = "ifany")
# chis$peds.i <- peds.i  ## attach peds.i to the chis dataset
# rm(peds.i)

# Generate pedsHiRisk variable that corresponds to high risk on the PEDS
table(chis$peds, useNA = "ifany")
pedsHiRisk <- rep(NA, samplesize)

pedsHiRisk[chis$peds == "HIGH RISK"] <- 1
pedsHiRisk[chis$peds != "HIGH RISK"] <- 0
table(pedsHiRisk, useNA = "ifany")

pedsHiRisk <- factor(pedsHiRisk, levels = c(0, 1), labels = c("No", "Yes"))
chis$pedsHiRisk <- pedsHiRisk  ## attach to the chis dataframe
rm(pedsHiRisk)
# svytotal(~pedsHiRisk, rchis, na.rm = T)
# svytotal(~peds, design=rchis, na.rm=T)

############
# Generate a pedsModHiRisk variable
# No need to impute peds Inapplicables, this is done by Westat
# Inapplicables
############

table(chis$peds, useNA = "ifany")

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

# table(chis$cf46)

devo.refer <- chis$cf46
table(devo.refer)
devo.refer[devo.refer == "INAPPLICABLE"] <- NA
devo.refer <- factor(devo.refer)
devo.refer <- factor(as.numeric(devo.refer), levels = 2:1, labels = c("Not referred", "Referred"))
chis$cf46 <- devo.refer
rm(devo.refer)

# table(chis$cf46, useNA = "ifany")
devo.referred.sample.size <- sum(table(chis$cf46))
# x <- table(chis$cf46,chis$peds, useNA="ifany")
# prop.table(x, 2)
###############
# Generate male variable
###############

# table(as.integer(chis$srsex))
male <- 2 - (as.integer(chis$srsex))
# table(chis$male)
male <- factor(male, levels = c(0, 1), labels = c("Female", "Male"))
chis$male <- male
rm(male)

###############
# Recode usual source of care variable
###############
table(chis$cd1)
sum(table(chis$cd1))
chis$cd1 <- factor(chis$cd1)
cd1.2 <- rep(NA, samplesize)
cd1.2[chis$cd1 == "NO"] <- 0
cd1.2[chis$cd1 != "NO"] <- 1
table(cd1.2, useNA = "ifany")
cd1.2 <- factor(cd1.2, levels = c(0, 1), labels = c("No Usual Source", "Has Usual Source"))
chis$cd1.2 <- cd1.2
table(chis$cd1.2)
rm(cd1.2)
# svyby(~pedsHiRisk, ~cd1.2, svymean, design = rchis)

###############
# Recode referral to speech, hearing, etc.
###############

# table(chis$cf47)
speech.refer <- chis$cf47
speech.refer[speech.refer == "INAPPLICABLE"] <- NA
speech.refer <- factor(speech.refer)
table(speech.refer)
speech.refer <- factor(as.numeric(speech.refer), levels = 2:1, labels = c("Not referred", "Referred"))
chis$cf47 <- speech.refer
speech.sample.size <- sum(table(chis$cf47))

# chis$cf47 <- factor(as.numeric(chis$cf47), levels = 2:1, labels = c("Not referred", "Referred"))
# with(chis, table(cf46, cf47, useNA = "ifany"))
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
# table(chis$referred)
# sum(table(chis$referred))

###############
# Generate maternal education variable
# There is no maternal education available, only MKA (most knowledgeable
# adult) or the Adult Respondent (aheduc)
###############

# cheduca vs. aheduc
# with(chis, table(cheduca))
# with(chis, table(aheduc))
# with(chis, table(cheduca, cheduca, useNA = "ifany"))

# sum(with(chis, table(cheduca, cheduca, useNA = "ifany")))
# sum(with(chis, table(aheduc)))
# sum(with(chis, table(cheduca)))
# It turns out that they are kind of the same but not quite overlapping
# Need to look for how others handle maternal education

###############
# Generate race variable
###############

# Lots of choices

# Census 2000 definition
# table(chis$racecn.p)

# UCLA CHPR definition
chis$racehp2p <- factor(chis$racehp2p)
racehp2p <- chis$racehp2p
racehp2p[chis$racehp2p == "AIAN"] <- "PI/OTHER SINGLE/MULTIPLE RACE"
chis$racehp2p <- racehp2p
chis$racehp2p <- factor(chis$racehp2p)
levels(chis$racehp2p)
chis$racehp2p <- relevel(chis$racehp2p, ref = 4)
table(chis$racehp2p)
rm(racehp2p)

# Also can make a white vs. non-white variable
white <- rep(NA, samplesize)
white[chis$racehp2p == "WHITE"] <- 1
white[chis$racehp2p != "WHITE"] <- 0
table(white)
white <- factor(white, levels = c(0, 1), labels = c("Not White", "White"))
chis$white <- white
table(chis$white)
rm(white)

###############
# Generate ethnicity (Hispanic) variable
###############

# FIX THIS NEXT

# table(chis$srh.a)
hispanic <- chis$srh.a
hispanic <- factor(hispanic)
table(hispanic, useNA = "if")

# Need to impute missing data here since 11 were not collected
hispanic[hispanic == levels(hispanic)[1]] <- NA
hispanic <- factor(hispanic)
hispanic <- impute(hispanic)

table(hispanic, useNA = "ifany")
chis$srh.a.i <- hispanic

table(chis$srh.a.i)
rm(hispanic)

# with(chis, table(racehp2p, srh.a))

###############
# Generate poverty variable
###############
# table(chis$povll)
chis$povll <- factor(chis$povll)

# levels(chis$povll)
belowpovl <- rep(NA, samplesize)
belowpovl[as.numeric(chis$povll) == 1] <- 1
belowpovl[as.numeric(chis$povll) > 1] <- 0
belowpovl <- factor(belowpovl, levels = 0:1, labels = c("100% FPL or higher", "Below 100% FPL"))
chis$belowpovl <- belowpovl
# table(chis$belowpovl)
rm(belowpovl)

###############
# Clean birthweight variable
###############
# summary(chis$brthwk.p)
# table(chis$brthwk.p == -9)
# 179 individuals have -9 value
chis$brthwk.p[chis$brthwk.p == -9] <- NA
# summary(chis$brthwk.p)
# IMPUTE birthweight by median
brthwk.p.impute <- impute(chis$brthwk.p)
# summary(brthwk.p.impute)
chis$brthwk.p.i <- brthwk.p.impute
# par(mfrow = c(1,1))
# hist(chis$brthwk.p.i)
rm(brthwk.p.impute)

###############
# Clean insured variable
# ins64 is type of current insurance coverage for <65 yos
###############
# table(chis$ins64)
chis$ins64 <- factor(chis$ins64) # get rid of empty categories
# table(chis$ins64)

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
# table(unins.ever)
unins.ever <- factor(unins.ever, levels = c(0,1), labels = c("Never uninsured", "Some or All Year uninsured"))
# table(unins.ever)
chis$unins.ever <- unins.ever
table(chis$unins.ever, useNA = "if")
rm(unins.ever)


###############
# survey specification of design
###############
rchis <- svrepdesign(chis[ , -( 212 + ( 1 : 80 ))], repweights = chis[ , ( 212 + ( 1 : 80 ))], weights = chis$rakedw0, combined.weights = TRUE, scale = 1, rscales = rep(1,80), type="other")
summary(rchis)
