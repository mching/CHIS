#######################
# CHIS All-in-one
#######################

#######
# Somehow the models result in opposite odds ratios (low referral for pedsHiRisk)
#######

# clear and load data
rm(list=ls())
library(ProjectTemplate)
setwd("~/Dropbox/Mike/CHIS")
load.project()

################
# Munge
################

# Munge for building variables

library(Hmisc)
library(survey)

# Drop all kids older than 5 
chis <- chis[chis$srage.p < 6, ]
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

table(chis$cf46, useNA = "ifany")
devo.referred.sample.size <- sum(table(chis$cf46))

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

#######################
#######################
# Univariate analyses
# Outcome = cf46
# Also check on associations with pedsHiRisk 
# Can also rerun using outcome = cf46 and outcome = cf47
#######################
#######################

#######################
# Gender
# variable = male
#######################
table(chis$male)
svytotal(~male, rchis)
svymean(~male, rchis)
svytable(~male + cf46, rchis)
svyby(~male, by = ~cf46, design = rchis, FUN = svymean)
svychisq(~male + cf46, rchis, statistic = "Chisq")

Table <- svytable(~male + cf46, rchis)
summary(Table)
prop.table(Table, 1)

# Almost significant association found by gender
# This differs from the referral to both devo and speech

#######################
# Birthweight
# variable = brthwk.p.i
#######################
summary(chis$brthwk.p.i)
svymean(~brthwk.p.i, rchis)
svyby(~brthwk.p.i, by = ~cf46, design = rchis, FUN = svymean)
svyttest(brthwk.p.i ~ cf46, design = rchis)
svyboxplot(brthwk.p.i ~ cf46, design = rchis, xlab = "Referred to Dev", ylab = "Birthweight (kg)", main = "Birthweight by Referral Status")
# There is not a significant difference between the mean birthweights in development referred
# This differs from the referred variable, possibly because of sample size

#######################
# Usual source of care
# variable = cd1.2
#######################
table(chis$cd1.2)
svytotal(~cd1.2, rchis)
svymean(~cd1.2, rchis)
svytable(~cd1.2 + cf46, rchis)
svyby(~cd1.2, by = ~cf46, design = rchis, FUN = svymean)
svychisq(~cd1.2 + cf46, rchis)
# No effect
# Should we be looking at finer detail than has or doesn't have. Perhaps
# having doctor or kaiser is better than ER

#######################
# Ethnicity
# srh.a.i
#######################
table(chis$srh.a.i)
svytotal(~srh.a.i, rchis)
svymean(~srh.a.i, rchis)
confint(svymean(~srh.a.i, rchis))

svytable(~srh.a.i + cf46, rchis)
svyby(~srh.a.i, by = ~cf46, design = rchis, FUN = svymean)
svychisq(~srh.a.i + cf46, rchis)
# No univariate ethnicity effect

#######################
# Race
# racehp2p
#######################
table(chis$racehp2p)
svytotal(~racehp2p, rchis)
svymean(~racehp2p, rchis)
svytable(~racehp2p + cf46, rchis)
summary(svytable(~racehp2p + cf46, rchis))
svyby(~racehp2p, by = ~cf46, design = rchis, FUN = svymean)
svychisq(~racehp2p + cf46, rchis)
# No significant univariate race association

#######################
# Just white race vs non-white
# white
#######################
table(chis$white)
svytotal(~white, rchis)

svymean(~white, rchis)
svytable(~white + cf46, rchis)
svyby(~white, by = ~cf46, design = rchis, FUN = svymean)
svychisq(~white + cf46, rchis)
# No significant difference between referrals in white and nonwhite

#######################
# Type of insurance
# ins64
#######################
table(chis$ins64)
svytotal(~ins64, rchis)
svymean(~ins64, rchis)
svytable(~ins64 + cf46, rchis)
svyby(~ins64, by = ~cf46, design = rchis, FUN = svymean)
svychisq(~ins64 + cf46, rchis)
# Type of insurance is almost significantly associated by chisquare

ref.ins64.univ <- svyglm(referred ~ ins64, family = quasibinomial, rchis)
summary(ref.ins64.univ)
exp(ref.ins64.univ$coef)
# Maybe too many categories
# Might have to collapse some

#######################
# Lack of insurance
# Look to see if current insurance vs this past year insurance has an effect
# unins.ever
#######################
# unins.ever refers to whether a child has had insurance the whole year or not
table(chis$unins.ever)
svytotal(~unins.ever, rchis)
svymean(~unins.ever, rchis)
svytable(~unins.ever + cf46, rchis)
svyby(~unins.ever, by = ~cf46, design = rchis, FUN = svymean)
svychisq(~unins.ever + cf46, rchis)
# Not significant at 0.05 level with uninsured kids being less likely
# to have been referred

# ref.unins.univ <- svyglm(referred ~ unins.ever, family = quasibinomial, rchis)
# summary(ref.unins.univ)
# exp(ref.unins.univ$coef)
# No longer significant although uninsured kids had half the odds of 
# having been referred

#######################
# Poverty level
# belowpovl
#######################
table(chis$belowpovl)
svytotal(~belowpovl, rchis)
svymean(~belowpovl, rchis)
svytable(~belowpovl + cf46, rchis)
svyby(~belowpovl, by = ~cf46, design = rchis, FUN = svymean)
svychisq(~belowpovl + cf46, rchis)

#######################
# Age
# srage.p
#######################
summary(chis$srage.p)
svymean(~srage.p, rchis)
svyby(~srage.p, by = ~cf46, design = rchis, FUN = svymean)
svyttest(srage.p ~ cf46, design = rchis)
model.age <- svyglm(cf46 ~ srage.p, design = rchis, family = quasibinomial())
summary(model.age)

#######################
#######################
# Univariate analyses
# Outcome = cf47
#######################
#######################

#######################
# Gender
# variable = male
#######################
table(chis$male)
svytotal(~male, rchis)
svymean(~male, rchis)
svytable(~male + cf47, rchis)
svyby(~male, by = ~cf47, design = rchis, FUN = svymean)
svychisq(~male + cf47, rchis, statistic = "Chisq")

Table <- svytable(~male + cf47, rchis)
summary(Table)
prop.table(Table, 1)

# Almost significant association found by gender
# This differs from the referral to both devo and speech

#######################
# Birthweight
# variable = brthwk.p.i
#######################
summary(chis$brthwk.p.i)
svymean(~brthwk.p.i, rchis)
svyby(~brthwk.p.i, by = ~cf47, design = rchis, FUN = svymean)
svyttest(brthwk.p.i ~ cf47, design = rchis)
svyboxplot(brthwk.p.i ~ cf47, design = rchis, xlab = "Referred to Dev", ylab = "Birthweight (kg)", main = "Birthweight by Referral Status")
# There is not a significant difference between the mean birthweights in development referred
# This differs from the referred variable, possibly because of sample size

#######################
# Usual source of care
# variable = cd1.2
#######################
table(chis$cd1.2)
svytotal(~cd1.2, rchis)
svymean(~cd1.2, rchis)
svytable(~cd1.2 + cf47, rchis)
svyby(~cd1.2, by = ~cf47, design = rchis, FUN = svymean)
svychisq(~cd1.2 + cf47, rchis)
# No effect
# Should we be looking at finer detail than has or doesn't have. Perhaps
# having doctor or kaiser is better than ER

#######################
# Ethnicity
# srh.a.i
#######################
table(chis$srh.a.i)
svytotal(~srh.a.i, rchis)
svymean(~srh.a.i, rchis)
confint(svymean(~srh.a.i, rchis))

svytable(~srh.a.i + cf47, rchis)
svyby(~srh.a.i, by = ~cf47, design = rchis, FUN = svymean)
svychisq(~srh.a.i + cf47, rchis)
# No univariate ethnicity effect

#######################
# Race
# racehp2p
#######################
table(chis$racehp2p)
svytotal(~racehp2p, rchis)
svymean(~racehp2p, rchis)
svytable(~racehp2p + cf47, rchis)
summary(svytable(~racehp2p + cf47, rchis))
svyby(~racehp2p, by = ~cf47, design = rchis, FUN = svymean)
svychisq(~racehp2p + cf47, rchis)
# No significant univariate race association

#######################
# Just white race vs non-white
# white
#######################
table(chis$white)
svytotal(~white, rchis)

svymean(~white, rchis)
svytable(~white + cf47, rchis)
svyby(~white, by = ~cf47, design = rchis, FUN = svymean)
svychisq(~white + cf47, rchis)
# No significant difference between referrals in white and nonwhite

#######################
# Type of insurance
# ins64
#######################
table(chis$ins64)
svytotal(~ins64, rchis)
svymean(~ins64, rchis)
svytable(~ins64 + cf47, rchis)
svyby(~ins64, by = ~cf47, design = rchis, FUN = svymean)
svychisq(~ins64 + cf47, rchis)
# Type of insurance is almost significantly associated by chisquare

ref.ins64.univ <- svyglm(referred ~ ins64, family = quasibinomial, rchis)
summary(ref.ins64.univ)
exp(ref.ins64.univ$coef)
# Maybe too many categories
# Might have to collapse some

#######################
# Lack of insurance
# Look to see if current insurance vs this past year insurance has an effect
# unins.ever
#######################
# unins.ever refers to whether a child has had insurance the whole year or not
table(chis$unins.ever)
svytotal(~unins.ever, rchis)
svymean(~unins.ever, rchis)
svytable(~unins.ever + cf47, rchis)
svyby(~unins.ever, by = ~cf47, design = rchis, FUN = svymean)
svychisq(~unins.ever + cf47, rchis)
# Not significant at 0.05 level with uninsured kids being less likely
# to have been referred

# ref.unins.univ <- svyglm(referred ~ unins.ever, family = quasibinomial, rchis)
# summary(ref.unins.univ)
# exp(ref.unins.univ$coef)
# No longer significant although uninsured kids had half the odds of 
# having been referred

#######################
# Poverty level
# belowpovl
#######################
table(chis$belowpovl)
svytotal(~belowpovl, rchis)
svymean(~belowpovl, rchis)
svytable(~belowpovl + cf47, rchis)
svyby(~belowpovl, by = ~cf47, design = rchis, FUN = svymean)
svychisq(~belowpovl + cf47, rchis)

#######################
# Age
# srage.p
#######################
summary(chis$srage.p)
svymean(~srage.p, rchis)
svyby(~srage.p, by = ~cf47, design = rchis, FUN = svymean)
svyttest(srage.p ~ cf47, design = rchis)
model.age <- svyglm(cf47 ~ srage.p, design = rchis, family = quasibinomial())
summary(model.age)

#######################
#######################
# Univariate analyses
# Outcome = Referral
# Also check on associations with pedsHiRisk 
# Can also rerun using outcome = cd46 and outcome = cd 47
#######################
#######################

#######################
# Gender
# variable = male
#######################
table(chis$male)
svytotal(~male, rchis)
svymean(~male, rchis)
svytable(~male + referred, rchis)
svyby(~male, by = ~referred, design = rchis, FUN = svymean)
svychisq(~male + referred, rchis, statistic = "Chisq")

Table <- svytable(~male + referred, rchis)
summary(Table)

# No association found by gender
# Why is this? I think boys have higher risk than girls, but
# maybe this is saying that boys are less likely to be referred?

#######################
# Birthweight
# variable = brthwk.p.i
#######################
summary(chis$brthwk.p.i)
svymean(~brthwk.p.i, rchis)
svyby(~brthwk.p.i, by = ~referred, design = rchis, FUN = svymean)
svyttest(brthwk.p.i ~ referred, design = rchis)
svyboxplot(brthwk.p.i ~ referred, design = rchis, xlab = "Referred", ylab = "Birthweight (kg)", main = "Birthweight by Referral Status")
# There is a significant difference between the mean birthweights in referred
# and non referred children

#######################
# Usual source of care
# variable = cd1.2
#######################
table(chis$cd1.2)
svytotal(~cd1.2, rchis)
svymean(~cd1.2, rchis)
svytable(~cd1.2 + referred, rchis)
svyby(~cd1.2, by = ~referred, design = rchis, FUN = svymean)
svychisq(~cd1.2 + referred, rchis)
# No effect
# Should we be looking at finer detail than has or doesn't have. Perhaps
# having doctor or kaiser is better than ER

#######################
# Ethnicity
# srh.a.i
#######################
table(chis$srh.a.i)
svytotal(~srh.a.i, rchis)
svymean(~srh.a.i, rchis)
confint(svymean(~srh.a.i, rchis))

svytable(~srh.a.i + referred, rchis)
svyby(~srh.a.i, by = ~referred, design = rchis, FUN = svymean)
svychisq(~srh.a.i + referred, rchis)
# No univariate ethnicity effect

#######################
# Race
# racehp2p
#######################
table(chis$racehp2p)
svytotal(~racehp2p, rchis)
svymean(~racehp2p, rchis)
svytable(~racehp2p + referred, rchis)
summary(svytable(~racehp2p + referred, rchis))
svyby(~racehp2p, by = ~referred, design = rchis, FUN = svymean)
svychisq(~racehp2p + referred, rchis)
# No significant univariate race association

#######################
# Just white race vs non-white
# white
#######################
table(chis$white)
svytotal(~white, rchis)
svymean(~white, rchis)
svytable(~white + referred, rchis)
svyby(~white, by = ~referred, design = rchis, FUN = svymean)
svychisq(~white + referred, rchis)
# No significant difference between referrals in white and nonwhite

#######################
# Type of insurance
# ins64
#######################
table(chis$ins64)
svytotal(~ins64, rchis)
svymean(~ins64, rchis)
svytable(~ins64 + referred, rchis)
svyby(~ins64, by = ~referred, design = rchis, FUN = svymean)
svychisq(~ins64 + referred, rchis)
# Type of insurance is significantly associated by chisquare

ref.ins64.univ <- svyglm(referred ~ ins64, family = quasibinomial, rchis)
summary(ref.ins64.univ)
exp(ref.ins64.univ$coef)
# Maybe too many categories
# Might have to collapse some

#######################
# Lack of insurance
# Look to see if current insurance vs this past year insurance has an effect
# unins.ever
#######################
# unins.ever refers to whether a child has had insurance the whole year or not
table(chis$unins.ever)
svytotal(~unins.ever, rchis)
svymean(~unins.ever, rchis)
svytable(~unins.ever + referred, rchis)
svyby(~unins.ever, by = ~referred, design = rchis, FUN = svymean)
svychisq(~unins.ever + referred, rchis)
# Just barely significant at 0.05 level with uninsured kids being less likely
# to have been referred

# ref.unins.univ <- svyglm(referred ~ unins.ever, family = quasibinomial, rchis)
# summary(ref.unins.univ)
# exp(ref.unins.univ$coef)
# No longer significant although uninsured kids had half the odds of 
# having been referred

#######################
# Poverty level
# belowpovl
#######################
table(chis$belowpovl)
svytotal(~belowpovl, rchis)
svymean(~belowpovl, rchis)
svytable(~belowpovl + referred, rchis)
svyby(~belowpovl, by = ~referred, design = rchis, FUN = svymean)
svychisq(~belowpovl + referred, rchis)

#######################
# Age
# srage.p
#######################
summary(chis$srage.p)
svymean(~srage.p, rchis)
svyby(~srage.p, by = ~referred, design = rchis, FUN = svymean)
svyttest(srage.p ~ referred, design = rchis)
model.age <- svyglm(referred ~ srage.p, design = rchis, family = quasibinomial())
summary(model.age)

########################
# Logistic Regression Models
# 1 Outcome = referred to devo or speech, Predictor including peds
# 2 Outcome = referred to devo, Predictor including peds
# 3 Outcome = referred to speech, Predictor including peds
# 4 Outcome = referred to devo or speech, pedsHiRisk only
# 5 Outcome = referred to devo, pedsHiRisk only
# 6 Outcome = referred to speech, pedsHiRisk only
########################

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
cbind(OddsRatio = exp(model5$coef), exp(confint(model5)))

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
