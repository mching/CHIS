####################################################################################
# This file contains code to generate the descriptive estimates for Table 1 and the 
# first portion of the Results section.
# 
# Michael Ching, MD, MPH
#
####################################################################################

# VARIABLES OF INTEREST:
# gender (male)
# birthweight  (brthwk.p.i)
# ethnicity  (srh.a.i)
# race  (racehp2p)
# below poverty level (belowpovl)
# PEDS risk categories (peds)
# age (srage.p)
# uninsured in the last year (unins.ever)
# Developmental specialist referred (cf46)
# speech referred (cf47)
# Referred to either (referred)

### Gender
svytotal(~male, rchis05)
sum(svytotal(~male, rchis05))
svymean(~male, rchis05)
confint(svymean(~male, rchis05))
cbind(svymean(~male, rchis05), confint(svymean(~male, rchis05)))

### Age
cbind(svymean(~srage.p, rchis05), confint(svymean(~srage.p, rchis05)))

### Birthweight
# 95% CI of mean
cbind(svymean(~brthwk.p.i, rchis05), confint(svymean(~brthwk.p.i, rchis05)))
# Median
svyquantile(~brthwk.p.i, design = rchis05, quantiles = 0.5) 

### PEDS
cbind(svymean(~peds, rchis05, na.rm = T), confint(svymean(~peds, rchis05, na.rm = T)))

### Hispanic Ethnicity
cbind(svymean(~srh.a.i, rchis05), confint(svymean(~srh.a.i, rchis05)))

### Race
svytotal(~racehp2p, rchis05)
cbind(svymean(~racehp2p, rchis05), confint(svymean(~racehp2p, rchis05)))

### Lack of Insurance
svytotal(~unins.ever, rchis05, na.rm = T)
cbind(svymean(~unins.ever, rchis05), confint(svymean(~unins.ever, rchis05)))

### Below Poverty Level
svytotal(~belowpovl, rchis05, na.rm = T)
cbind(svymean(~belowpovl, rchis05), confint(svymean(~belowpovl, rchis05)))

### Referred to Developmental Specialist
svytotal(~cf46, rchis05, na.rm = T)
svyvar(~as.numeric(cf46), rchis05, na.rm = T)
cbind(svymean(~cf46, rchis05, na.rm = T), confint(svymean(~cf46, rchis05, na.rm = T)))

### Referred to Speech, Language, Hearing Evaluation
svytotal(~cf47, rchis05, na.rm = T)
svyvar(~as.numeric(cf47), rchis05, na.rm = T)
cbind(svymean(~cf47, rchis05, na.rm = T), confint(svymean(~cf47, rchis05, na.rm = T)))

### Referred to either Developmental Specialist or Speech, Language, Hearing Evaluation
svytotal(~referred, rchis05, na.rm = T)
svyvar(~as.numeric(referred), rchis05, na.rm = T)
cbind(svytotal(~referred, rchis05, na.rm = T), svymean(~referred, rchis05, na.rm = T), confint(svymean(~referred, rchis05, na.rm = T)))

### Generate 2x2 table of referrals to DS vs referrals to SLH
svytable(~cf46+cf47, rchis05)
referred.total <- sum(svytotal(~cf46, na.rm = T, design = rchis05))
a <- svyby(~cf46, by = ~cf47, FUN = svytotal, design = rchis05)
a[,2:5]
confint(a)
a[,2:5]/referred.total
confint(a)/referred.total

### Break referrals down by PEDS risk status (i.e., how many low risk were referred? high risk?)
svyby(~cf46, ~pedsHiRisk, design = rchis05, FUN = svymean, na.rm = T) # referred to DS
svyby(~cf47, ~pedsHiRisk, design = rchis05, FUN = svymean, na.rm = T) # referred to SLH
svyby(~referred, ~pedsHiRisk, design = rchis05, FUN = svymean, na.rm = T) # referred to DS or SLH

