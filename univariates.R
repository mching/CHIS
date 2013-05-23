##############
# Univariate analyses
# Outcome = Referral
# Also check on pedsHiRisk
##############

#######################
# Gender
# variable = male
#######################
table(chis$male)
svytotal(~male, rchis)
svymean(~male, rchis)
svytable(~male + referred, rchis)
svyby(~male, by = ~referred, design = rchis, FUN = svymean)
svychisq(~male + referred, rchis)
# No association found by gender

#######################
# Birthweight
# variable = brthwk.p.i
#######################
summary(chis$brthwk.p.i)
svymean(~brthwk.p.i, rchis)
svyby(~brthwk.p.i, by = ~referred, design = rchis, FUN = svymean)
svyttest(brthwk.p.i ~ referred, design = rchis)
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
# Poverty level
# srage.p
#######################
summary(chis$srage.p)
svymean(~srage.p, rchis)
svyby(~srage.p, by = ~referred, design = rchis, FUN = svymean)
svyttest(srage.p ~ referred, design = rchis)
