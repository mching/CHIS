#######################
#######################
# Univariate analyses
# Outcome = cf47
#######################
#######################
sum(svytable(~cf47, rchis))

sum(svytotal(~cf47, rchis, na.rm = T))
svymean(~cf47, rchis, na.rm = T)

svytable(~pedsHiRisk + cf47, rchis)
svyby(~cf47, by = ~pedsHiRisk, design = rchis, FUN = svymean, na.rm = T)
svychisq(~pedsHiRisk + cf47, rchis, statistic = "Chisq")

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
# not a significant association between poverty and speech referral

#######################
# Age
# srage.p
#######################
summary(chis$srage.p)
svymean(~srage.p, rchis)
svyby(~srage.p, by = ~cf47, design = rchis, FUN = svymean)
svyttest(srage.p ~ cf47, design = rchis)
# significant association between speech and age

model.age <- svyglm(cf47 ~ srage.p, design = rchis, family = quasibinomial())
summary(model.age)
