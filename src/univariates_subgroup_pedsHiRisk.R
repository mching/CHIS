#######################
#######################
# Univariate analyses
# Outcome = Referral
# Subgroup= pedsHiRisk
#######################
#######################

# How to specify the new subgroup for pedsHiRisk only
pedsHi.subpop.design <- subset(rchis, pedsHiRisk == "Yes")
summary(pedsHi.subpop.design)

#######################
# Gender
# variable = male
#######################
table(chis$male)
svytotal(~male, pedsHi.subpop.design)
svymean(~male, pedsHi.subpop.design)
svytable(~male + referred, pedsHi.subpop.design)
svyby(~male, by = ~referred, design = pedsHi.subpop.design, FUN = svymean)
svychisq(~male + referred, pedsHi.subpop.design, statistic = "Chisq")

Table <- svytable(~male + referred, pedsHi.subpop.design)
summary(Table)

# No association found by gender
# Why is this? I think boys have higher risk than girls, but
# maybe this is saying that boys are less likely to be referred?

#######################
# Birthweight
# variable = brthwk.p.i
#######################
summary(chis$brthwk.p.i)
svymean(~brthwk.p.i, pedsHi.subpop.design)
svyby(~brthwk.p.i, by = ~referred, design = pedsHi.subpop.design, FUN = svymean)
svyttest(brthwk.p.i ~ referred, design = pedsHi.subpop.design)
svyboxplot(brthwk.p.i ~ referred, design = pedsHi.subpop.design, xlab = "Referred", ylab = "Birthweight (kg)", main = "Birthweight by Referral Status")
# There is not a significant difference between the mean birthweights in referred
# and non referred children among pedsHiRisk patients

#######################
# Usual source of care
# variable = cd1.2
#######################
table(chis$cd1.2)
svytotal(~cd1.2, pedsHi.subpop.design)
svymean(~cd1.2, pedsHi.subpop.design)
svytable(~cd1.2 + referred, pedsHi.subpop.design)
svyby(~cd1.2, by = ~referred, design = pedsHi.subpop.design, FUN = svymean)
svychisq(~cd1.2 + referred, pedsHi.subpop.design)
# No effect
# Should we be looking at finer detail than has or doesn't have. Perhaps
# having doctor or kaiser is better than ER

#######################
# Ethnicity
# srh.a.i
#######################
table(chis$srh.a.i)
svytotal(~srh.a.i, pedsHi.subpop.design)
svymean(~srh.a.i, pedsHi.subpop.design)
confint(svymean(~srh.a.i, pedsHi.subpop.design))

svytable(~srh.a.i + referred, pedsHi.subpop.design)
svyby(~srh.a.i, by = ~referred, design = pedsHi.subpop.design, FUN = svymean)
svychisq(~srh.a.i + referred, pedsHi.subpop.design)
# No bivariate ethnicity effect

#######################
# Race
# racehp2p
#######################
table(chis$racehp2p)
svytotal(~racehp2p, pedsHi.subpop.design)
svymean(~racehp2p, pedsHi.subpop.design)
svytable(~racehp2p + referred, pedsHi.subpop.design)
summary(svytable(~racehp2p + referred, pedsHi.subpop.design))
svyby(~racehp2p, by = ~referred, design = pedsHi.subpop.design, FUN = svymean)
svychisq(~racehp2p + referred, pedsHi.subpop.design)
# No significant bivariate race association

#######################
# Just white race vs non-white
# white
#######################
table(chis$white)
svytotal(~white, pedsHi.subpop.design)
svymean(~white, pedsHi.subpop.design)
svytable(~white + referred, pedsHi.subpop.design)
svyby(~white, by = ~referred, design = pedsHi.subpop.design, FUN = svymean)
svychisq(~white + referred, pedsHi.subpop.design)
# No significant difference between referrals in white and nonwhite

#######################
# Type of insurance
# ins64
#######################
table(chis$ins64)
svytotal(~ins64, pedsHi.subpop.design)
svymean(~ins64, pedsHi.subpop.design)
svytable(~ins64 + referred, pedsHi.subpop.design)
svyby(~ins64, by = ~referred, design = pedsHi.subpop.design, FUN = svymean)
svychisq(~ins64 + referred, pedsHi.subpop.design)
# Type of insurance is almost (very close) significantly associated by chisquare

ref.ins64.univ <- svyglm(referred ~ ins64, family = quasibinomial, pedsHi.subpop.design)
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
svytotal(~unins.ever, pedsHi.subpop.design)
svymean(~unins.ever, pedsHi.subpop.design)
svytable(~unins.ever + referred, pedsHi.subpop.design)
svyby(~unins.ever, by = ~referred, design = pedsHi.subpop.design, FUN = svymean)
svychisq(~unins.ever + referred, pedsHi.subpop.design)
# Significant at 0.05 level with uninsured kids being less likely
# to have been referred

# ref.unins.univ <- svyglm(referred ~ unins.ever, family = quasibinomial, pedsHi.subpop.design)
# summary(ref.unins.univ)
# exp(ref.unins.univ$coef)
# No longer significant although uninsured kids had half the odds of 
# having been referred

#######################
# Poverty level
# belowpovl
#######################
table(chis$belowpovl)
svytotal(~belowpovl, pedsHi.subpop.design)
svymean(~belowpovl, pedsHi.subpop.design)
svytable(~belowpovl + referred, pedsHi.subpop.design)
svyby(~belowpovl, by = ~referred, design = pedsHi.subpop.design, FUN = svymean)
svychisq(~belowpovl + referred, pedsHi.subpop.design)
# Not significant association between poverty level and referral

#######################
# Age
# srage.p
#######################
summary(chis$srage.p)
svymean(~srage.p, pedsHi.subpop.design)
svyby(~srage.p, by = ~referred, design = pedsHi.subpop.design, FUN = svymean)
svyttest(srage.p ~ referred, design = pedsHi.subpop.design)
# Highly significant association by mean age between referred and not referred

model.age <- svyglm(referred ~ srage.p, design = pedsHi.subpop.design, family = quasibinomial())
summary(model.age)
