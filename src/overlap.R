# What is the overlab between cf46 and cf47?
# cf46 = developmental specialist referral
# cf47 = speech-language-hearing referral
# chis has only children under 6

library(survey)

# Population level estimates without standard errors
svytable(~cf46+cf47, rchis)

# There are no missing on either cf46 or cf47
table(is.na(chis$cf47) == is.na(chis$cf46))

# The total of children 
referred.total <- sum(svytotal(~cf46, na.rm = T, design = rchis))
# sum(svytotal(~cf47, na.rm = T, design = rchis))

a <- svyby(~cf46, by = ~cf47, FUN = svytotal, design = rchis)
a
a/referred.total
confint(a)
confint(a)/referred.total

svymean(~cf46, na.rm = T, rchis)
svymean(~cf47, na.rm = T, rchis)