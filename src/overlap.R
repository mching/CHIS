# What is the overlab between cf46 and cf47?
# cf46 = developmental specialist referral
# cf47 = speech-language-hearing referral
# run munge and 0-1 scripts first

library(survey)

# Population level estimates without standard errors
svytable(~cf46+cf47, rchis05)

# There are no missing on either cf46 or cf47
table(is.na(chis$cf47) == is.na(chis$cf46))

# The total of children 
referred.total <- sum(svytotal(~cf46, na.rm = T, design = rchis05))

a <- svyby(~cf46, by = ~cf47, FUN = svytotal, design = rchis05)

# sum(svytotal(~cf47, na.rm = T, design = rchis))

a[,2:5]
a[,2:5]/referred.total

confint(a)
confint(a)/referred.total

svymean(~cf46, na.rm = T, rchis05)
svymean(~cf47, na.rm = T, rchis05)
