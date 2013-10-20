# What is the overlab between cf46 and cf47?
# cf46 = developmental specialist referral
# cf47 = speech-language-hearing referral
# chis has only children under 6
<<<<<<< HEAD
=======
# run munge and 0-1 scripts first
>>>>>>> 9952b0077c2ef74ae9cf47e302c4af22c430e4bb

library(survey)

# Population level estimates without standard errors
<<<<<<< HEAD
svytable(~cf46+cf47, rchis05)
=======
svytable(~cf46+cf47, rchis)
>>>>>>> 9952b0077c2ef74ae9cf47e302c4af22c430e4bb

# There are no missing on either cf46 or cf47
table(is.na(chis$cf47) == is.na(chis$cf46))

# The total of children 
<<<<<<< HEAD
referred.total <- sum(svytotal(~cf46, na.rm = T, design = rchis05))
# sum(svytotal(~cf47, na.rm = T, design = rchis))

a <- svyby(~cf46, by = ~cf47, FUN = svytotal, design = rchis05)
=======
referred.total <- sum(svytotal(~cf46, na.rm = T, design = rchis))
# sum(svytotal(~cf47, na.rm = T, design = rchis))

a <- svyby(~cf46, by = ~cf47, FUN = svytotal, design = rchis)
>>>>>>> 9952b0077c2ef74ae9cf47e302c4af22c430e4bb
a
a/referred.total
confint(a)
confint(a)/referred.total

<<<<<<< HEAD
svymean(~cf46, na.rm = T, rchis05)
svymean(~cf47, na.rm = T, rchis05)
=======
svymean(~cf46, na.rm = T, rchis)
svymean(~cf47, na.rm = T, rchis)
>>>>>>> 9952b0077c2ef74ae9cf47e302c4af22c430e4bb
