## This is an obsolete file kept for historical purposes. The updated file to read in the data uses the Project Template library
## Refer to the readme file to load the data properly

# Michael Ching
# Script to read in data from CHIS 2009 Child Public Use 
# dataset

rm(list= ls())
# Read in 2009 CHIS data file CHILD.dta
library(foreign, pos = 4)
library(survey)

setwd("~/Dropbox/Mike/CHIS")
chis <- read.dta("/home/mching/Dropbox/Mike/CHIS/data/CHILD.dta", convert.dates = TRUE, convert.factors = TRUE, missing.type = TRUE, convert.underscore = TRUE, warn.missing.labels = FALSE)
save(chis, file="CHILD.rda")

# Tell R the model design.
# Code modified from:
# http://faculty.washington.edu/tlumley/survey/survey-wss.pdf
# rakedw0 is column 212
# rakedw1-80 are columns 213-292

rchis <- svrepdesign(chis[ , -( 212 + ( 1 : 80 ))], 
                   repweights = chis[ , ( 212 + ( 1 : 80 ))],
                   weights = chis$rakedw0, combined.weights = TRUE,
                   scale = 1, rscales = rep(1,80), type="other")
summary(rchis)

# Run some tests to compare with AskCHIS results
plot(svytable(~peds+srsex, rchis))

chis$peds[1:10]
(chis$peds == "INAPPLICABLE")[1:10]
chis$peds[chis$peds == "INAPPLICABLE"] <- NA

svytotal(~peds, design = rchis) ## the same?
confint(svymean(~peds, rchis))

gender <- svymean(~srsex, design=rchis)
svyby(~srsex, ~racehp2p, svymean, design=rchis, keep.names=F)

gender
gender/sum(gender)
confint(gender)
confint(gender) / sum(gender) 
# SE is the proportion SE
# confint uses the wrong SE for generating the confidence intervals for gender

confint(svymean(~srsex, rchis))

# ins describes current insurance status
insured <- svytotal(~ins, rchis)
insured
confint(insured)
confint(insured) / sum(insured) * 100
# Result is the same as in AskCHIS

# subset to export to STATA
chis.1<-chis[, c(107, 130, 182, 210:292)]

setwd("/home/mching/Dropbox/Mike/CHIS")
write.csv(chis.1, file = "chis1.csv")

str(chis.1)
