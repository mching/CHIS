Tables for poster
=========

Load the Data
-----
First we load the data:


```r
library(ProjectTemplate)
```

```
## Loading required package: testthat
```

```r
library(Hmisc)
```

```
## Loading required package: survival
```

```
## Loading required package: splines
```

```
## Hmisc library by Frank E Harrell Jr
## 
## Type library(help='Hmisc'), ?Overview, or ?Hmisc.Overview') to see overall
## documentation.
## 
## NOTE:Hmisc no longer redefines [.factor to drop unused levels when
## subsetting.  To get the old behavior of Hmisc type dropUnusedLevels().
```

```
## Attaching package: 'Hmisc'
```

```
## The following object(s) are masked from 'package:survival':
## 
## untangle.specials
```

```
## The following object(s) are masked from 'package:base':
## 
## format.pval, round.POSIXt, trunc.POSIXt, units
```

```r
library(survey)
```

```
## Attaching package: 'survey'
```

```
## The following object(s) are masked from 'package:Hmisc':
## 
## deff
```

```
## The following object(s) are masked from 'package:graphics':
## 
## dotchart
```

```r
library(stargazer)
```

```
## Error: there is no package called 'stargazer'
```

```r
library(binom)
```

```
## Loading required package: lattice
```

```r

setwd("~/Dropbox/Mike/CHIS")
load.project()
```

```
## Loading project configuration
```

```
## Autoloading helper functions
```

```
## Running helper script: helpers.R
```

```
## Autoloading data
```

```
## Loading data set: CHILD
```

```
## Munging data
```

```
## Running preprocessing script: 01-A.R
```

```
## Warning: duplicated levels will not be allowed in factors anymore
```


This also runs the script to clean the data, contained within the file `/munge/01-A.R`.

Table 1: Univariates
-------
We want:  
* gender (male)
* birthweight  (brthwk.p.i)
* ethnicity  (srh.a.i)
* race  (racehp2p)
* below poverty level (belowpovl)
* PEDS risk categories (peds)
* age (srage.p)
* uninsured in the last year (unins.ever)
* Developmental specialist referred (cf46)
* speech referred (cf47)
* Referred to either (referred)

### Gender

```r
summary(chis$male)
```

```
## Female   Male 
##   2035   2228
```

```r
length(chis$male)
```

```
## [1] 4263
```

```r
binom.confint(sum(chis$male == "Male", na.rm = T), length(!is.na(chis$male)), 
    method = "asym")
```

```
##       method    x    n   mean  lower  upper
## 1 asymptotic 2228 4263 0.5226 0.5076 0.5376
```

```r

svytotal(~male, rchis)
```

```
##              total    SE
## maleFemale 1595089 16455
## maleMale   1660717 20365
```

```r
sum(svytotal(~male, rchis))
```

```
## [1] 3255806
```

```r
svymean(~male, rchis)
```

```
##            mean   SE
## maleFemale 0.49 0.01
## maleMale   0.51 0.01
```

```r
confint(svymean(~male, rchis))
```

```
##             2.5 % 97.5 %
## maleFemale 0.4797 0.5001
## maleMale   0.4999 0.5203
```


### Age

```r
mean(chis$srage.p)
```

```
## [1] 2.605
```

```r
sd(chis$srage.p)
```

```
## [1] 1.679
```

```r
n.age <- length(chis$srage.p)
mean(chis$srage.p) + qt(0.025, n.age)
```

```
## [1] 0.6445
```

```r
mean(chis$srage.p) + qt(0.975, n.age)
```

```
## [1] 4.565
```

```r

svymean(~srage.p, rchis)
```

```
##         mean   SE
## srage.p  2.5 0.03
```

```r
confint(svymean(~srage.p, rchis))
```

```
##         2.5 % 97.5 %
## srage.p 2.444  2.559
```


### Birthweight

```r
summary(chis$brthwk.p.i)
```

```
## 
##  187 values imputed to 3.35
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##    0.91    3.01    3.35    3.34    3.69    4.99
```

```r
mean(chis$brthwk.p.i)
```

```
## [1] 3.335
```

```r
sd(chis$brthwk.p.i)
```

```
## [1] 0.5941
```

```r
n.bw <- length(chis$brthwk.p.i)
n.bw
```

```
## [1] 4263
```

```r
mean(chis$brthwk.p.i) + qt(0.025, n.bw)
```

```
## [1] 1.375
```

```r
mean(chis$brthwk.p.i) + qt(0.975, n.bw)
```

```
## [1] 5.296
```

```r

svymean(~brthwk.p.i, rchis)
```

```
##            mean   SE
## brthwk.p.i 3.31 0.02
```

```r
confint(svymean(~brthwk.p.i, rchis))
```

```
##            2.5 % 97.5 %
## brthwk.p.i 3.281  3.344
```


### PEDS

```r
summary(chis$peds)
```

```
##       NO RISK      LOW RISK MODERATE RISK     HIGH RISK          NA's 
##          1754           800           756           792           161
```

```r
pedstable <- table(chis$peds)
pedstable
```

```
## 
##       NO RISK      LOW RISK MODERATE RISK     HIGH RISK 
##          1754           800           756           792
```

```r
sum(pedstable)
```

```
## [1] 4102
```

```r
sapply(pedstable, binom.confint, n = sum(pedstable), method = "asy")
```

```
##        NO RISK  LOW RISK MODERATE RISK HIGH RISK
## method factor,1 factor,1 factor,1      factor,1 
## x      1754     800      756           792      
## n      4102     4102     4102          4102     
## mean   0.4276   0.195    0.1843        0.1931   
## lower  0.4125   0.1829   0.1724        0.181    
## upper  0.4427   0.2072   0.1962        0.2052
```


### Hispanic Ethnicity

```r
summary(chis$srh.a.i)
```

```
## 
##  11 values imputed to NO
```

```
##  YES   NO 
## 1556 2707
```

```r
binom.confint(sum(chis$srh.a.i == "YES"), length(chis$srh.a.i), method = "asym")
```

```
##       method    x    n  mean  lower  upper
## 1 asymptotic 1556 4263 0.365 0.3505 0.3795
```

```r

svytotal(~srh.a.i, rchis)
```

```
##              total    SE
## srh.a.iYES 1576142 38297
## srh.a.iNO  1679664 39011
```

```r
sum(svytotal(~srh.a.i, rchis))
```

```
## [1] 3255806
```

```r
svymean(~srh.a.i, rchis)
```

```
##             mean   SE
## srh.a.iYES 0.484 0.01
## srh.a.iNO  0.516 0.01
```

```r
confint(svymean(~srh.a.i, rchis))
```

```
##             2.5 % 97.5 %
## srh.a.iYES 0.4613 0.5069
## srh.a.iNO  0.4931 0.5387
```


### Race

```r
summary(chis$racehp2p)
```

```
##                         WHITE                        LATINO 
##                          1948                          1239 
##                         ASIAN              AFRICAN AMERICAN 
##                           617                           140 
## PI/OTHER SINGLE/MULTIPLE RACE 
##                           319
```

```r
racetable <- table(chis$racehp2p)
racetable
```

```
## 
##                         WHITE                        LATINO 
##                          1948                          1239 
##                         ASIAN              AFRICAN AMERICAN 
##                           617                           140 
## PI/OTHER SINGLE/MULTIPLE RACE 
##                           319
```

```r
sapply(racetable, binom.confint, n = sum(racetable), method = "asy")
```

```
##        WHITE    LATINO   ASIAN    AFRICAN AMERICAN
## method factor,1 factor,1 factor,1 factor,1        
## x      1948     1239     617      140             
## n      4263     4263     4263     4263            
## mean   0.457    0.2906   0.1447   0.03284         
## lower  0.442    0.277    0.1342   0.02749         
## upper  0.4719   0.3043   0.1553   0.03819         
##        PI/OTHER SINGLE/MULTIPLE RACE
## method factor,1                     
## x      319                          
## n      4263                         
## mean   0.07483                      
## lower  0.06693                      
## upper  0.08273
```

```r

svytotal(~racehp2p, rchis)
```

```
##                                         total    SE
## racehp2pWHITE                         1172443 35327
## racehp2pLATINO                        1281091 41261
## racehp2pASIAN                          322606 23191
## racehp2pAFRICAN AMERICAN               153466 14233
## racehp2pPI/OTHER SINGLE/MULTIPLE RACE  326200 22629
```

```r
sum(svytotal(~racehp2p, rchis))
```

```
## [1] 3255806
```

```r
svymean(~racehp2p, rchis)
```

```
##                                         mean   SE
## racehp2pWHITE                         0.3601 0.01
## racehp2pLATINO                        0.3935 0.01
## racehp2pASIAN                         0.0991 0.01
## racehp2pAFRICAN AMERICAN              0.0471 0.00
## racehp2pPI/OTHER SINGLE/MULTIPLE RACE 0.1002 0.01
```

```r
confint(svymean(~racehp2p, rchis))
```

```
##                                         2.5 %  97.5 %
## racehp2pWHITE                         0.33920 0.38102
## racehp2pLATINO                        0.36894 0.41802
## racehp2pASIAN                         0.08527 0.11290
## racehp2pAFRICAN AMERICAN              0.03854 0.05573
## racehp2pPI/OTHER SINGLE/MULTIPLE RACE 0.08647 0.11391
```


### Lack of Insurance

```r
ins.table <- table(chis$unins.ever)
ins.table
```

```
## 
##            Never uninsured Some or All Year uninsured 
##                       3994                        269
```

```r
binom.confint(ins.table[2], sum(ins.table), method = "asym")
```

```
##       method   x    n   mean  lower  upper
## 1 asymptotic 269 4263 0.0631 0.0558 0.0704
```


### Below Poverty Level

```r
summary(chis$belowpovl)
```

```
## 100% FPL or higher     Below 100% FPL 
##               3349                914
```

```r
length(chis$belowpovl)
```

```
## [1] 4263
```

```r
binom.confint(sum(chis$belowpovl == "Below 100% FPL", na.rm = T), length(!is.na(chis$belowpovl)), 
    method = "asym")
```

```
##       method   x    n   mean  lower  upper
## 1 asymptotic 914 4263 0.2144 0.2021 0.2267
```

```r
# svymean(~belowpovl, rchis) ## Population level estimate
# confint(svymean(~belowpovl, rchis))
```



### Referred to Developmental Specialist

```r
summary(chis$cf46)
```

```
## Not referred     Referred         NA's 
##         3335          441          487
```

```r
length(chis$cf46)
```

```
## [1] 4263
```

```r
table(chis$cf46)
```

```
## 
## Not referred     Referred 
##         3335          441
```

```r
sum(table(chis$cf46))
```

```
## [1] 3776
```

```r
binom.confint(sum(chis$cf46 == "Referred", na.rm = T), sum(table(chis$cf46)), 
    method = "asym")
```

```
##       method   x    n   mean  lower upper
## 1 asymptotic 441 3776 0.1168 0.1065 0.127
```


### Referred to Speech, Language, Hearing Evaluation

```r
summary(chis$cf47)
```

```
## Not referred     Referred         NA's 
##         3221          555          487
```

```r
length(chis$cf47)
```

```
## [1] 4263
```

```r
table(chis$cf47)
```

```
## 
## Not referred     Referred 
##         3221          555
```

```r
sum(table(chis$cf47))
```

```
## [1] 3776
```

```r
binom.confint(sum(chis$cf47 == "Referred", na.rm = T), sum(table(chis$cf47)), 
    method = "asym")
```

```
##       method   x    n  mean  lower  upper
## 1 asymptotic 555 3776 0.147 0.1357 0.1583
```


### Referred to either Developmental Specialist or Speech, Language, Hearing Evaluation

```r
summary(chis$referred)
```

```
##   No  Yes NA's 
## 3061  715  487
```

```r
length(chis$referred)
```

```
## [1] 4263
```

```r
table(chis$referred)
```

```
## 
##   No  Yes 
## 3061  715
```

```r
sum(table(chis$referred))
```

```
## [1] 3776
```

```r
binom.confint(sum(chis$referred == "Yes", na.rm = T), sum(table(chis$referred)), 
    method = "asym")
```

```
##       method   x    n   mean  lower  upper
## 1 asymptotic 715 3776 0.1894 0.1769 0.2019
```


Table 2: Unadjusted Bivariate Comparisons
------
We will want to see what is the rate of referrals among children in each of those categories and whether they are significant. We must use the survey functions for weighting to test the associations.

### Gender

```r
svyby(~male, by = ~cf46, design = rchis, FUN = svymean)
```

```
##                      cf46 maleFemale maleMale     se1     se2
## Not referred Not referred     0.5050   0.4950 0.01102 0.01102
## Referred         Referred     0.4241   0.5759 0.03739 0.03739
```

```r
svychisq(~male + cf46, rchis, statistic = "Chisq")
```

```
## 
## 	Pearson's X^2: Rao & Scott adjustment
## 
## data:  svychisq(~male + cf46, rchis, statistic = "Chisq") 
## X-squared = 11.35, df = 1, p-value = 0.05685
```

```r

svyby(~male, by = ~cf47, design = rchis, FUN = svymean)
```

```
##                      cf47 maleFemale maleMale     se1     se2
## Not referred Not referred     0.5068   0.4932 0.01056 0.01056
## Referred         Referred     0.4326   0.5674 0.03756 0.03756
```

```r
svychisq(~male + cf47, rchis, statistic = "Chisq")
```

```
## 
## 	Pearson's X^2: Rao & Scott adjustment
## 
## data:  svychisq(~male + cf47, rchis, statistic = "Chisq") 
## X-squared = 11.96, df = 1, p-value = 0.06888
```

```r

svyby(~male, by = ~referred, design = rchis, FUN = svymean)
```

```
##     referred maleFemale maleMale     se1     se2
## No        No     0.5050   0.4950 0.01182 0.01182
## Yes      Yes     0.4572   0.5428 0.03191 0.03191
```

```r
svychisq(~male + referred, rchis, statistic = "Chisq")
```

```
## 
## 	Pearson's X^2: Rao & Scott adjustment
## 
## data:  svychisq(~male + referred, rchis, statistic = "Chisq") 
## X-squared = 6.097, df = 1, p-value = 0.1995
```


### Age

```r
svyby(~srage.p, by = ~cf46, design = rchis, FUN = svymean)
```

```
##                      cf46 srage.p      se
## Not referred Not referred   2.857 0.03012
## Referred         Referred   3.095 0.12037
```

```r
svyttest(srage.p ~ cf46, design = rchis)
```

```
## 
## 	Design-based t-test
## 
## data:  srage.p ~ cf46 
## t = 1.881, df = 78, p-value = 0.06364
## alternative hypothesis: true difference in mean is not equal to 0 
## sample estimates:
## difference in mean 
##             0.2381
```

```r

svyby(~srage.p, by = ~cf47, design = rchis, FUN = svymean)
```

```
##                      cf47 srage.p      se
## Not referred Not referred   2.812 0.03388
## Referred         Referred   3.297 0.10165
```

```r
svyttest(srage.p ~ cf47, design = rchis)
```

```
## 
## 	Design-based t-test
## 
## data:  srage.p ~ cf47 
## t = 4.178, df = 78, p-value = 7.598e-05
## alternative hypothesis: true difference in mean is not equal to 0 
## sample estimates:
## difference in mean 
##             0.4848
```

```r

svyby(~srage.p, by = ~referred, design = rchis, FUN = svymean)
```

```
##     referred srage.p      se
## No        No   2.826 0.03484
## Yes      Yes   3.128 0.08728
```

```r
svyttest(srage.p ~ referred, design = rchis)
```

```
## 
## 	Design-based t-test
## 
## data:  srage.p ~ referred 
## t = 2.95, df = 78, p-value = 0.004195
## alternative hypothesis: true difference in mean is not equal to 0 
## sample estimates:
## difference in mean 
##             0.3016
```


### Birthweight

```r
svyby(~brthwk.p, by = ~cf46, design = rchis, FUN = svymean, na.rm = T)
```

```
##                      cf46 brthwk.p      se
## Not referred Not referred    3.330 0.01749
## Referred         Referred    3.191 0.08931
```

```r
svyttest(brthwk.p ~ cf46, design = rchis)
```

```
## 
## 	Design-based t-test
## 
## data:  brthwk.p ~ cf46 
## t = -1.526, df = 78, p-value = 0.1311
## alternative hypothesis: true difference in mean is not equal to 0 
## sample estimates:
## difference in mean 
##            -0.1394
```

```r

svyby(~brthwk.p, by = ~cf47, design = rchis, FUN = svymean, na.rm = T)
```

```
##                      cf47 brthwk.p      se
## Not referred Not referred    3.330 0.01703
## Referred         Referred    3.225 0.06839
```

```r
svyttest(brthwk.p ~ cf47, design = rchis)
```

```
## 
## 	Design-based t-test
## 
## data:  brthwk.p ~ cf47 
## t = -1.524, df = 78, p-value = 0.1317
## alternative hypothesis: true difference in mean is not equal to 0 
## sample estimates:
## difference in mean 
##            -0.1055
```

```r

svyby(~brthwk.p, by = ~referred, design = rchis, FUN = svymean, na.rm = T)
```

```
##     referred brthwk.p      se
## No        No    3.343 0.01773
## Yes      Yes    3.193 0.05685
```

```r
svyttest(brthwk.p ~ referred, design = rchis)
```

```
## 
## 	Design-based t-test
## 
## data:  brthwk.p ~ referred 
## t = -2.571, df = 78, p-value = 0.01204
## alternative hypothesis: true difference in mean is not equal to 0 
## sample estimates:
## difference in mean 
##            -0.1507
```


### Race

```r
svyby(~racehp2p, by = ~cf46, design = rchis, FUN = svymean)
```

```
##                      cf46 racehp2pWHITE racehp2pLATINO racehp2pASIAN
## Not referred Not referred        0.3670         0.3900       0.09821
## Referred         Referred        0.3991         0.3451       0.10589
##              racehp2pAFRICAN AMERICAN
## Not referred                  0.05080
## Referred                      0.04303
##              racehp2pPI/OTHER SINGLE/MULTIPLE RACE     se1     se2
## Not referred                               0.09395 0.01270 0.01399
## Referred                                   0.10686 0.04036 0.03900
##                   se3      se4      se5
## Not referred 0.007715 0.005373 0.007869
## Referred     0.021922 0.010583 0.031622
```

```r
svychisq(~racehp2p + cf46, rchis, statistic = "Chisq")
```

```
## 
## 	Pearson's X^2: Rao & Scott adjustment
## 
## data:  svychisq(~racehp2p + cf46, rchis, statistic = "Chisq") 
## X-squared = 5.004, df = 4, p-value = 0.7909
```

```r

svyby(~racehp2p, by = ~cf47, design = rchis, FUN = svymean)
```

```
##                      cf47 racehp2pWHITE racehp2pLATINO racehp2pASIAN
## Not referred Not referred        0.3690         0.3850       0.10073
## Referred         Referred        0.3807         0.3841       0.08977
##              racehp2pAFRICAN AMERICAN
## Not referred                  0.05131
## Referred                      0.04190
##              racehp2pPI/OTHER SINGLE/MULTIPLE RACE     se1     se2
## Not referred                               0.09402 0.01296 0.01543
## Referred                                   0.10348 0.03767 0.04168
##                   se3      se4      se5
## Not referred 0.008778 0.005705 0.007768
## Referred     0.017195 0.011840 0.027294
```

```r
svychisq(~racehp2p + cf47, rchis, statistic = "Chisq")
```

```
## 
## 	Pearson's X^2: Rao & Scott adjustment
## 
## data:  svychisq(~racehp2p + cf47, rchis, statistic = "Chisq") 
## X-squared = 2.332, df = 4, p-value = 0.9569
```

```r

svyby(~racehp2p, by = ~referred, design = rchis, FUN = svymean)
```

```
##     referred racehp2pWHITE racehp2pLATINO racehp2pASIAN
## No        No        0.3681         0.3867       0.09902
## Yes      Yes        0.3818         0.3773       0.09937
##     racehp2pAFRICAN AMERICAN racehp2pPI/OTHER SINGLE/MULTIPLE RACE     se1
## No                   0.05205                               0.09421 0.01319
## Yes                  0.04100                               0.10055 0.03206
##         se2      se3      se4      se5
## No  0.01555 0.008578 0.006026 0.007955
## Yes 0.03439 0.015680 0.009666 0.022454
```

```r
svychisq(~racehp2p + referred, rchis, statistic = "Chisq")
```

```
## 
## 	Pearson's X^2: Rao & Scott adjustment
## 
## data:  svychisq(~racehp2p + referred, rchis, statistic = "Chisq") 
## X-squared = 2.4, df = 4, p-value = 0.9456
```


### Hispanic Ethnicity

```r
svyby(~srh.a.i, by = ~cf46, design = rchis, FUN = svymean)
```

```
##                      cf46 srh.a.iYES srh.a.iNO     se1     se2
## Not referred Not referred     0.4789    0.5211 0.01377 0.01377
## Referred         Referred     0.4229    0.5771 0.04086 0.04086
```

```r
svychisq(~srh.a.i + cf46, rchis, statistic = "Chisq")
```

```
## 
## 	Pearson's X^2: Rao & Scott adjustment
## 
## data:  svychisq(~srh.a.i + cf46, rchis, statistic = "Chisq") 
## X-squared = 5.446, df = 1, p-value = 0.2097
```

```r

svyby(~srh.a.i, by = ~cf47, design = rchis, FUN = svymean)
```

```
##                      cf47 srh.a.iYES srh.a.iNO     se1     se2
## Not referred Not referred     0.4736    0.5264 0.01404 0.01404
## Referred         Referred     0.4657    0.5343 0.04026 0.04026
```

```r
svychisq(~srh.a.i + cf47, rchis, statistic = "Chisq")
```

```
## 
## 	Pearson's X^2: Rao & Scott adjustment
## 
## data:  svychisq(~srh.a.i + cf47, rchis, statistic = "Chisq") 
## X-squared = 0.134, df = 1, p-value = 0.8566
```

```r

svyby(~srh.a.i, by = ~referred, design = rchis, FUN = svymean)
```

```
##     referred srh.a.iYES srh.a.iNO     se1     se2
## No        No     0.4754    0.5246 0.01422 0.01422
## Yes      Yes     0.4602    0.5398 0.03225 0.03225
```

```r
svychisq(~srh.a.i + referred, rchis, statistic = "Chisq")
```

```
## 
## 	Pearson's X^2: Rao & Scott adjustment
## 
## data:  svychisq(~srh.a.i + referred, rchis, statistic = "Chisq") 
## X-squared = 0.611, df = 1, p-value = 0.6707
```


### Below Poverty Level

```r
svyby(~belowpovl, by = ~cf46, design = rchis, FUN = svymean)
```

```
##                      cf46 belowpovl100% FPL or higher
## Not referred Not referred                      0.7170
## Referred         Referred                      0.7779
##              belowpovlBelow 100% FPL     se1     se2
## Not referred                  0.2830 0.01462 0.01462
## Referred                      0.2221 0.03371 0.03371
```

```r
svychisq(~belowpovl + cf46, rchis, statistic = "Chisq")
```

```
## 
## 	Pearson's X^2: Rao & Scott adjustment
## 
## data:  svychisq(~belowpovl + cf46, rchis, statistic = "Chisq") 
## X-squared = 8.042, df = 1, p-value = 0.1331
```

```r

svyby(~belowpovl, by = ~cf47, design = rchis, FUN = svymean)
```

```
##                      cf47 belowpovl100% FPL or higher
## Not referred Not referred                      0.7277
## Referred         Referred                      0.7029
##              belowpovlBelow 100% FPL     se1     se2
## Not referred                  0.2723 0.01440 0.01440
## Referred                      0.2971 0.04353 0.04353
```

```r
svychisq(~belowpovl + cf47, rchis, statistic = "Chisq")
```

```
## 
## 	Pearson's X^2: Rao & Scott adjustment
## 
## data:  svychisq(~belowpovl + cf47, rchis, statistic = "Chisq") 
## X-squared = 1.663, df = 1, p-value = 0.5878
```

```r

svyby(~belowpovl, by = ~referred, design = rchis, FUN = svymean)
```

```
##     referred belowpovl100% FPL or higher belowpovlBelow 100% FPL     se1
## No        No                      0.7257                  0.2743 0.01522
## Yes      Yes                      0.7169                  0.2831 0.03468
##         se2
## No  0.01522
## Yes 0.03468
```

```r
svychisq(~belowpovl + referred, rchis, statistic = "Chisq")
```

```
## 
## 	Pearson's X^2: Rao & Scott adjustment
## 
## data:  svychisq(~belowpovl + referred, rchis, statistic = "Chisq") 
## X-squared = 0.26, df = 1, p-value = 0.8208
```


### Uninsured Ever in Last Year

```r
svyby(~unins.ever, by = ~cf46, design = rchis, FUN = svymean)
```

```
##                      cf46 unins.everNever uninsured
## Not referred Not referred                    0.9118
## Referred         Referred                    0.9514
##              unins.everSome or All Year uninsured      se1      se2
## Not referred                              0.08823 0.009263 0.009263
## Referred                                  0.04857 0.019138 0.019138
```

```r
svychisq(~unins.ever + cf46, rchis, statistic = "Chisq")
```

```
## 
## 	Pearson's X^2: Rao & Scott adjustment
## 
## data:  svychisq(~unins.ever + cf46, rchis, statistic = "Chisq") 
## X-squared = 8.895, df = 1, p-value = 0.1345
```

```r

svyby(~unins.ever, by = ~cf47, design = rchis, FUN = svymean)
```

```
##                      cf47 unins.everNever uninsured
## Not referred Not referred                    0.9112
## Referred         Referred                    0.9454
##              unins.everSome or All Year uninsured      se1      se2
## Not referred                              0.08879 0.009557 0.009557
## Referred                                  0.05457 0.016478 0.016478
```

```r
svychisq(~unins.ever + cf47, rchis, statistic = "Chisq")
```

```
## 
## 	Pearson's X^2: Rao & Scott adjustment
## 
## data:  svychisq(~unins.ever + cf47, rchis, statistic = "Chisq") 
## X-squared = 8.283, df = 1, p-value = 0.1266
```

```r

svyby(~unins.ever, by = ~referred, design = rchis, FUN = svymean)
```

```
##     referred unins.everNever uninsured
## No        No                    0.9084
## Yes      Yes                    0.9491
##     unins.everSome or All Year uninsured     se1     se2
## No                               0.09157 0.01014 0.01014
## Yes                              0.05085 0.01340 0.01340
```

```r
svychisq(~unins.ever + referred, rchis, statistic = "Chisq")
```

```
## 
## 	Pearson's X^2: Rao & Scott adjustment
## 
## data:  svychisq(~unins.ever + referred, rchis, statistic = "Chisq") 
## X-squared = 14.41, df = 1, p-value = 0.04059
```


### PEDS High Risk

```r
svyby(~pedsHiRisk, by = ~cf46, design = rchis, FUN = svymean)
```

```
##                      cf46 pedsHiRiskNo pedsHiRiskYes     se1     se2
## Not referred Not referred       0.8249        0.1751 0.01433 0.01433
## Referred         Referred       0.5078        0.4922 0.04173 0.04173
```

```r
svychisq(~pedsHiRisk + cf46, rchis, statistic = "Chisq")
```

```
## 
## 	Pearson's X^2: Rao & Scott adjustment
## 
## data:  svychisq(~pedsHiRisk + cf46, rchis, statistic = "Chisq") 
## X-squared = 261.2, df = 1, p-value = 8.514e-16
```

```r

svyby(~pedsHiRisk, by = ~cf47, design = rchis, FUN = svymean)
```

```
##                      cf47 pedsHiRiskNo pedsHiRiskYes     se1     se2
## Not referred Not referred       0.8311        0.1689 0.01372 0.01372
## Referred         Referred       0.5460        0.4540 0.04216 0.04216
```

```r
svychisq(~pedsHiRisk + cf47, rchis, statistic = "Chisq")
```

```
## 
## 	Pearson's X^2: Rao & Scott adjustment
## 
## data:  svychisq(~pedsHiRisk + cf47, rchis, statistic = "Chisq") 
## X-squared = 264.2, df = 1, p-value = 5.615e-16
```

```r

svyby(~pedsHiRisk, by = ~referred, design = rchis, FUN = svymean)
```

```
##     referred pedsHiRiskNo pedsHiRiskYes     se1     se2
## No        No       0.8402        0.1598 0.01419 0.01419
## Yes      Yes       0.5731        0.4269 0.03444 0.03444
```

```r
svychisq(~pedsHiRisk + referred, rchis, statistic = "Chisq")
```

```
## 
## 	Pearson's X^2: Rao & Scott adjustment
## 
## data:  svychisq(~pedsHiRisk + referred, rchis, statistic = "Chisq") 
## X-squared = 285, df = 1, p-value < 2.2e-16
```


Table 3: Regression Analyses
-----
