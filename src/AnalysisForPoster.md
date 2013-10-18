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
## Loading required package: splines
## Loading required package: Formula
## Hmisc library by Frank E Harrell Jr
## 
## Type library(help='Hmisc'), ?Overview, or ?Hmisc.Overview')
## to see overall documentation.
## 
## 
## Attaching package: 'Hmisc'
## 
## The following object is masked from 'package:survival':
## 
##     untangle.specials
## 
## The following object is masked from 'package:base':
## 
##     format.pval, round.POSIXt, trunc.POSIXt, units
```

```r
library(survey)
```

```
## 
## Attaching package: 'survey'
## 
## The following object is masked from 'package:Hmisc':
## 
##     deff
## 
## The following object is masked from 'package:graphics':
## 
##     dotchart
```

```r
library(binom)
```

```
## Loading required package: lattice
```

```r
library(ggplot2)
library(scales)

setwd("~/Dropbox/Mike/CHIS")
load.project()
```

```
## Loading project configuration
## Autoloading helper functions
##  Running helper script: helpers.R
## Autoloading data
##  Loading data set: CHILD
## Munging data
##  Running preprocessing script: 01-A.R
```

```
## Warning: duplicated levels in factors are deprecated
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

```r

svytotal(~peds, rchis, na.rm = T)
```

```
##                     total    SE
## pedsNO RISK       1282446 50173
## pedsLOW RISK       581922 29600
## pedsMODERATE RISK  609918 39432
## pedsHIGH RISK      621856 39044
```

```r
sum(svytotal(~peds, rchis, na.rm = T))
```

```
## [1] 3096142
```

```r
svymean(~peds, rchis, na.rm = T)
```

```
##                    mean   SE
## pedsNO RISK       0.414 0.02
## pedsLOW RISK      0.188 0.01
## pedsMODERATE RISK 0.197 0.01
## pedsHIGH RISK     0.201 0.01
```

```r
confint(svymean(~peds, rchis, na.rm = T))
```

```
##                    2.5 % 97.5 %
## pedsNO RISK       0.3827 0.4458
## pedsLOW RISK      0.1695 0.2064
## pedsMODERATE RISK 0.1727 0.2213
## pedsHIGH RISK     0.1765 0.2252
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

```r

svytotal(~unins.ever, rchis, na.rm = T)
```

```
##                                        total    SE
## unins.everNever uninsured            3004682 27163
## unins.everSome or All Year uninsured  251124 23755
```

```r
sum(svytotal(~unins.ever, rchis, na.rm = T))
```

```
## [1] 3255806
```

```r
svymean(~unins.ever, rchis)
```

```
##                                        mean   SE
## unins.everNever uninsured            0.9229 0.01
## unins.everSome or All Year uninsured 0.0771 0.01
```

```r
confint(svymean(~unins.ever, rchis))
```

```
##                                        2.5 % 97.5 %
## unins.everNever uninsured            0.90860 0.9371
## unins.everSome or All Year uninsured 0.06286 0.0914
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

svytotal(~belowpovl, rchis, na.rm = T)
```

```
##                               total    SE
## belowpovl100% FPL or higher 2367090 41594
## belowpovlBelow 100% FPL      888716 36737
```

```r
sum(svytotal(~belowpovl, rchis, na.rm = T))
```

```
## [1] 3255806
```

```r
svymean(~belowpovl, rchis)
```

```
##                              mean   SE
## belowpovl100% FPL or higher 0.727 0.01
## belowpovlBelow 100% FPL     0.273 0.01
```

```r
confint(svymean(~belowpovl, rchis))
```

```
##                              2.5 % 97.5 %
## belowpovl100% FPL or higher 0.7044 0.7496
## belowpovlBelow 100% FPL     0.2504 0.2956
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

```r

svytotal(~cf46, rchis, na.rm = T)
```

```
##                    total    SE
## cf46Not referred 2497857 36629
## cf46Referred      324251 23668
```

```r
sum(svytotal(~cf46, rchis, na.rm = T))
```

```
## [1] 2822108
```

```r
svymean(~cf46, rchis, na.rm = T)
```

```
##                   mean   SE
## cf46Not referred 0.885 0.01
## cf46Referred     0.115 0.01
```

```r
confint(svymean(~cf46, rchis, na.rm = T))
```

```
##                    2.5 % 97.5 %
## cf46Not referred 0.86883 0.9014
## cf46Referred     0.09862 0.1312
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

```r

svytotal(~cf47, rchis, na.rm = T)
```

```
##                    total    SE
## cf47Not referred 2399995 39689
## cf47Referred      422113 31599
```

```r
sum(svytotal(~cf47, rchis, na.rm = T))
```

```
## [1] 2822108
```

```r
svymean(~cf47, rchis, na.rm = T)
```

```
##                  mean   SE
## cf47Not referred 0.85 0.01
## cf47Referred     0.15 0.01
```

```r
confint(svymean(~cf47, rchis, na.rm = T))
```

```
##                   2.5 % 97.5 %
## cf47Not referred 0.8289 0.8719
## cf47Referred     0.1281 0.1711
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

```r

svytotal(~referred, rchis, na.rm = T)
```

```
##               total    SE
## referredNo  2274827 38592
## referredYes  547281 32783
```

```r
sum(svytotal(~referred, rchis, na.rm = T))
```

```
## [1] 2822108
```

```r
svymean(~referred, rchis, na.rm = T)
```

```
##              mean   SE
## referredNo  0.806 0.01
## referredYes 0.194 0.01
```

```r
confint(svymean(~referred, rchis, na.rm = T))
```

```
##              2.5 % 97.5 %
## referredNo  0.7841 0.8280
## referredYes 0.1720 0.2159
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


Table 2: Alternate Method by Odds Ratios
-----
We can also calculate the unadjusted odds ratios as below. We should probably write a function to automate this task but maybe this is too much effort.

### Gender

```r
model1 <- svyglm(cf46 ~ male, design = rchis, family = quasibinomial)
model2 <- svyglm(cf47 ~ male, design = rchis, family = quasibinomial)
model3 <- svyglm(referred ~ male, design = rchis, family = quasibinomial)

summary(model1)
```

```
## 
## Call:
## svyglm(formula = cf46 ~ male, design = rchis, family = quasibinomial)
## 
## Survey design:
## svrepdesign.default(chis[, -(212 + (1:80))], repweights = chis[, 
##     (212 + (1:80))], weights = chis$rakedw0, combined.weights = TRUE, 
##     scale = 1, rscales = rep(1, 80), type = "other")
## 
## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)    
## (Intercept)   -2.216      0.113   -19.6   <2e-16 ***
## maleMale       0.326      0.171     1.9    0.061 .  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for quasibinomial family taken to be 0.8668)
## 
## Number of Fisher Scoring iterations: 5
```

```r
summary(model2)
```

```
## 
## Call:
## svyglm(formula = cf47 ~ male, design = rchis, family = quasibinomial)
## 
## Survey design:
## svrepdesign.default(chis[, -(212 + (1:80))], repweights = chis[, 
##     (212 + (1:80))], weights = chis$rakedw0, combined.weights = TRUE, 
##     scale = 1, rscales = rep(1, 80), type = "other")
## 
## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)    
## (Intercept)   -1.896      0.150  -12.64   <2e-16 ***
## maleMale       0.299      0.167    1.79    0.078 .  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for quasibinomial family taken to be 0.8668)
## 
## Number of Fisher Scoring iterations: 5
```

```r
summary(model3)
```

```
## 
## Call:
## svyglm(formula = referred ~ male, design = rchis, family = quasibinomial)
## 
## Survey design:
## svrepdesign.default(chis[, -(212 + (1:80))], repweights = chis[, 
##     (212 + (1:80))], weights = chis$rakedw0, combined.weights = TRUE, 
##     scale = 1, rscales = rep(1, 80), type = "other")
## 
## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)    
## (Intercept)   -1.524      0.121  -12.62   <2e-16 ***
## maleMale       0.192      0.151    1.27     0.21    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for quasibinomial family taken to be 0.8668)
## 
## Number of Fisher Scoring iterations: 4
```

```r

cbind(OddsRatio = exp(model1$coef), exp(confint(model1)))
```

```
##             OddsRatio   2.5 % 97.5 %
## (Intercept)     0.109 0.08735 0.1361
## maleMale        1.385 0.99015 1.9382
```

```r
cbind(OddsRatio = exp(model2$coef), exp(confint(model2)))
```

```
##             OddsRatio  2.5 % 97.5 %
## (Intercept)    0.1501 0.1119 0.2014
## maleMale       1.3480 0.9718 1.8698
```

```r
cbind(OddsRatio = exp(model3$coef), exp(confint(model3)))
```

```
##             OddsRatio  2.5 % 97.5 %
## (Intercept)    0.2178 0.1719  0.276
## maleMale       1.2113 0.9014  1.628
```


### Age

```r
model1 <- svyglm(cf46 ~ srage.p, design = rchis, family = quasibinomial)
model2 <- svyglm(cf47 ~ srage.p, design = rchis, family = quasibinomial)
model3 <- svyglm(referred ~ srage.p, design = rchis, family = quasibinomial)

summary(model1)
```

```
## 
## Call:
## svyglm(formula = cf46 ~ srage.p, design = rchis, family = quasibinomial)
## 
## Survey design:
## svrepdesign.default(chis[, -(212 + (1:80))], repweights = chis[, 
##     (212 + (1:80))], weights = chis$rakedw0, combined.weights = TRUE, 
##     scale = 1, rscales = rep(1, 80), type = "other")
## 
## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  -2.3686     0.1861  -12.73   <2e-16 ***
## srage.p       0.1098     0.0586    1.88    0.064 .  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for quasibinomial family taken to be 0.8658)
## 
## Number of Fisher Scoring iterations: 5
```

```r
summary(model2)
```

```
## 
## Call:
## svyglm(formula = cf47 ~ srage.p, design = rchis, family = quasibinomial)
## 
## Survey design:
## svrepdesign.default(chis[, -(212 + (1:80))], repweights = chis[, 
##     (212 + (1:80))], weights = chis$rakedw0, combined.weights = TRUE, 
##     scale = 1, rscales = rep(1, 80), type = "other")
## 
## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  -2.4332     0.2130  -11.43  < 2e-16 ***
## srage.p       0.2274     0.0548    4.15  8.5e-05 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for quasibinomial family taken to be 0.8622)
## 
## Number of Fisher Scoring iterations: 5
```

```r
summary(model3)
```

```
## 
## Call:
## svyglm(formula = referred ~ srage.p, design = rchis, family = quasibinomial)
## 
## Survey design:
## svrepdesign.default(chis[, -(212 + (1:80))], repweights = chis[, 
##     (212 + (1:80))], weights = chis$rakedw0, combined.weights = TRUE, 
##     scale = 1, rscales = rep(1, 80), type = "other")
## 
## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  -1.8398     0.1700  -10.82   <2e-16 ***
## srage.p       0.1394     0.0469    2.98   0.0039 ** 
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for quasibinomial family taken to be 0.8655)
## 
## Number of Fisher Scoring iterations: 4
```

```r

cbind(OddsRatio = exp(model1$coef), exp(confint(model1)))
```

```
##             OddsRatio   2.5 % 97.5 %
## (Intercept)   0.09362 0.06501 0.1348
## srage.p       1.11608 0.99505 1.2518
```

```r
cbind(OddsRatio = exp(model2$coef), exp(confint(model2)))
```

```
##             OddsRatio   2.5 % 97.5 %
## (Intercept)   0.08775 0.05781 0.1332
## srage.p       1.25539 1.12746 1.3978
```

```r
cbind(OddsRatio = exp(model3$coef), exp(confint(model3)))
```

```
##             OddsRatio  2.5 % 97.5 %
## (Intercept)    0.1588 0.1138 0.2217
## srage.p        1.1496 1.0487 1.2602
```


### Birthweight

```r
model1 <- svyglm(cf46 ~ brthwk.p.i, design = rchis, family = quasibinomial)
model2 <- svyglm(cf47 ~ brthwk.p.i, design = rchis, family = quasibinomial)
model3 <- svyglm(referred ~ brthwk.p.i, design = rchis, family = quasibinomial)

summary(model1)
```

```
## 
## Call:
## svyglm(formula = cf46 ~ brthwk.p.i, design = rchis, family = quasibinomial)
## 
## Survey design:
## svrepdesign.default(chis[, -(212 + (1:80))], repweights = chis[, 
##     (212 + (1:80))], weights = chis$rakedw0, combined.weights = TRUE, 
##     scale = 1, rscales = rep(1, 80), type = "other")
## 
## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)
## (Intercept)   -0.977      0.660   -1.48     0.14
## brthwk.p.i    -0.326      0.212   -1.53     0.13
## 
## (Dispersion parameter for quasibinomial family taken to be 0.8856)
## 
## Number of Fisher Scoring iterations: 5
```

```r
summary(model2)
```

```
## 
## Call:
## svyglm(formula = cf47 ~ brthwk.p.i, design = rchis, family = quasibinomial)
## 
## Survey design:
## svrepdesign.default(chis[, -(212 + (1:80))], repweights = chis[, 
##     (212 + (1:80))], weights = chis$rakedw0, combined.weights = TRUE, 
##     scale = 1, rscales = rep(1, 80), type = "other")
## 
## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)  
## (Intercept)   -0.905      0.526   -1.72    0.089 .
## brthwk.p.i    -0.254      0.166   -1.53    0.130  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for quasibinomial family taken to be 0.8737)
## 
## Number of Fisher Scoring iterations: 5
```

```r
summary(model3)
```

```
## 
## Call:
## svyglm(formula = referred ~ brthwk.p.i, design = rchis, family = quasibinomial)
## 
## Survey design:
## svrepdesign.default(chis[, -(212 + (1:80))], repweights = chis[, 
##     (212 + (1:80))], weights = chis$rakedw0, combined.weights = TRUE, 
##     scale = 1, rscales = rep(1, 80), type = "other")
## 
## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)  
## (Intercept)   -0.239      0.452   -0.53    0.599  
## brthwk.p.i    -0.362      0.143   -2.54    0.013 *
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for quasibinomial family taken to be 0.8784)
## 
## Number of Fisher Scoring iterations: 4
```

```r

cbind(OddsRatio = exp(model1$coef), exp(confint(model1)))
```

```
##             OddsRatio  2.5 % 97.5 %
## (Intercept)    0.3764 0.1033  1.372
## brthwk.p.i     0.7220 0.4762  1.095
```

```r
cbind(OddsRatio = exp(model2$coef), exp(confint(model2)))
```

```
##             OddsRatio  2.5 % 97.5 %
## (Intercept)    0.4046 0.1444  1.134
## brthwk.p.i     0.7758 0.5604  1.074
```

```r
cbind(OddsRatio = exp(model3$coef), exp(confint(model3)))
```

```
##             OddsRatio  2.5 % 97.5 %
## (Intercept)    0.7876 0.3247 1.9108
## brthwk.p.i     0.6961 0.5263 0.9207
```


### Race

```r
model1 <- svyglm(cf46 ~ racehp2p, design = rchis, family = quasibinomial)
model2 <- svyglm(cf47 ~ racehp2p, design = rchis, family = quasibinomial)
model3 <- svyglm(referred ~ racehp2p, design = rchis, family = quasibinomial)

summary(model1)
```

```
## 
## Call:
## svyglm(formula = cf46 ~ racehp2p, design = rchis, family = quasibinomial)
## 
## Survey design:
## svrepdesign.default(chis[, -(212 + (1:80))], repweights = chis[, 
##     (212 + (1:80))], weights = chis$rakedw0, combined.weights = TRUE, 
##     scale = 1, rscales = rep(1, 80), type = "other")
## 
## Coefficients:
##                                       Estimate Std. Error t value Pr(>|t|)
## (Intercept)                           -1.95801    0.13794  -14.20   <2e-16
## racehp2pLATINO                        -0.20583    0.19421   -1.06     0.29
## racehp2pASIAN                         -0.00837    0.25059   -0.03     0.97
## racehp2pAFRICAN AMERICAN              -0.24963    0.31760   -0.79     0.43
## racehp2pPI/OTHER SINGLE/MULTIPLE RACE  0.04504    0.38507    0.12     0.91
##                                          
## (Intercept)                           ***
## racehp2pLATINO                           
## racehp2pASIAN                            
## racehp2pAFRICAN AMERICAN                 
## racehp2pPI/OTHER SINGLE/MULTIPLE RACE    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for quasibinomial family taken to be 0.8668)
## 
## Number of Fisher Scoring iterations: 5
```

```r
summary(model2)
```

```
## 
## Call:
## svyglm(formula = cf47 ~ racehp2p, design = rchis, family = quasibinomial)
## 
## Survey design:
## svrepdesign.default(chis[, -(212 + (1:80))], repweights = chis[, 
##     (212 + (1:80))], weights = chis$rakedw0, combined.weights = TRUE, 
##     scale = 1, rscales = rep(1, 80), type = "other")
## 
## Coefficients:
##                                       Estimate Std. Error t value Pr(>|t|)
## (Intercept)                            -1.7066     0.1310  -13.03   <2e-16
## racehp2pLATINO                         -0.0335     0.2080   -0.16     0.87
## racehp2pASIAN                          -0.1466     0.2424   -0.60     0.55
## racehp2pAFRICAN AMERICAN               -0.2340     0.3560   -0.66     0.51
## racehp2pPI/OTHER SINGLE/MULTIPLE RACE   0.0645     0.3323    0.19     0.85
##                                          
## (Intercept)                           ***
## racehp2pLATINO                           
## racehp2pASIAN                            
## racehp2pAFRICAN AMERICAN                 
## racehp2pPI/OTHER SINGLE/MULTIPLE RACE    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for quasibinomial family taken to be 0.8668)
## 
## Number of Fisher Scoring iterations: 5
```

```r
summary(model3)
```

```
## 
## Call:
## svyglm(formula = referred ~ racehp2p, design = rchis, family = quasibinomial)
## 
## Survey design:
## svrepdesign.default(chis[, -(212 + (1:80))], repweights = chis[, 
##     (212 + (1:80))], weights = chis$rakedw0, combined.weights = TRUE, 
##     scale = 1, rscales = rep(1, 80), type = "other")
## 
## Coefficients:
##                                       Estimate Std. Error t value Pr(>|t|)
## (Intercept)                            -1.3881     0.1112  -12.48   <2e-16
## racehp2pLATINO                         -0.0611     0.1771   -0.34     0.73
## racehp2pASIAN                          -0.0330     0.2089   -0.16     0.87
## racehp2pAFRICAN AMERICAN               -0.2751     0.3171   -0.87     0.39
## racehp2pPI/OTHER SINGLE/MULTIPLE RACE   0.0285     0.2793    0.10     0.92
##                                          
## (Intercept)                           ***
## racehp2pLATINO                           
## racehp2pASIAN                            
## racehp2pAFRICAN AMERICAN                 
## racehp2pPI/OTHER SINGLE/MULTIPLE RACE    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for quasibinomial family taken to be 0.8668)
## 
## Number of Fisher Scoring iterations: 4
```

```r

cbind(OddsRatio = exp(model1$coef), exp(confint(model1)))
```

```
##                                       OddsRatio  2.5 % 97.5 %
## (Intercept)                              0.1411 0.1077  0.185
## racehp2pLATINO                           0.8140 0.5563  1.191
## racehp2pASIAN                            0.9917 0.6068  1.621
## racehp2pAFRICAN AMERICAN                 0.7791 0.4181  1.452
## racehp2pPI/OTHER SINGLE/MULTIPLE RACE    1.0461 0.4918  2.225
```

```r
cbind(OddsRatio = exp(model2$coef), exp(confint(model2)))
```

```
##                                       OddsRatio  2.5 % 97.5 %
## (Intercept)                              0.1815 0.1404 0.2346
## racehp2pLATINO                           0.9670 0.6433 1.4537
## racehp2pASIAN                            0.8636 0.5370 1.3889
## racehp2pAFRICAN AMERICAN                 0.7913 0.3938 1.5901
## racehp2pPI/OTHER SINGLE/MULTIPLE RACE    1.0666 0.5561 2.0459
```

```r
cbind(OddsRatio = exp(model3$coef), exp(confint(model3)))
```

```
##                                       OddsRatio  2.5 % 97.5 %
## (Intercept)                              0.2495 0.2007 0.3103
## racehp2pLATINO                           0.9408 0.6649 1.3310
## racehp2pASIAN                            0.9675 0.6425 1.4569
## racehp2pAFRICAN AMERICAN                 0.7595 0.4079 1.4140
## racehp2pPI/OTHER SINGLE/MULTIPLE RACE    1.0290 0.5952 1.7788
```


### Hispanic Ethnicity

```r
model1 <- svyglm(cf46 ~ srh.a.i, design = rchis, family = quasibinomial)
model2 <- svyglm(cf47 ~ srh.a.i, design = rchis, family = quasibinomial)
model3 <- svyglm(referred ~ srh.a.i, design = rchis, family = quasibinomial)

summary(model1)
```

```
## 
## Call:
## svyglm(formula = cf46 ~ srh.a.i, design = rchis, family = quasibinomial)
## 
## Survey design:
## svrepdesign.default(chis[, -(212 + (1:80))], repweights = chis[, 
##     (212 + (1:80))], weights = chis$rakedw0, combined.weights = TRUE, 
##     scale = 1, rscales = rep(1, 80), type = "other")
## 
## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)    
## (Intercept)   -2.166      0.128  -16.96   <2e-16 ***
## srh.a.iNO      0.226      0.180    1.26     0.21    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for quasibinomial family taken to be 0.8668)
## 
## Number of Fisher Scoring iterations: 5
```

```r
summary(model2)
```

```
## 
## Call:
## svyglm(formula = cf47 ~ srh.a.i, design = rchis, family = quasibinomial)
## 
## Survey design:
## svrepdesign.default(chis[, -(212 + (1:80))], repweights = chis[, 
##     (212 + (1:80))], weights = chis$rakedw0, combined.weights = TRUE, 
##     scale = 1, rscales = rep(1, 80), type = "other")
## 
## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  -1.7547     0.1437  -12.21   <2e-16 ***
## srh.a.iNO     0.0315     0.1760    0.18     0.86    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for quasibinomial family taken to be 0.8668)
## 
## Number of Fisher Scoring iterations: 5
```

```r
summary(model3)
```

```
## 
## Call:
## svyglm(formula = referred ~ srh.a.i, design = rchis, family = quasibinomial)
## 
## Survey design:
## svrepdesign.default(chis[, -(212 + (1:80))], repweights = chis[, 
##     (212 + (1:80))], weights = chis$rakedw0, combined.weights = TRUE, 
##     scale = 1, rscales = rep(1, 80), type = "other")
## 
## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  -1.4570     0.1186  -12.28   <2e-16 ***
## srh.a.iNO     0.0607     0.1434    0.42     0.67    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for quasibinomial family taken to be 0.8668)
## 
## Number of Fisher Scoring iterations: 4
```

```r

cbind(OddsRatio = exp(model1$coef), exp(confint(model1)))
```

```
##             OddsRatio   2.5 % 97.5 %
## (Intercept)    0.1146 0.08926 0.1473
## srh.a.iNO      1.2539 0.88180 1.7830
```

```r
cbind(OddsRatio = exp(model2$coef), exp(confint(model2)))
```

```
##             OddsRatio  2.5 % 97.5 %
## (Intercept)     0.173 0.1305 0.2292
## srh.a.iNO       1.032 0.7310 1.4571
```

```r
cbind(OddsRatio = exp(model3$coef), exp(confint(model3)))
```

```
##             OddsRatio  2.5 % 97.5 %
## (Intercept)    0.2329 0.1846 0.2939
## srh.a.iNO      1.0626 0.8022 1.4076
```


### Below Poverty Level

```r
model1 <- svyglm(cf46 ~ belowpovl, design = rchis, family = quasibinomial)
model2 <- svyglm(cf47 ~ belowpovl, design = rchis, family = quasibinomial)
model3 <- svyglm(referred ~ belowpovl, design = rchis, family = quasibinomial)

summary(model1)
```

```
## 
## Call:
## svyglm(formula = cf46 ~ belowpovl, design = rchis, family = quasibinomial)
## 
## Survey design:
## svrepdesign.default(chis[, -(212 + (1:80))], repweights = chis[, 
##     (212 + (1:80))], weights = chis$rakedw0, combined.weights = TRUE, 
##     scale = 1, rscales = rep(1, 80), type = "other")
## 
## Coefficients:
##                         Estimate Std. Error t value Pr(>|t|)    
## (Intercept)              -1.9602     0.0992   -19.8   <2e-16 ***
## belowpovlBelow 100% FPL  -0.3237     0.2165    -1.5     0.14    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for quasibinomial family taken to be 0.8668)
## 
## Number of Fisher Scoring iterations: 5
```

```r
summary(model2)
```

```
## 
## Call:
## svyglm(formula = cf47 ~ belowpovl, design = rchis, family = quasibinomial)
## 
## Survey design:
## svrepdesign.default(chis[, -(212 + (1:80))], repweights = chis[, 
##     (212 + (1:80))], weights = chis$rakedw0, combined.weights = TRUE, 
##     scale = 1, rscales = rep(1, 80), type = "other")
## 
## Coefficients:
##                         Estimate Std. Error t value Pr(>|t|)    
## (Intercept)              -1.7726     0.0871  -20.36   <2e-16 ***
## belowpovlBelow 100% FPL   0.1216     0.2313    0.53      0.6    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for quasibinomial family taken to be 0.8668)
## 
## Number of Fisher Scoring iterations: 5
```

```r
summary(model3)
```

```
## 
## Call:
## svyglm(formula = referred ~ belowpovl, design = rchis, family = quasibinomial)
## 
## Survey design:
## svrepdesign.default(chis[, -(212 + (1:80))], repweights = chis[, 
##     (212 + (1:80))], weights = chis$rakedw0, combined.weights = TRUE, 
##     scale = 1, rscales = rep(1, 80), type = "other")
## 
## Coefficients:
##                         Estimate Std. Error t value Pr(>|t|)    
## (Intercept)              -1.4369     0.0734  -19.56   <2e-16 ***
## belowpovlBelow 100% FPL   0.0439     0.1976    0.22     0.82    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for quasibinomial family taken to be 0.8668)
## 
## Number of Fisher Scoring iterations: 4
```

```r

cbind(OddsRatio = exp(model1$coef), exp(confint(model1)))
```

```
##                         OddsRatio  2.5 % 97.5 %
## (Intercept)                0.1408 0.1159 0.1711
## belowpovlBelow 100% FPL    0.7234 0.4732 1.1059
```

```r
cbind(OddsRatio = exp(model2$coef), exp(confint(model2)))
```

```
##                         OddsRatio  2.5 % 97.5 %
## (Intercept)                0.1699 0.1432 0.2015
## belowpovlBelow 100% FPL    1.1293 0.7177 1.7772
```

```r
cbind(OddsRatio = exp(model3$coef), exp(confint(model3)))
```

```
##                         OddsRatio  2.5 % 97.5 %
## (Intercept)                0.2377 0.2058 0.2744
## belowpovlBelow 100% FPL    1.0449 0.7093 1.5392
```


### Uninsured Ever in Last Year

```r
model1 <- svyglm(cf46 ~ unins.ever, design = rchis, family = quasibinomial)
model2 <- svyglm(cf47 ~ unins.ever, design = rchis, family = quasibinomial)
model3 <- svyglm(referred ~ unins.ever, design = rchis, family = quasibinomial)

summary(model1)
```

```
## 
## Call:
## svyglm(formula = cf46 ~ unins.ever, design = rchis, family = quasibinomial)
## 
## Survey design:
## svrepdesign.default(chis[, -(212 + (1:80))], repweights = chis[, 
##     (212 + (1:80))], weights = chis$rakedw0, combined.weights = TRUE, 
##     scale = 1, rscales = rep(1, 80), type = "other")
## 
## Coefficients:
##                                      Estimate Std. Error t value Pr(>|t|)
## (Intercept)                           -1.9991     0.0833   -24.0   <2e-16
## unins.everSome or All Year uninsured  -0.6396     0.4906    -1.3      0.2
##                                         
## (Intercept)                          ***
## unins.everSome or All Year uninsured    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for quasibinomial family taken to be 0.8668)
## 
## Number of Fisher Scoring iterations: 5
```

```r
summary(model2)
```

```
## 
## Call:
## svyglm(formula = cf47 ~ unins.ever, design = rchis, family = quasibinomial)
## 
## Survey design:
## svrepdesign.default(chis[, -(212 + (1:80))], repweights = chis[, 
##     (212 + (1:80))], weights = chis$rakedw0, combined.weights = TRUE, 
##     scale = 1, rscales = rep(1, 80), type = "other")
## 
## Coefficients:
##                                      Estimate Std. Error t value Pr(>|t|)
## (Intercept)                            -1.701      0.091  -18.70   <2e-16
## unins.everSome or All Year uninsured   -0.524      0.370   -1.42     0.16
##                                         
## (Intercept)                          ***
## unins.everSome or All Year uninsured    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for quasibinomial family taken to be 0.8668)
## 
## Number of Fisher Scoring iterations: 5
```

```r
summary(model3)
```

```
## 
## Call:
## svyglm(formula = referred ~ unins.ever, design = rchis, family = quasibinomial)
## 
## Survey design:
## svrepdesign.default(chis[, -(212 + (1:80))], repweights = chis[, 
##     (212 + (1:80))], weights = chis$rakedw0, combined.weights = TRUE, 
##     scale = 1, rscales = rep(1, 80), type = "other")
## 
## Coefficients:
##                                      Estimate Std. Error t value Pr(>|t|)
## (Intercept)                           -1.3808     0.0758  -18.22   <2e-16
## unins.everSome or All Year uninsured  -0.6321     0.3273   -1.93    0.057
##                                         
## (Intercept)                          ***
## unins.everSome or All Year uninsured .  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for quasibinomial family taken to be 0.8668)
## 
## Number of Fisher Scoring iterations: 5
```

```r

cbind(OddsRatio = exp(model1$coef), exp(confint(model1)))
```

```
##                                      OddsRatio  2.5 % 97.5 %
## (Intercept)                             0.1355 0.1151 0.1595
## unins.everSome or All Year uninsured    0.5275 0.2017 1.3798
```

```r
cbind(OddsRatio = exp(model2$coef), exp(confint(model2)))
```

```
##                                      OddsRatio  2.5 % 97.5 %
## (Intercept)                             0.1825 0.1527 0.2181
## unins.everSome or All Year uninsured    0.5924 0.2870 1.2224
```

```r
cbind(OddsRatio = exp(model3$coef), exp(confint(model3)))
```

```
##                                      OddsRatio  2.5 % 97.5 %
## (Intercept)                             0.2514 0.2167 0.2916
## unins.everSome or All Year uninsured    0.5315 0.2798 1.0094
```


### PEDS High Risk

```r
model1 <- svyglm(cf46 ~ pedsHiRisk, design = rchis, family = quasibinomial)
model2 <- svyglm(cf47 ~ pedsHiRisk, design = rchis, family = quasibinomial)
model3 <- svyglm(referred ~ pedsHiRisk, design = rchis, family = quasibinomial)

summary(model1)
```

```
## 
## Call:
## svyglm(formula = cf46 ~ pedsHiRisk, design = rchis, family = quasibinomial)
## 
## Survey design:
## svrepdesign.default(chis[, -(212 + (1:80))], repweights = chis[, 
##     (212 + (1:80))], weights = chis$rakedw0, combined.weights = TRUE, 
##     scale = 1, rscales = rep(1, 80), type = "other")
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)     -2.527      0.104  -24.26  < 2e-16 ***
## pedsHiRiskYes    1.518      0.201    7.56  6.6e-11 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for quasibinomial family taken to be 0.8668)
## 
## Number of Fisher Scoring iterations: 5
```

```r
summary(model2)
```

```
## 
## Call:
## svyglm(formula = cf47 ~ pedsHiRisk, design = rchis, family = quasibinomial)
## 
## Survey design:
## svrepdesign.default(chis[, -(212 + (1:80))], repweights = chis[, 
##     (212 + (1:80))], weights = chis$rakedw0, combined.weights = TRUE, 
##     scale = 1, rscales = rep(1, 80), type = "other")
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)     -2.158      0.132  -16.41  < 2e-16 ***
## pedsHiRiskYes    1.409      0.184    7.66  4.3e-11 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for quasibinomial family taken to be 0.8668)
## 
## Number of Fisher Scoring iterations: 5
```

```r
summary(model3)
```

```
## 
## Call:
## svyglm(formula = referred ~ pedsHiRisk, design = rchis, family = quasibinomial)
## 
## Survey design:
## svrepdesign.default(chis[, -(212 + (1:80))], repweights = chis[, 
##     (212 + (1:80))], weights = chis$rakedw0, combined.weights = TRUE, 
##     scale = 1, rscales = rep(1, 80), type = "other")
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)     -1.807      0.104  -17.42   <2e-16 ***
## pedsHiRiskYes    1.365      0.165    8.25    3e-12 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for quasibinomial family taken to be 0.8668)
## 
## Number of Fisher Scoring iterations: 5
```

```r

cbind(OddsRatio = exp(model1$coef), exp(confint(model1)))
```

```
##               OddsRatio   2.5 %  97.5 %
## (Intercept)     0.07992 0.06516 0.09802
## pedsHiRiskYes   4.56448 3.07975 6.76497
```

```r
cbind(OddsRatio = exp(model2$coef), exp(confint(model2)))
```

```
##               OddsRatio   2.5 % 97.5 %
## (Intercept)      0.1155 0.08929 0.1495
## pedsHiRiskYes    4.0912 2.85257 5.8676
```

```r
cbind(OddsRatio = exp(model3$coef), exp(confint(model3)))
```

```
##               OddsRatio  2.5 % 97.5 %
## (Intercept)      0.1641 0.1339 0.2011
## pedsHiRiskYes    3.9171 2.8325 5.4170
```



Table 3: Regression Analyses
-----


### GLM with univariates from univariates_referral_devo.R

```r
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
```

```
## 
## Call:
## svyglm(formula = cf46 ~ pedsHiRisk + male + srage.p + belowpovl + 
##     unins.ever + racehp2p + srh.a.i + brthwk.p.i, design = rchis, 
##     family = quasibinomial)
## 
## Survey design:
## svrepdesign.default(chis[, -(212 + (1:80))], repweights = chis[, 
##     (212 + (1:80))], weights = chis$rakedw0, combined.weights = TRUE, 
##     scale = 1, rscales = rep(1, 80), type = "other")
## 
## Coefficients:
##                                       Estimate Std. Error t value Pr(>|t|)
## (Intercept)                           -1.63820    0.92395   -1.77   0.0807
## pedsHiRiskYes                          1.58506    0.20002    7.92    3e-11
## maleMale                               0.26932    0.17661    1.52   0.1319
## srage.p                                0.09287    0.05807    1.60   0.1144
## belowpovlBelow 100% FPL               -0.42426    0.27875   -1.52   0.1326
## unins.everSome or All Year uninsured  -0.86265    0.55434   -1.56   0.1243
## racehp2pLATINO                         0.17088    0.53311    0.32   0.7495
## racehp2pASIAN                         -0.39354    0.24592   -1.60   0.1142
## racehp2pAFRICAN AMERICAN               0.00179    0.30496    0.01   0.9953
## racehp2pPI/OTHER SINGLE/MULTIPLE RACE  0.20741    0.64629    0.32   0.7493
## srh.a.iNO                              0.42637    0.52146    0.82   0.4164
## brthwk.p.i                            -0.44152    0.16353   -2.70   0.0087
##                                          
## (Intercept)                           .  
## pedsHiRiskYes                         ***
## maleMale                                 
## srage.p                                  
## belowpovlBelow 100% FPL                  
## unins.everSome or All Year uninsured     
## racehp2pLATINO                           
## racehp2pASIAN                            
## racehp2pAFRICAN AMERICAN                 
## racehp2pPI/OTHER SINGLE/MULTIPLE RACE    
## srh.a.iNO                                
## brthwk.p.i                            ** 
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for quasibinomial family taken to be 0.8949)
## 
## Number of Fisher Scoring iterations: 6
```

```r
cbind(OddsRatio = exp(model2$coef), exp(confint(model2)))
```

```
##                                       OddsRatio   2.5 % 97.5 %
## (Intercept)                              0.1943 0.03177  1.189
## pedsHiRiskYes                            4.8796 3.29708  7.222
## maleMale                                 1.3091 0.92604  1.851
## srage.p                                  1.0973 0.97927  1.230
## belowpovlBelow 100% FPL                  0.6543 0.37885  1.130
## unins.everSome or All Year uninsured     0.4220 0.14240  1.251
## racehp2pLATINO                           1.1864 0.41729  3.373
## racehp2pASIAN                            0.6747 0.41664  1.092
## racehp2pAFRICAN AMERICAN                 1.0018 0.55106  1.821
## racehp2pPI/OTHER SINGLE/MULTIPLE RACE    1.2305 0.34670  4.367
## srh.a.iNO                                1.5317 0.55119  4.256
## brthwk.p.i                               0.6431 0.46671  0.886
```


### GLM with univariates from univariates_referral_speech-only.R


```r
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
```

```
## 
## Call:
## svyglm(formula = cf47 ~ pedsHiRisk + male + srage.p + belowpovl + 
##     unins.ever + racehp2p + srh.a.i + brthwk.p.i, design = rchis, 
##     family = quasibinomial)
## 
## Survey design:
## svrepdesign.default(chis[, -(212 + (1:80))], repweights = chis[, 
##     (212 + (1:80))], weights = chis$rakedw0, combined.weights = TRUE, 
##     scale = 1, rscales = rep(1, 80), type = "other")
## 
## Coefficients:
##                                       Estimate Std. Error t value Pr(>|t|)
## (Intercept)                           -1.85625    0.71570   -2.59  0.01162
## pedsHiRiskYes                          1.43167    0.19905    7.19  6.4e-10
## maleMale                               0.23864    0.18193    1.31  0.19402
## srage.p                                0.22982    0.05737    4.01  0.00016
## belowpovlBelow 100% FPL                0.11256    0.25428    0.44  0.65941
## unins.everSome or All Year uninsured  -0.85480    0.44788   -1.91  0.06055
## racehp2pLATINO                        -0.00933    0.41806   -0.02  0.98226
## racehp2pASIAN                         -0.50884    0.24255   -2.10  0.03964
## racehp2pAFRICAN AMERICAN              -0.09796    0.35725   -0.27  0.78476
## racehp2pPI/OTHER SINGLE/MULTIPLE RACE -0.07511    0.50863   -0.15  0.88304
## srh.a.iNO                              0.25915    0.38273    0.68  0.50062
## brthwk.p.i                            -0.36054    0.13792   -2.61  0.01100
##                                          
## (Intercept)                           *  
## pedsHiRiskYes                         ***
## maleMale                                 
## srage.p                               ***
## belowpovlBelow 100% FPL                  
## unins.everSome or All Year uninsured  .  
## racehp2pLATINO                           
## racehp2pASIAN                         *  
## racehp2pAFRICAN AMERICAN                 
## racehp2pPI/OTHER SINGLE/MULTIPLE RACE    
## srh.a.iNO                                
## brthwk.p.i                            *  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for quasibinomial family taken to be 0.8763)
## 
## Number of Fisher Scoring iterations: 5
```

```r
cbind(OddsRatio = exp(model3$coef), exp(confint(model3)))
```

```
##                                       OddsRatio   2.5 % 97.5 %
## (Intercept)                              0.1563 0.03843 0.6354
## pedsHiRiskYes                            4.1857 2.83360 6.1830
## maleMale                                 1.2695 0.88876 1.8134
## srage.p                                  1.2584 1.12454 1.4081
## belowpovlBelow 100% FPL                  1.1191 0.67990 1.8421
## unins.everSome or All Year uninsured     0.4254 0.17682 1.0233
## racehp2pLATINO                           0.9907 0.43661 2.2480
## racehp2pASIAN                            0.6012 0.37373 0.9671
## racehp2pAFRICAN AMERICAN                 0.9067 0.45016 1.8262
## racehp2pPI/OTHER SINGLE/MULTIPLE RACE    0.9276 0.34232 2.5138
## srh.a.iNO                                1.2958 0.61203 2.7436
## brthwk.p.i                               0.6973 0.53214 0.9137
```


### GLM with univariates from univariates.R


```r
model1 <- svyglm(referred ~ pedsHiRisk
                # + peds
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
summary(model1)
```

```
## 
## Call:
## svyglm(formula = referred ~ pedsHiRisk + male + srage.p + belowpovl + 
##     unins.ever + racehp2p + srh.a.i + brthwk.p.i, design = rchis, 
##     family = quasibinomial)
## 
## Survey design:
## svrepdesign.default(chis[, -(212 + (1:80))], repweights = chis[, 
##     (212 + (1:80))], weights = chis$rakedw0, combined.weights = TRUE, 
##     scale = 1, rscales = rep(1, 80), type = "other")
## 
## Coefficients:
##                                       Estimate Std. Error t value Pr(>|t|)
## (Intercept)                           -0.77940    0.61203   -1.27  0.20719
## pedsHiRiskYes                          1.41617    0.18262    7.75  6.1e-11
## maleMale                               0.16198    0.16648    0.97  0.33399
## srage.p                                0.13883    0.04965    2.80  0.00672
## belowpovlBelow 100% FPL                0.00756    0.23493    0.03  0.97444
## unins.everSome or All Year uninsured  -0.93476    0.38359   -2.44  0.01744
## racehp2pLATINO                        -0.03658    0.34321   -0.11  0.91543
## racehp2pASIAN                         -0.40739    0.22362   -1.82  0.07289
## racehp2pAFRICAN AMERICAN              -0.13768    0.30656   -0.45  0.65477
## racehp2pPI/OTHER SINGLE/MULTIPLE RACE -0.07081    0.40710   -0.17  0.86243
## srh.a.iNO                              0.20398    0.31128    0.66  0.51449
## brthwk.p.i                            -0.46339    0.12642   -3.67  0.00048
##                                          
## (Intercept)                              
## pedsHiRiskYes                         ***
## maleMale                                 
## srage.p                               ** 
## belowpovlBelow 100% FPL                  
## unins.everSome or All Year uninsured  *  
## racehp2pLATINO                           
## racehp2pASIAN                         .  
## racehp2pAFRICAN AMERICAN                 
## racehp2pPI/OTHER SINGLE/MULTIPLE RACE    
## srh.a.iNO                                
## brthwk.p.i                            ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for quasibinomial family taken to be 0.8734)
## 
## Number of Fisher Scoring iterations: 5
```

```r
cbind(OddsRatio = exp(model1$coef), exp(confint(model1)))
```

```
##                                       OddsRatio  2.5 % 97.5 %
## (Intercept)                              0.4587 0.1382 1.5222
## pedsHiRiskYes                            4.1213 2.8813 5.8949
## maleMale                                 1.1758 0.8485 1.6295
## srage.p                                  1.1489 1.0424 1.2664
## belowpovlBelow 100% FPL                  1.0076 0.6358 1.5968
## unins.everSome or All Year uninsured     0.3927 0.1852 0.8328
## racehp2pLATINO                           0.9641 0.4920 1.8891
## racehp2pASIAN                            0.6654 0.4293 1.0314
## racehp2pAFRICAN AMERICAN                 0.8714 0.4778 1.5891
## racehp2pPI/OTHER SINGLE/MULTIPLE RACE    0.9316 0.4195 2.0691
## srh.a.iNO                                1.2263 0.6662 2.2571
## brthwk.p.i                               0.6291 0.4911 0.8060
```



Add interaction terms
=====

### GLM with univariates from univariates_referral_devo.R

```r
model2int <- svyglm(cf46 ~ pedsHiRisk 
                 + male 
                 #                + cd1.2 
                 + srage.p 
                 + belowpovl 
                 + unins.ever 
                 + racehp2p 
                 # + white
                 + srh.a.i
                 + brthwk.p.i
                 + pedsHiRisk*srage.p
                 + pedsHiRisk*brthwk.p.i
#                 + pedsHiRisk*unins.ever
                 ,
                 design = rchis, family = quasibinomial)
summary(model2int)
```

```
## 
## Call:
## svyglm(formula = cf46 ~ pedsHiRisk + male + srage.p + belowpovl + 
##     unins.ever + racehp2p + srh.a.i + brthwk.p.i + pedsHiRisk * 
##     srage.p + pedsHiRisk * brthwk.p.i, design = rchis, family = quasibinomial)
## 
## Survey design:
## svrepdesign.default(chis[, -(212 + (1:80))], repweights = chis[, 
##     (212 + (1:80))], weights = chis$rakedw0, combined.weights = TRUE, 
##     scale = 1, rscales = rep(1, 80), type = "other")
## 
## Coefficients:
##                                       Estimate Std. Error t value Pr(>|t|)
## (Intercept)                            -0.0133     0.9157   -0.01  0.98843
## pedsHiRiskYes                          -1.4110     1.1280   -1.25  0.21539
## maleMale                                0.2350     0.1744    1.35  0.18254
## srage.p                                -0.0672     0.0662   -1.01  0.31396
## belowpovlBelow 100% FPL                -0.4334     0.2808   -1.54  0.12756
## unins.everSome or All Year uninsured   -0.8963     0.5578   -1.61  0.11285
## racehp2pLATINO                          0.0177     0.4546    0.04  0.96912
## racehp2pASIAN                          -0.3855     0.2478   -1.56  0.12458
## racehp2pAFRICAN AMERICAN               -0.0438     0.3164   -0.14  0.89037
## racehp2pPI/OTHER SINGLE/MULTIPLE RACE   0.0650     0.5527    0.12  0.90680
## srh.a.iNO                               0.3018     0.4479    0.67  0.50276
## brthwk.p.i                             -0.7603     0.2180   -3.49  0.00087
## pedsHiRiskYes:srage.p                   0.3919     0.1249    3.14  0.00255
## pedsHiRiskYes:brthwk.p.i                0.5580     0.3191    1.75  0.08496
##                                          
## (Intercept)                              
## pedsHiRiskYes                            
## maleMale                                 
## srage.p                                  
## belowpovlBelow 100% FPL                  
## unins.everSome or All Year uninsured     
## racehp2pLATINO                           
## racehp2pASIAN                            
## racehp2pAFRICAN AMERICAN                 
## racehp2pPI/OTHER SINGLE/MULTIPLE RACE    
## srh.a.iNO                                
## brthwk.p.i                            ***
## pedsHiRiskYes:srage.p                 ** 
## pedsHiRiskYes:brthwk.p.i              .  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for quasibinomial family taken to be 0.8992)
## 
## Number of Fisher Scoring iterations: 6
```

```r
cbind(OddsRatio = exp(model2int$coef), exp(confint(model2int)))
```

```
##                                       OddsRatio   2.5 % 97.5 %
## (Intercept)                              0.9868 0.16396 5.9386
## pedsHiRiskYes                            0.2439 0.02673 2.2252
## maleMale                                 1.2649 0.89863 1.7803
## srage.p                                  0.9350 0.82127 1.0646
## belowpovlBelow 100% FPL                  0.6483 0.37391 1.1241
## unins.everSome or All Year uninsured     0.4081 0.13675 1.2177
## racehp2pLATINO                           1.0178 0.41754 2.4811
## racehp2pASIAN                            0.6801 0.41841 1.1054
## racehp2pAFRICAN AMERICAN                 0.9572 0.51486 1.7794
## racehp2pPI/OTHER SINGLE/MULTIPLE RACE    1.0671 0.36122 3.1524
## srh.a.iNO                                1.3523 0.56211 3.2535
## brthwk.p.i                               0.4675 0.30495 0.7168
## pedsHiRiskYes:srage.p                    1.4798 1.15839 1.8903
## pedsHiRiskYes:brthwk.p.i                 1.7472 0.93487 3.2653
```


### GLM with univariates from univariates_referral_speech-only.R


```r
model3int <- svyglm(cf47 ~ pedsHiRisk 
                 + male 
                 #                 + cd1.2 
                 + srage.p 
                 + belowpovl 
                 + unins.ever 
                 + racehp2p 
                 # + white
                 + srh.a.i
                 + brthwk.p.i
                 + pedsHiRisk*srage.p
                 + pedsHiRisk*brthwk.p.i
#                 + pedsHiRisk*unins.ever
                 ,
                 design = rchis, family = quasibinomial)
summary(model3int)
```

```
## 
## Call:
## svyglm(formula = cf47 ~ pedsHiRisk + male + srage.p + belowpovl + 
##     unins.ever + racehp2p + srh.a.i + brthwk.p.i + pedsHiRisk * 
##     srage.p + pedsHiRisk * brthwk.p.i, design = rchis, family = quasibinomial)
## 
## Survey design:
## svrepdesign.default(chis[, -(212 + (1:80))], repweights = chis[, 
##     (212 + (1:80))], weights = chis$rakedw0, combined.weights = TRUE, 
##     scale = 1, rscales = rep(1, 80), type = "other")
## 
## Coefficients:
##                                       Estimate Std. Error t value Pr(>|t|)
## (Intercept)                            -0.6110     0.7344   -0.83  0.40844
## pedsHiRiskYes                          -1.0260     1.0557   -0.97  0.33466
## maleMale                                0.2275     0.1837    1.24  0.21991
## srage.p                                 0.1642     0.0761    2.16  0.03452
## belowpovlBelow 100% FPL                 0.1187     0.2552    0.47  0.64333
## unins.everSome or All Year uninsured   -0.8445     0.4491   -1.88  0.06446
## racehp2pLATINO                         -0.1157     0.3650   -0.32  0.75230
## racehp2pASIAN                          -0.5043     0.2426   -2.08  0.04150
## racehp2pAFRICAN AMERICAN               -0.1409     0.3668   -0.38  0.70213
## racehp2pPI/OTHER SINGLE/MULTIPLE RACE  -0.1940     0.4391   -0.44  0.66002
## srh.a.iNO                               0.1601     0.3287    0.49  0.62781
## brthwk.p.i                             -0.6507     0.1885   -3.45  0.00097
## pedsHiRiskYes:srage.p                   0.1723     0.1266    1.36  0.17827
## pedsHiRiskYes:brthwk.p.i                0.5849     0.2808    2.08  0.04114
##                                          
## (Intercept)                              
## pedsHiRiskYes                            
## maleMale                                 
## srage.p                               *  
## belowpovlBelow 100% FPL                  
## unins.everSome or All Year uninsured  .  
## racehp2pLATINO                           
## racehp2pASIAN                         *  
## racehp2pAFRICAN AMERICAN                 
## racehp2pPI/OTHER SINGLE/MULTIPLE RACE    
## srh.a.iNO                                
## brthwk.p.i                            ***
## pedsHiRiskYes:srage.p                    
## pedsHiRiskYes:brthwk.p.i              *  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for quasibinomial family taken to be 0.8745)
## 
## Number of Fisher Scoring iterations: 5
```

```r
cbind(OddsRatio = exp(model3int$coef), exp(confint(model3int)))
```

```
##                                       OddsRatio   2.5 % 97.5 %
## (Intercept)                              0.5428 0.12869 2.2896
## pedsHiRiskYes                            0.3584 0.04527 2.8381
## maleMale                                 1.2554 0.87589 1.7995
## srage.p                                  1.1784 1.01522 1.3678
## belowpovlBelow 100% FPL                  1.1261 0.68281 1.8571
## unins.everSome or All Year uninsured     0.4298 0.17822 1.0363
## racehp2pLATINO                           0.8908 0.43557 1.8216
## racehp2pASIAN                            0.6039 0.37539 0.9715
## racehp2pAFRICAN AMERICAN                 0.8686 0.42326 1.7825
## racehp2pPI/OTHER SINGLE/MULTIPLE RACE    0.8236 0.34832 1.9476
## srh.a.iNO                                1.1736 0.61623 2.2353
## brthwk.p.i                               0.5217 0.36058 0.7548
## pedsHiRiskYes:srage.p                    1.1880 0.92692 1.5227
## pedsHiRiskYes:brthwk.p.i                 1.7948 1.03511 3.1121
```


### GLM with univariates from univariates.R


```r
model1int <- svyglm(referred ~ pedsHiRisk
                # + peds
                + male 
                #                + cd1.2 
                + srage.p 
                + belowpovl 
                + unins.ever 
                + racehp2p 
                #               + white
                + srh.a.i
                + brthwk.p.i
                + pedsHiRisk*srage.p
                + pedsHiRisk*brthwk.p.i
                #                + pedsHiRisk*unins.ever
                ,
                design = rchis, family = quasibinomial)
summary(model1int)
```

```
## 
## Call:
## svyglm(formula = referred ~ pedsHiRisk + male + srage.p + belowpovl + 
##     unins.ever + racehp2p + srh.a.i + brthwk.p.i + pedsHiRisk * 
##     srage.p + pedsHiRisk * brthwk.p.i, design = rchis, family = quasibinomial)
## 
## Survey design:
## svrepdesign.default(chis[, -(212 + (1:80))], repweights = chis[, 
##     (212 + (1:80))], weights = chis$rakedw0, combined.weights = TRUE, 
##     scale = 1, rscales = rep(1, 80), type = "other")
## 
## Coefficients:
##                                       Estimate Std. Error t value Pr(>|t|)
## (Intercept)                            0.27345    0.61402    0.45    0.658
## pedsHiRiskYes                         -0.91477    0.86254   -1.06    0.293
## maleMale                               0.13706    0.16566    0.83    0.411
## srage.p                                0.04402    0.06457    0.68    0.498
## belowpovlBelow 100% FPL                0.00514    0.23719    0.02    0.983
## unins.everSome or All Year uninsured  -0.96338    0.38900   -2.48    0.016
## racehp2pLATINO                        -0.14064    0.30194   -0.47    0.643
## racehp2pASIAN                         -0.39566    0.22351   -1.77    0.081
## racehp2pAFRICAN AMERICAN              -0.17463    0.31904   -0.55    0.586
## racehp2pPI/OTHER SINGLE/MULTIPLE RACE -0.17288    0.34889   -0.50    0.622
## srh.a.iNO                              0.11786    0.27066    0.44    0.665
## brthwk.p.i                            -0.67088    0.15643   -4.29    6e-05
## pedsHiRiskYes:srage.p                  0.28957    0.11823    2.45    0.017
## pedsHiRiskYes:brthwk.p.i               0.44239    0.22875    1.93    0.057
##                                          
## (Intercept)                              
## pedsHiRiskYes                            
## maleMale                                 
## srage.p                                  
## belowpovlBelow 100% FPL                  
## unins.everSome or All Year uninsured  *  
## racehp2pLATINO                           
## racehp2pASIAN                         .  
## racehp2pAFRICAN AMERICAN                 
## racehp2pPI/OTHER SINGLE/MULTIPLE RACE    
## srh.a.iNO                                
## brthwk.p.i                            ***
## pedsHiRiskYes:srage.p                 *  
## pedsHiRiskYes:brthwk.p.i              .  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for quasibinomial family taken to be 0.8757)
## 
## Number of Fisher Scoring iterations: 5
```

```r
cbind(OddsRatio = exp(model1int$coef), exp(confint(model1int)))
```

```
##                                       OddsRatio   2.5 % 97.5 %
## (Intercept)                              1.3145 0.39455 4.3794
## pedsHiRiskYes                            0.4006 0.07388 2.1723
## maleMale                                 1.1469 0.82892 1.5868
## srage.p                                  1.0450 0.92079 1.1860
## belowpovlBelow 100% FPL                  1.0052 0.63145 1.6000
## unins.everSome or All Year uninsured     0.3816 0.17803 0.8180
## racehp2pLATINO                           0.8688 0.48074 1.5701
## racehp2pASIAN                            0.6732 0.43443 1.0433
## racehp2pAFRICAN AMERICAN                 0.8398 0.44936 1.5694
## racehp2pPI/OTHER SINGLE/MULTIPLE RACE    0.8412 0.42456 1.6669
## srh.a.iNO                                1.1251 0.66191 1.9124
## brthwk.p.i                               0.5113 0.37626 0.6947
## pedsHiRiskYes:srage.p                    1.3359 1.05956 1.6842
## pedsHiRiskYes:brthwk.p.i                 1.5564 0.99406 2.4369
```


Using AIC to compare models does not work because it svrepglm is not fitted by maximum likelihood. See: https://stat.ethz.ch/pipermail/r-help/2012-July/319508.html


```r
regTermTest(model2int, test.terms = ~ pedsHiRisk * srage.p 
            + pedsHiRisk * brthwk.p.i 
#            + pedsHiRisk * unins.ever
            )
```

```
## Wald test for pedsHiRisk srage.p brthwk.p.i pedsHiRisk:srage.p pedsHiRisk:brthwk.p.i
##  in svyglm(formula = cf46 ~ pedsHiRisk + male + srage.p + belowpovl + 
##     unins.ever + racehp2p + srh.a.i + brthwk.p.i + pedsHiRisk * 
##     srage.p + pedsHiRisk * brthwk.p.i, design = rchis, family = quasibinomial)
## F =  23.82  on  5  and  66  df: p= 1.3e-13
```

```r

regTermTest(model3int, test.terms = ~ pedsHiRisk * srage.p 
            + pedsHiRisk * brthwk.p.i 
#            + pedsHiRisk * unins.ever
            )
```

```
## Wald test for pedsHiRisk srage.p brthwk.p.i pedsHiRisk:srage.p pedsHiRisk:brthwk.p.i
##  in svyglm(formula = cf47 ~ pedsHiRisk + male + srage.p + belowpovl + 
##     unins.ever + racehp2p + srh.a.i + brthwk.p.i + pedsHiRisk * 
##     srage.p + pedsHiRisk * brthwk.p.i, design = rchis, family = quasibinomial)
## F =  13.54  on  5  and  66  df: p= 4.3e-09
```

```r

regTermTest(model1int, test.terms = ~ pedsHiRisk * srage.p 
            + pedsHiRisk * brthwk.p.i 
#            + pedsHiRisk * unins.ever
            )
```

```
## Wald test for pedsHiRisk srage.p brthwk.p.i pedsHiRisk:srage.p pedsHiRisk:brthwk.p.i
##  in svyglm(formula = referred ~ pedsHiRisk + male + srage.p + belowpovl + 
##     unins.ever + racehp2p + srh.a.i + brthwk.p.i + pedsHiRisk * 
##     srage.p + pedsHiRisk * brthwk.p.i, design = rchis, family = quasibinomial)
## F =  18.77  on  5  and  66  df: p= 1.5e-11
```



Looking at the interaction terms, there is a clear interaction between PEDS risk and birthweight or age. This kind of makes sense for birthweight. If you are low birthweight, you are likely to be referred for services regardless of PEDS result. In addition, perhaps the effect of PEDS is less strong on referral when the children are younger. To illustrate, we should try to subdivide the population by age and birthweight.

#### Age
We can see that the rate of referral in the low risk PEDS group is very low in the 0 age group and ranges about 13-16% after that. In contrast, the rate of referral in the high risk PEDS group is higher in older children than in younger children. 


```r
by.age.all <- svyby(~referred, by = ~srage.p + pedsHiRisk, FUN = svymean, design = rchis, 
    na.rm = T)
by.age.devo <- svyby(~cf46, by = ~srage.p + pedsHiRisk, FUN = svymean, design = rchis, 
    na.rm = T)
by.age.speech <- svyby(~cf47, by = ~srage.p + pedsHiRisk, FUN = svymean, design = rchis, 
    na.rm = T)

by.age.all
```

```
##       srage.p pedsHiRisk referredNo referredYes     se1     se2
## 0.No        0         No     0.9565     0.04347 0.02799 0.02799
## 1.No        1         No     0.8720     0.12796 0.02363 0.02363
## 2.No        2         No     0.8399     0.16013 0.04243 0.04243
## 3.No        3         No     0.8374     0.16257 0.02115 0.02115
## 4.No        4         No     0.8588     0.14121 0.02952 0.02952
## 5.No        5         No     0.8684     0.13161 0.01594 0.01594
## 0.Yes       0        Yes     0.7341     0.26592 0.15827 0.15827
## 1.Yes       1        Yes     0.7974     0.20263 0.06094 0.06094
## 2.Yes       2        Yes     0.6290     0.37100 0.05275 0.05275
## 3.Yes       3        Yes     0.6172     0.38279 0.06358 0.06358
## 4.Yes       4        Yes     0.5462     0.45380 0.06614 0.06614
## 5.Yes       5        Yes     0.4880     0.51205 0.07201 0.07201
```

```r
by.age.devo
```

```
##       srage.p pedsHiRisk cf46Not referred cf46Referred      se1      se2
## 0.No        0         No           0.9765      0.02348 0.017292 0.017292
## 1.No        1         No           0.9166      0.08337 0.021399 0.021399
## 2.No        2         No           0.9122      0.08778 0.018571 0.018571
## 3.No        3         No           0.9045      0.09547 0.018370 0.018370
## 4.No        4         No           0.9463      0.05369 0.009693 0.009693
## 5.No        5         No           0.9398      0.06022 0.010893 0.010893
## 0.Yes       0        Yes           0.8922      0.10779 0.074512 0.074512
## 1.Yes       1        Yes           0.9175      0.08251 0.027314 0.027314
## 2.Yes       2        Yes           0.6875      0.31251 0.054032 0.054032
## 3.Yes       3        Yes           0.7472      0.25276 0.054498 0.054498
## 4.Yes       4        Yes           0.7112      0.28879 0.050787 0.050787
## 5.Yes       5        Yes           0.6269      0.37311 0.080563 0.080563
```

```r
by.age.speech
```

```
##       srage.p pedsHiRisk cf47Not referred cf47Referred     se1     se2
## 0.No        0         No           0.9800      0.02000 0.01341 0.01341
## 1.No        1         No           0.9330      0.06703 0.01828 0.01828
## 2.No        2         No           0.8908      0.10916 0.03952 0.03952
## 3.No        3         No           0.8750      0.12500 0.01826 0.01826
## 4.No        4         No           0.8749      0.12514 0.02917 0.02917
## 5.No        5         No           0.8901      0.10988 0.01591 0.01591
## 0.Yes       0        Yes           0.7874      0.21258 0.17141 0.17141
## 1.Yes       1        Yes           0.8428      0.15715 0.05438 0.05438
## 2.Yes       2        Yes           0.7166      0.28339 0.04633 0.04633
## 3.Yes       3        Yes           0.6984      0.30155 0.06015 0.06015
## 4.Yes       4        Yes           0.6250      0.37503 0.06051 0.06051
## 5.Yes       5        Yes           0.5465      0.45348 0.07544 0.07544
```


What is the RR of referral at different ages?

```r
for (i in 0:5) print(by.age.all[i + 7, 4]/by.age.all[i + 1, 4])
```

```
## [1] 6.117
## [1] 1.583
## [1] 2.317
## [1] 2.355
## [1] 3.214
## [1] 3.891
```


# Figures

### Devo by PEDS and Age

```r
ggplot(data = by.age.devo, aes(x = srage.p, y = cf46Referred, fill = pedsHiRisk)) + 
  geom_bar(stat="identity", position=position_dodge(), colour="black") +
#  geom_smooth(method = "lm") + 
#  scale_x_discrete(breaks=seq(0, 10, 2), labels=0:5) +
  scale_y_continuous(labels=percent) +
  scale_fill_brewer(name = "PEDS High Risk") + 
#  scale_fill_grey(name = "PEDS High Risk") +
  xlab("Age (years)") +
  ylab("Percentage Referred") +
  theme_bw(base_family = "Times") +
  ggtitle("Percentage Referred to Developmental Specialist by PEDS and Age") + 
  geom_errorbar(aes(ymin=cf46Referred - se2, ymax = cf46Referred + se2),
                  width=.2,                    # Width of the error bars
                  position=position_dodge(.9))
```

![plot of chunk unnamed-chunk-38](figure/unnamed-chunk-38.png) 


# Devo or SLHT by PEDS and age

```r
ggplot(data = by.age.all, aes(x = srage.p, y = referredYes, fill = pedsHiRisk)) + 
  geom_bar(stat="identity", position=position_dodge(), colour="black") +
#  geom_smooth(method = "lm") + 
#  scale_x_discrete(breaks=seq(0, 10, 2), labels=0:5) +
  scale_y_continuous(labels=percent) +
  scale_fill_brewer(name = "PEDS High Risk") + 
#  scale_fill_grey(name = "PEDS High Risk") +
  xlab("Age (years)") +
  ylab("Percentage Referred") +
  theme_bw(base_family = "Times") +
  ggtitle("Percentage Referred to Developmental Specialist or Speech-Language-Hearing by PEDS and Age")+ 
  geom_errorbar(aes(ymin=referredYes - se2, ymax = referredYes + se2),
                  width=.2,                    # Width of the error bars
                  position=position_dodge(.9))
```

![plot of chunk unnamed-chunk-39](figure/unnamed-chunk-391.png) 

```r


ggplot(data = by.age.all, aes(x = srage.p, y = referredYes, fill = pedsHiRisk)) + 
  geom_point() + 
#  facet_grid(. ~ pedsHiRisk) +
#  geom_smooth(method = "lm") + 
#  scale_x_discrete(breaks=seq(0, 10, 2), labels=0:5) +
  scale_y_continuous(labels=percent) +
  scale_fill_brewer(name = "PEDS High Risk") + 
#  scale_fill_grey(name = "PEDS High Risk") +
  xlab("Age (years)") +
  ylab("Percentage Referred") +
#  theme_bw() +
  ggtitle("Percentage Referred to Developmental Specialist or Speech-Language-Hearing by PEDS and Age")
```

![plot of chunk unnamed-chunk-39](figure/unnamed-chunk-392.png) 




#### Birthweight
We need to cut the birthweight logically, perhaps by LBW and normal birthweight.

```r
head(chis$brthwk.p.i)
```

```
## [1] 1.73 4.34 3.23 3.74 3.03 3.40
```

```r
chis$bw.cut <- cut(chis$brthwk.p.i, breaks = c(0, 2.5, 1000), labels = c("LBW", 
    "normal BW"))

# Attach bw.cut variable to survey definition
rchis <- svrepdesign(chis[, -(212 + (1:80))], repweights = chis[, (212 + (1:80))], 
    weights = chis$rakedw0, combined.weights = TRUE, scale = 1, rscales = rep(1, 
        80), type = "other")
```




```r
by.lbw.all <- svyby(~referred, ~pedsHiRisk + bw.cut, FUN = svymean, design = rchis, 
    na.rm = T)
by.lbw.devo <- svyby(~cf46, ~pedsHiRisk + bw.cut, FUN = svymean, design = rchis, 
    na.rm = T)
by.lbw.speech <- svyby(~cf47, ~pedsHiRisk + bw.cut, FUN = svymean, design = rchis, 
    na.rm = T)

by.lbw.all
```

```
##               pedsHiRisk    bw.cut referredNo referredYes     se1     se2
## No.LBW                No       LBW     0.6865      0.3135 0.04862 0.04862
## Yes.LBW              Yes       LBW     0.4093      0.5907 0.10832 0.10832
## No.normal BW          No normal BW     0.8735      0.1265 0.01284 0.01284
## Yes.normal BW        Yes normal BW     0.6315      0.3685 0.03173 0.03173
```

```r
by.lbw.devo
```

```
##               pedsHiRisk    bw.cut cf46Not referred cf46Referred      se1
## No.LBW                No       LBW           0.8048      0.19522 0.045250
## Yes.LBW              Yes       LBW           0.5523      0.44766 0.093010
## No.normal BW          No normal BW           0.9362      0.06384 0.006694
## Yes.normal BW        Yes normal BW           0.7534      0.24664 0.032331
##                    se2
## No.LBW        0.045250
## Yes.LBW       0.093010
## No.normal BW  0.006694
## Yes.normal BW 0.032331
```

```r
by.lbw.speech
```

```
##               pedsHiRisk    bw.cut cf47Not referred cf47Referred     se1
## No.LBW                No       LBW           0.7898      0.21018 0.04361
## Yes.LBW              Yes       LBW           0.5187      0.48127 0.10042
## No.normal BW          No normal BW           0.9054      0.09464 0.01243
## Yes.normal BW        Yes normal BW           0.6974      0.30263 0.02973
##                   se2
## No.LBW        0.04361
## Yes.LBW       0.10042
## No.normal BW  0.01243
## Yes.normal BW 0.02973
```


# Plot for Birthweight

```r
ggplot(data = by.lbw.speech, aes(x = bw.cut, y = cf47Referred, fill = pedsHiRisk)) + 
  geom_bar(stat="identity", position=position_dodge(), colour="black") +
#  geom_smooth(method = "lm") + 
#  scale_x_discrete(breaks=seq(0, 10, 2), labels=0:5) +
  scale_y_continuous(labels=percent) +
  scale_fill_brewer(name = "PEDS High Risk") + 
#  scale_fill_grey(name = "PEDS High Risk") +
  xlab("Birthweight") +
  ylab("Percentage Referred") +
  theme_bw(base_family = "Times") +
  ggtitle("Percentage Referred to Speech-Language-Hearing by PEDS and Birthweight")+ 
  geom_errorbar(aes(ymin=cf47Referred - se2, ymax = cf47Referred + se2),
                  width=.2,                    # Width of the error bars
                  position=position_dodge(.9))
```

![plot of chunk unnamed-chunk-42](figure/unnamed-chunk-42.png) 


