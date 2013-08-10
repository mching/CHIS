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
## The following object is masked from 'package:survival':
## 
## untangle.specials
```

```
## The following object is masked from 'package:base':
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
## The following object is masked from 'package:Hmisc':
## 
## deff
```

```
## The following object is masked from 'package:graphics':
## 
## dotchart
```

```r
library(stargazer)
```

```
## Please cite as:
```

```
## Hlavac, Marek (2013). stargazer: LaTeX code and ASCII text for
## well-formatted regression and summary statistics tables.
```

```
## R package version 4.0. http://CRAN.R-project.org/package=stargazer
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
* PEDS risk categories (peds)
* age (srage.p)
* uninsured in the last year (unins.ever)
* Developmental specialist referred (cf46)
* speech referred (cf47)
* Referred to either (referred)

Create a new dataframe to hold just these variables

```r
Table1.frame <- with(chis, data.frame(brthwk.p.i, racehp2p, srh.a.i, male, peds, 
    srage.p, unins.ever, cf46, cf47, referred))
head(Table1.frame)
```

```
##   brthwk.p.i                      racehp2p srh.a.i   male          peds
## 1       1.73                         WHITE      NO   Male MODERATE RISK
## 2       4.34                         WHITE      NO   Male      LOW RISK
## 3       3.23                         WHITE      NO   Male      LOW RISK
## 4       3.74 PI/OTHER SINGLE/MULTIPLE RACE     YES   Male     HIGH RISK
## 5       3.03                        LATINO     YES Female     HIGH RISK
## 6       3.40                         WHITE      NO   Male       NO RISK
##   srage.p                 unins.ever         cf46         cf47 referred
## 1       0            Never uninsured Not referred Not referred       No
## 2       4            Never uninsured Not referred Not referred       No
## 3       5 Some or All Year uninsured Not referred Not referred       No
## 4       2            Never uninsured Not referred Not referred       No
## 5       4            Never uninsured     Referred     Referred      Yes
## 6       0            Never uninsured         <NA>         <NA>     <NA>
```

```r
summary(Table1.frame)
```

```
## 
##  187 values imputed to 3.35 
## 
## 
##  11 values imputed to NO
```

```
##    brthwk.p.i                            racehp2p    srh.a.i   
##  Min.   :0.91   WHITE                        :1948   YES:1556  
##  1st Qu.:3.01   LATINO                       :1239   NO :2707  
##  Median :3.35   ASIAN                        : 617             
##  Mean   :3.33   AFRICAN AMERICAN             : 140             
##  3rd Qu.:3.69   PI/OTHER SINGLE/MULTIPLE RACE: 319             
##  Max.   :4.99                                                  
##      male                 peds         srage.p   
##  Female:2035   NO RISK      :1754   Min.   :0.0  
##  Male  :2228   LOW RISK     : 800   1st Qu.:1.0  
##                MODERATE RISK: 756   Median :3.0  
##                HIGH RISK    : 792   Mean   :2.6  
##                NA's         : 161   3rd Qu.:4.0  
##                                     Max.   :5.0  
##                       unins.ever             cf46                cf47     
##  Never uninsured           :3994   Not referred:3335   Not referred:3221  
##  Some or All Year uninsured: 269   Referred    : 441   Referred    : 555  
##                                    NA's        : 487   NA's        : 487  
##                                                                           
##                                                                           
##                                                                           
##  referred   
##  No  :3061  
##  Yes : 715  
##  NA's: 487  
##             
##             
## 
```


### Gender

```r
summary(Table1.frame$male)
```

```
## Female   Male 
##   2035   2228
```

```r
length(Table1.frame$male)
```

```
## [1] 4263
```

```r
binom.confint(sum(Table1.frame$male == "Male", na.rm = T), length(!is.na(Table1.frame$male)), 
    method = "asym")
```

```
##       method    x    n   mean  lower  upper
## 1 asymptotic 2228 4263 0.5226 0.5076 0.5376
```


### Age

```r
mean(Table1.frame$srage.p)
```

```
## [1] 2.605
```

```r
sd(Table1.frame$srage.p)
```

```
## [1] 1.679
```

```r
n.age <- length(Table1.frame$srage.p)
mean(Table1.frame$srage.p) + qt(0.025, n.age)
```

```
## [1] 0.6445
```

```r
mean(Table1.frame$srage.p) + qt(0.975, n.age)
```

```
## [1] 4.565
```


### Birthweight

```r
summary(Table1.frame$brthwk.p.i)
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
mean(Table1.frame$brthwk.p.i)
```

```
## [1] 3.335
```

```r
sd(Table1.frame$brthwk.p.i)
```

```
## [1] 0.5941
```

```r
n.bw <- length(Table1.frame$brthwk.p.i)
n.bw
```

```
## [1] 4263
```

```r
mean(Table1.frame$brthwk.p.i) + qt(0.025, n.bw)
```

```
## [1] 1.375
```

```r
mean(Table1.frame$brthwk.p.i) + qt(0.975, n.bw)
```

```
## [1] 5.296
```


### PEDS

```r
summary(Table1.frame$peds)
```

```
##       NO RISK      LOW RISK MODERATE RISK     HIGH RISK          NA's 
##          1754           800           756           792           161
```

```r
pedstable <- table(Table1.frame$peds)
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
summary(Table1.frame$srh.a.i)
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
binom.confint(sum(Table1.frame$srh.a.i == "YES"), length(Table1.frame$srh.a.i), 
    method = "asym")
```

```
##       method    x    n  mean  lower  upper
## 1 asymptotic 1556 4263 0.365 0.3505 0.3795
```


### Race

```r
summary(Table1.frame$racehp2p)
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
racetable <- table(Table1.frame$racehp2p)
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


### Referred to Developmental Specialist

```r
summary(Table1.frame$cf46)
```

```
## Not referred     Referred         NA's 
##         3335          441          487
```

```r
length(Table1.frame$cf46)
```

```
## [1] 4263
```

```r
table(Table1.frame$cf46)
```

```
## 
## Not referred     Referred 
##         3335          441
```

```r
sum(table(Table1.frame$cf46))
```

```
## [1] 3776
```

```r
binom.confint(sum(Table1.frame$cf46 == "Referred", na.rm = T), sum(table(Table1.frame$cf46)), 
    method = "asym")
```

```
##       method   x    n   mean  lower upper
## 1 asymptotic 441 3776 0.1168 0.1065 0.127
```


### Referred to Speech, Language, Hearing Evaluation

```r
summary(Table1.frame$cf47)
```

```
## Not referred     Referred         NA's 
##         3221          555          487
```

```r
length(Table1.frame$cf47)
```

```
## [1] 4263
```

```r
table(Table1.frame$cf47)
```

```
## 
## Not referred     Referred 
##         3221          555
```

```r
sum(table(Table1.frame$cf47))
```

```
## [1] 3776
```

```r
binom.confint(sum(Table1.frame$cf47 == "Referred", na.rm = T), sum(table(Table1.frame$cf47)), 
    method = "asym")
```

```
##       method   x    n  mean  lower  upper
## 1 asymptotic 555 3776 0.147 0.1357 0.1583
```


### Referred to either Developmental Specialist or Speech, Language, Hearing Evaluation

```r
summary(Table1.frame$referred)
```

```
##   No  Yes NA's 
## 3061  715  487
```

```r
length(Table1.frame$referred)
```

```
## [1] 4263
```

```r
table(Table1.frame$referred)
```

```
## 
##   No  Yes 
## 3061  715
```

```r
sum(table(Table1.frame$referred))
```

```
## [1] 3776
```

```r
binom.confint(sum(Table1.frame$referred == "Yes", na.rm = T), sum(table(Table1.frame$referred)), 
    method = "asym")
```

```
##       method   x    n   mean  lower  upper
## 1 asymptotic 715 3776 0.1894 0.1769 0.2019
```

