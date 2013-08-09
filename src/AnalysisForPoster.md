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
* gender  
* birthweight  
* ethnicity  
* race  
* PEDS risk categories
* age
* uninsured in the last year

