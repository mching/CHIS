library(survey)
library(stargazer)

ORTable <- function(roundDigits = 2) {
  DSTable <- cbind(OddsRatio = exp(model1$coef), exp(confint(model1)))
  DSTable <- round(DSTable, roundDigits)
  DS_OR_CI <- paste0(DSTable[,1], " ", "(", DSTable[,2], "-", DSTable[,3], ")")
  
  SLHTable <- cbind(OddsRatio = exp(model2$coef), exp(confint(model2)))
  SLHTable <- round(SLHTable, roundDigits)
  SLH_OR_CI <- paste0(SLHTable[,1], " ", "(", SLHTable[,2], "-", SLHTable[,3], ")")
  
  EitherTable <- cbind(OddsRatio = exp(model3$coef), exp(confint(model3)))
  EitherTable <- round(EitherTable, roundDigits)
  Either_OR_CI <- paste0(EitherTable[,1], " ", "(", EitherTable[,2], "-", EitherTable[,3], ")")
  
  cbind(attr(DSTable, "dimnames")[[1]][-1], DS_OR_CI[-1], SLH_OR_CI[-1], Either_OR_CI[-1])
}

# Male
model1 <- svyglm(cf46 ~ male, design = rchis05, family = quasibinomial)
model2 <- svyglm(cf47 ~ male, design = rchis05, family = quasibinomial)
model3 <- svyglm(referred ~ male, design = rchis05, family = quasibinomial)
Tab2_1 <- ORTable()

# Age
model1 <- svyglm(cf46 ~ srage.p, design = rchis05, family = quasibinomial)
model2 <- svyglm(cf47 ~ srage.p, design = rchis05, family = quasibinomial)
model3 <- svyglm(referred ~ srage.p, design = rchis05, family = quasibinomial)
Tab2_2 <- ORTable()

### Birthweight
model1 <- svyglm(cf46 ~ brthwk.p.i, design = rchis05, family = quasibinomial)
model2 <- svyglm(cf47 ~ brthwk.p.i, design = rchis05, family = quasibinomial)
model3 <- svyglm(referred ~ brthwk.p.i, design = rchis05, family = quasibinomial)
Tab2_3 <- ORTable()

### Race
model1 <- svyglm(cf46 ~ racehp2p, design = rchis05, family = quasibinomial)
model2 <- svyglm(cf47 ~ racehp2p, design = rchis05, family = quasibinomial)
model3 <- svyglm(referred ~ racehp2p, design = rchis05, family = quasibinomial)
Tab2_4 <- ORTable()

### Hispanic Ethnicity
model1 <- svyglm(cf46 ~ srh.a.i, design = rchis05, family = quasibinomial)
model2 <- svyglm(cf47 ~ srh.a.i, design = rchis05, family = quasibinomial)
model3 <- svyglm(referred ~ srh.a.i, design = rchis05, family = quasibinomial)
Tab2_5 <- ORTable()

### Below Poverty Level
model1 <- svyglm(cf46 ~ belowpovl, design = rchis05, family = quasibinomial)
model2 <- svyglm(cf47 ~ belowpovl, design = rchis05, family = quasibinomial)
model3 <- svyglm(referred ~ belowpovl, design = rchis05, family = quasibinomial)
Tab2_6 <- ORTable()

### Uninsured Ever in Last Year
model1 <- svyglm(cf46 ~ unins.ever, design = rchis05, family = quasibinomial)
model2 <- svyglm(cf47 ~ unins.ever, design = rchis05, family = quasibinomial)
model3 <- svyglm(referred ~ unins.ever, design = rchis05, family = quasibinomial)
Tab2_7 <- ORTable()

### PEDS High Risk
model1 <- svyglm(cf46 ~ pedsHiRisk, design = rchis05, family = quasibinomial)
model2 <- svyglm(cf47 ~ pedsHiRisk, design = rchis05, family = quasibinomial)
model3 <- svyglm(referred ~ pedsHiRisk, design = rchis05, family = quasibinomial)
Tab2_8 <- ORTable()

Table2 <- rbind(Tab2_1, Tab2_2, Tab2_3, Tab2_4, Tab2_5, Tab2_6, Tab2_7, Tab2_8)
write.csv(Table2, file = "./Tables/Table2.csv")