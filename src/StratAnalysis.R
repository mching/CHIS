#########
# Stratified analysis by age
#########

# Recenter birthweight to median
svyquantile(~brthwk.p.i, design = rchis05, quant = 0.5)
chis$brthwk.p.i.recenter <- chis$brthwk.p.i - 3.35

# attach to survey design object
rchis <- svrepdesign(chis[ , -( 212 + ( 1 : 80 ))], repweights = chis[ , ( 212 + ( 1 : 80 ))], weights = chis$rakedw0, combined.weights = TRUE, scale = 1, rscales = rep(1,80), type="other")

# Create subset design object
rchis05 <- subset(rchis, srage.p < 6)


# Create function that will take age and return odds ratios for each outcome

ORAgeStrat <- function(age) {

  rchis.subset <- subset(rchis, srage.p == age)
  
  output <- list(NA, NA, NA)
  
  mvreg.dev <- svyglm(cf46 ~ 
                        male +
                        # srage.p +
                        brthwk.p.i.recenter +
                        racehp2p +
                        srh.a.i +
                        belowpovl +
                        unins.ever +
                        pedsHiRisk +
                        pedsHiRisk * brthwk.p.i.recenter
                      ,
                      design = rchis.subset, family = quasibinomial)
  
  mvreg.speech <- svyglm(cf47 ~ 
                        male +
                        # srage.p +
                        brthwk.p.i.recenter +
                        racehp2p +
                        srh.a.i +
                        belowpovl +
                        unins.ever +
                        pedsHiRisk +
                        pedsHiRisk * brthwk.p.i.recenter
                         ,
                      design = rchis.subset, family = quasibinomial)
  
  mvreg.ref <- svyglm(referred ~ 
                        male +
                        # srage.p +
                        brthwk.p.i.recenter +
                        racehp2p +
                        srh.a.i +
                        belowpovl +
                        unins.ever +
                        pedsHiRisk +
                        pedsHiRisk * brthwk.p.i.recenter
                      ,
                      design = rchis.subset, family = quasibinomial)
  
  output[[1]] <- mvreg.dev
  output[[2]] <- mvreg.speech
  output[[3]] <- mvreg.ref
  
  return(output)
}

model.list.strat <- lapply(as.list(0:5), ORAgeStrat)

names(model.list.strat) <- paste("Age", 0:5)

exp(summary(model.list.strat[[5]][[3]])$coef)
exp(confint(model.list.strat[[5]][[3]]))

cbind(exp(summary(model.list.strat[[5]][[3]])$coef[, 1]), exp(confint(model.list.strat[[5]][[3]])))


ModelNames <- c("DS", "SLH", "DS/SLH")

for(i in 1:6) {
  for(j in 1:3) {
    print("###################################")
    print(paste("Age", i, "Model", ModelNames[j]))
    print(cbind(exp(summary(model.list.strat[[i]][[j]])$coef[, 1]), exp(confint(model.list.strat[[i]][[j]]))))
  }
}

###########################
# SLH birthweight interaction stratified analysis
###########################
