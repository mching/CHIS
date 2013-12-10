#########
# Stratified analysis by age
#########

# Create function that will take age and return odds ratios for each outcome

ORAgeStrat <- function(age) {

  rchis.subset <- subset(rchis, srage.p == age)
  
  output <- list(NA, NA, NA)
  
  mvreg.dev <- svyglm(cf46 ~ 
                        male +
                        # srage.p +
                        brthwk.p.i +
                        racehp2p +
                        srh.a.i +
                        belowpovl +
                        unins.ever +
                        pedsHiRisk
                      ,
                      design = rchis.subset, family = quasibinomial)
  
  mvreg.speech <- svyglm(cf47 ~ 
                        male +
                        # srage.p +
                        brthwk.p.i +
                        racehp2p +
                        srh.a.i +
                        belowpovl +
                        unins.ever +
                        pedsHiRisk
                      ,
                      design = rchis.subset, family = quasibinomial)
  
  mvreg.ref <- svyglm(referred ~ 
                        male +
                        # srage.p +
                        brthwk.p.i +
                        racehp2p +
                        srh.a.i +
                        belowpovl +
                        unins.ever +
                        pedsHiRisk
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



for(i in 1:6) {
  for(j in 1:3) {
    print(cbind(exp(summary(model.list.strat[[i]][[j]])$coef[, 1]), exp(confint(model.list.strat[[i]][[j]]))))
  }
}

a <- as.list(rep(NA, 3))
OR.strat.list <- list(a, a, a)
OR.strat.list <- lapply(OR.strat.list, function(x) lapply(x, function(x) {mean(x)}))
