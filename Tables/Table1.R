# Make Table1.csv

props <- rbind(cbind(svymean(~male, rchis05), confint(svymean(~male, rchis05))),
      cbind(svymean(~racehp2p, rchis05), confint(svymean(~racehp2p, rchis05))),
      cbind(svymean(~srh.a.i, rchis05), confint(svymean(~srh.a.i, rchis05))),
      cbind(svymean(~unins.ever, rchis05), confint(svymean(~unins.ever, rchis05))),
      cbind(svymean(~belowpovl, rchis05), confint(svymean(~belowpovl, rchis05))),
      cbind(svymean(~cf46, rchis05, na.rm = T), confint(svymean(~cf46, rchis05, na.rm = T))),
      cbind(svymean(~cf47, rchis05, na.rm = T), confint(svymean(~cf47, rchis05, na.rm = T))),
      cbind(svymean(~referred, rchis05, na.rm = T), confint(svymean(~referred, rchis05, na.rm = T))),
      cbind(svymean(~peds, rchis05, na.rm = T), confint(svymean(~peds, rchis05, na.rm = T)))    
)

means <- rbind(cbind(svymean(~srage.p, rchis05), confint(svymean(~srage.p, rchis05))),
      cbind(svymean(~brthwk.p.i, rchis05), confint(svymean(~brthwk.p.i, rchis05)))
)

Table1 <- rbind(props, means)
rownames(Table1) <- c("Female", "Male",
                       "White", "Latino", "Asian", "African American", "Pacific Islander/Other/Multiple",
                       "Hispanic", "Non-Hispanic",
                       "Never uninsured", "Some or All Year Uninsured",
                       "100% FPL or higher", "Below 100% FPL",
                       "Not referred to DS", "Referred to DS",
                       "Not referred to SLH", "Referred to SLH",
                       "Not referred to DS/SLH", "Referred to DS or SLH",
                       "No Risk", "Low Risk", "Moderate Risk", "High Risk",
                       "Age (y)", 
                       "Birthweight (kg)"
                      )
colnames(Table1) <- c("Estimate", "2.5%", "97.5%")
Table1
write.csv(Table1, "./Tables/Table1.csv")

# After writing table, need to enter Excel to reorder the rows, add percents, change the decimal places, save as
# text and re-import here

Table1 <- read.csv("./Tables/Table1_reformat.csv", colClasses = "character")
Table1
Table1[[3]] <- gsub("%", "", Table1[[3]])
CIest <- rep("", length(Table1[[3]]))

for(i in 1:length(Table1[[3]])){
  CIest[i] <- (paste0("(", Table1[i, 3], "-", Table1[i, 4], ")"))
}
Table1 <- cbind(Table1[1:2], CIest)
write.csv(Table1, "./Tables/Table1out.csv")