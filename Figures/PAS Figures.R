# Figures
# Do not run in RStudio in Mac OSX unless you have X11. You will crash RStudio.

library(ggplot2)
library(survey)
library(scales)

# Create by age objects
by.age.all <- svyby(~referred, by = ~srage.p + pedsHiRisk, FUN = svymean, design = rchis05, na.rm = T)
by.age.devo <- svyby(~cf46, by = ~srage.p + pedsHiRisk, FUN = svymean, design = rchis05, na.rm = T)
by.age.speech <- svyby(~cf47, by = ~srage.p + pedsHiRisk, FUN = svymean, design = rchis05, na.rm = T)

by.age.all
by.age.devo
by.age.speech

# Low birth weight cuts

chis$bw.cut <- cut(chis$brthwk.p.i, breaks = c(0, 2.5, 1000), labels = c("LBW", "normal BW"))

# Attach bw.cut variable to survey definition
rchis05 <- svrepdesign(chis[ , -( 212 + ( 1 : 80 ))], repweights = chis[ , ( 212 + ( 1 : 80 ))], weights = chis$rakedw0, combined.weights = TRUE, scale = 1, rscales = rep(1,80), type="other")

by.lbw.all <- svyby(~referred, ~pedsHiRisk + bw.cut, FUN = svymean, design = rchis05, na.rm = T)
by.lbw.devo <- svyby(~cf46, ~pedsHiRisk + bw.cut, FUN = svymean, design = rchis05, na.rm = T)
by.lbw.speech <- svyby(~cf47, ~pedsHiRisk + bw.cut, FUN = svymean, design = rchis05, na.rm = T)

# Figure settings

# color
pptcolor <- hsv(rgb2hsv(48, 49, 180))

# Figure 1: Association between DS/SLH and  PEDS by age

p <- ggplot(data = by.age.all, aes(x = srage.p, y = referredYes, fill = pedsHiRisk)) + 
      geom_bar(stat="identity", position=position_dodge(), colour="black") +
      #  geom_smooth(method = "lm") + 
      scale_x_continuous(breaks=0:5) +
      scale_y_continuous(labels=percent) +
      scale_fill_brewer(name = "PEDS High Risk") + 
      # scale_fill_grey(name = "PEDS High Risk", start = 0.5, end = 0.9) +
      xlab("Age (years)") +
      ylab("Percentage Referred") +
      # theme_bw(base_family = "Times") +
      # ggtitle("Percentage Referred to Developmental Specialist by PEDS and Age") + 
      geom_errorbar(aes(ymin=referredYes - se2, ymax = referredYes + se2),
                    width=.2,                    # Width of the error bars
                    position=position_dodge(.9))
# p <- p + theme(text = element_text(colour = pptcolor), axis.text = element_text(colour = pptcolor),
#          rect = element_rect(colour = pptcolor))
ggsave("./Figures/PASFigure1.pdf", plot = p, scale = 0.5)

# Figure 2: Association between either DS or SLH referral and PEDS by Birthweight
# Interaction by BW

p2 <- ggplot(data = by.lbw.all, aes(x = bw.cut, y = referredYes, fill = pedsHiRisk)) + 
  geom_bar(stat="identity", position=position_dodge(), colour="black") +
  #  geom_smooth(method = "lm") + 
  #  scale_x_discrete(breaks=seq(0, 10, 2), labels=0:5) +
  scale_y_continuous(labels=percent) +
  scale_fill_brewer(name = "PEDS High Risk") + 
  # scale_fill_grey(name = "PEDS High Risk", start = 0.5, end = 0.9) +
  # scale_fill_discrete(name = "PEDS High Risk") +
  xlab("Birthweight") +
  ylab("Percentage Referred") +
  # theme_bw(base_family = "Times") +
  # ggtitle("Percentage Referred to DS or SLH by PEDS and Birthweight")+ 
  geom_errorbar(aes(ymin=referredYes - se2, ymax = referredYes + se2),
                width=.2,                    # Width of the error bars
                position=position_dodge(.9))
ggsave("./Figures/PASFigure2.pdf", plot = p2, scale = 0.5)
ggsave("./Figures/PASFigure2.png", plot = p2)