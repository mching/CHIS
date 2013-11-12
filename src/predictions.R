# Predicted probability of referral from model
# Next step add error lines, something wrong with my geom_ribbon method here

library(ggplot2)
library(survey)

# Smaller model for practice
mv.either.simple <- svyglm(referred ~ pedsHiRisk
                           + srage.p 
                           + pedsHiRisk*brthwk.p.i
                           + pedsHiRisk*srage.p,
                           design = rchis05, family = quasibinomial)
summary(mv.either.simple)

medbw <- svyquantile(~brthwk.p.i, rchis05, 0.5) # Median birthweight

# Generate new simulated data
age.pred <- rep(seq(0, 5, 0.1), 2)
pedsHiRisk.pred <- rep(c(1, 2), c(length(age.pred)/2, length(age.pred)/2))
pedsHiRisk.pred <- factor(pedsHiRisk.pred, levels = 1:2, labels = c("No", "Yes"))
bw.pred <- rep(medbw, c(length(age.pred)))

newdata <- data.frame(srage.p = age.pred, pedsHiRisk = pedsHiRisk.pred, brthwk.p.i = bw.pred)
new.pred <- predict(mv.either.simple, newdata = newdata, type = "response", 
                    se.fit = T, vcov=T)
newdata$new.pred <- new.pred
newdata$new.pred.se <- SE(new.pred)^0.5

newdata
ggplot(data = newdata, aes(x = srage.p, y = as.numeric(new.pred), 
                           group = pedsHiRisk, 
                           color = pedsHiRisk)) + 
  geom_line() 
# geom_ribbon(aes(ymin = new.pred - new.pred.se, ymax = new.pred + new.pred.se))


# Birthweight predictions

medage <- svyquantile(~srage.p, rchis05, 0.5) # Median age

bw.pred <- rep(seq(1, 5, 0.1), 2)
age.pred <- rep(medage, length(bw.pred))
pedsHiRisk.pred <- rep(c(1, 2), c(length(bw.pred)/2, length(bw.pred)/2))
pedsHiRisk.pred <- factor(pedsHiRisk.pred, levels = 1:2, labels = c("No", "Yes"))

newdata <- data.frame(srage.p = age.pred, pedsHiRisk = pedsHiRisk.pred, brthwk.p.i = bw.pred)
newdata$new.pred <- predict(mv.either.simple, newdata = newdata, type = "response", se.fit = T)
head(newdata)
ggplot(data = newdata, aes(x = brthwk.p.i, y = as.numeric(new.pred), 
                           group = pedsHiRisk, 
                           color = pedsHiRisk)) + 
  geom_line()