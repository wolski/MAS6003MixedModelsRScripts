library(lme4)
load('MAS473.RData')
attach(raildata)

library(ggplot2)
qplot(Rail, travel, geom='boxplot')

fm1.ml<-lmer(travel~1+(1|Rail),raildata,REML=F)
fm1.reml<-lmer(travel~1+(1|Rail),raildata)

logLik(fm1.ml)
logLik(fm1.reml)

logLik(fm1.reml, REML=FALSE) 
# not the same thing as gives log likelihood at REML parameter estimates

#############################

fm1<-lmer(travel~1+(1|Rail),raildata)
summary(fm1)

lm1<-lm(travel~Rail, contrasts=list(Rail=contr.sum),raildata)
summary(lm1)


coef(lm1)
sum(-coef(lm1)[-1]) # alpha_6
ranef(fm1.reml)

mean(travel) # estimate of beta for both models
sqrt(4.021^2/18)
sqrt(1/18*(3*615.31 + 16.17))

## sjPlot library
library(sjPlot)
sjp.lmer(fm1,sort.coef=T)
sjp.lmer(fm1, type='fe')

