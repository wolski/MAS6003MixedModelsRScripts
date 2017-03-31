library(lme4)
library(lattice)
load('MAS473.RData')
attach(ergoStool)
ergoStool

#### Section 3.5.2 inspecting the data
str(ergoStool)
head(ergoStool)
tail(ergoStool)
xtabs(~ Type + Subject, ergoStool)
matrix(by(effort,list(Type,Subject),mean),4,9)


#### Plotting the data
xyplot(effort~Type|Subject,data=ergoStool)
library(ggplot2)
qplot(Type, effort, facets=~Subject, data =ergoStool)


dotplot(reorder(Subject,effort) ~ effort, data=ergoStool, groups = Type, type = c("p"),pch=Type, par.settings=simpleTheme(pch=Type), xlab = "Effort to arise (Borg scale)", auto.key = list(columns=4))
qplot(reorder(effort, effort), reorder(Subject,effort),  pch=Type, data =ergoStool, color=Type,xlab = "Effort to arise (Borg scale)")


# the argument reorder(Subject,effort) arranges the groups ("Subject") in order of increasing mean effort. 

plot(Type,effort, xlab="Type", ylab="effort")
qplot(Type, effort, geom='boxplot')

plot(reorder(Subject,effort),effort, xlab="Subject", ylab="effort") 
qplot(reorder(Subject, effort), effort, geom='boxplot', xlab='Subject')

plot(Subject,effort, xlab="Subject", ylab="effort")
plot.design(ergoStool)

#### 3.5.3 Fitting the model
# Cell means formulation
(fm1<-lmer(effort~Type-1 + (1|Subject),ergoStool))

# Default formulation
(fm2<-lmer(effort~Type + (1|Subject),ergoStool))

# Sum to zero formulation
options(contrasts = rep("contr.sum", 2))
(fm3<-lmer(effort~Type + (1|Subject),ergoStool))

# go back to default treatment constrasts.
options (contrasts = rep("contr.treatment", 2))

summary(fm1)


# Compare fixed effects estimates with
by(effort,Type,mean)
mean(effort[Type=='T1'])


# Standard errors for the fixed effects estimates
(1.7755/9+1.2106/9)^0.5


# Correlation between fixed effects estimates
1.7755/(1.7755+1.2106)

#############################################

# Compare fixed and random effects
fm1
(lm1<-lm(effort~Type-1+Subject,ergoStool, contrasts = list(Subject=contr.sum)))
summary(lm1)
summary(fm1)
# Note the standard errors are smaller.
# Why?

########################################################

