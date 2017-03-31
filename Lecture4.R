## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)

## ------------------------------------------------------------------------
library(lme4)
str(sleepstudy)
head(sleepstudy)

## ------------------------------------------------------------------------
library(ggplot2)
qplot(Days, Reaction, facets=~Subject, data = sleepstudy)

## ------------------------------------------------------------------------

qplot(Days, Reaction, facets=~Subject, data = sleepstudy,
geom=c('point', 'smooth'), method='lm')



## ------------------------------------------------------------------------
(fm06 <- lmer(Reaction ~ 1 + Days + (1 + Days | Subject), sleepstudy))
#summary(fm06)

## ------------------------------------------------------------------------
(fm07 <- lmer(Reaction ~ 1 + (1 | Subject)+ Days + (Days-1|Subject), sleepstudy))


