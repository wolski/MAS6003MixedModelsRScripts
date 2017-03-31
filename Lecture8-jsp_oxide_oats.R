## ------------------------------------------------------------------------
library(faraway)
library(lme4)
library(ggplot2)
data(jsp)
head(jsp)
dim(jsp)

## ------------------------------------------------------------------------
jspr <- jsp[jsp$year==2,]
qplot(social, math, data = jspr, geom='boxplot')
qplot(school, math, data = jspr, geom='boxplot')

## ------------------------------------------------------------------------
lmer(math~ social + (1|school), data=jspr, contrasts=list(social=contr.sum))

## ------------------------------------------------------------------------
lmer(math~ social + (1|school)+(1| school:class), data=jspr, contrasts=list(social=contr.sum))


## ------------------------------------------------------------------------
lmer(math~ social + (1| school/class), data=jspr, contrasts=list(social=contr.sum))


## ------------------------------------------------------------------------
load('MAS473.RData')

## ------------------------------------------------------------------------
head(Oxide)

## ------------------------------------------------------------------------
library(ggplot2)
qplot(data=Oxide, Wafer, Thickness, facets=~Lot)
qplot(data=Oxide, Wafer, Thickness, facets=~Lot, geom='boxplot')

## ------------------------------------------------------------------------
qplot(data=Oxide,Lot,Thickness,geom='boxplot')

## ------------------------------------------------------------------------
fm1<-lmer(Thickness~1+(1|Lot/Wafer),data=Oxide)
summary(fm1)


## ------------------------------------------------------------------------
fm2 <- lmer(Thickness~1 +(1|Lot)+(1|Lot:Wafer), data =Oxide)
summary(fm2)
# Estimated random effects
ranef(fm1)


## ------------------------------------------------------------------------

attach(Oats)
qplot(Block, yield, geom='boxplot')
qplot(nitro, yield)
qplot(Variety,yield, geom='boxplot')

qplot(Variety:Block, yield, geom='boxplot', col=Variety)

## ------------------------------------------------------------------------
(fm1<-lmer(yield~nitro+Variety+(1|Block/Variety),Oats))

## ------------------------------------------------------------------------
Plt<-gl(18,4)
Oats<-data.frame(Oats,Plt)

## ------------------------------------------------------------------------
(fm1b<-lmer(yield~nitro+Variety+(1|Block)+(1|Plt),Oats))

## ------------------------------------------------------------------------
summary(fm1b)
ranef(fm1b)

## ------------------------------------------------------------------------

(lm1<-lm(yield~nitro+Variety+Plt ,Oats, contrasts=list(Plt=contr.sum)))

## ------------------------------------------------------------------------
lm1<-lm(yield~nitro+Variety*Block ,Oats, contrasts=list(Block=contr.sum))
summary(lm1)

