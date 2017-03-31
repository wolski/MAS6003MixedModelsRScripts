
load('MAS473.RData')
attach(beetle)

(fit<- glm(propn.dead ~conc, family=binomial(logit), weights=number,data=beetle))
(fit2<- glm(cbind(dead, number-dead) ~conc, family=binomial(logit), data=beetle))

####
(eta_1 = coef(fit)[1]+conc[1]*coef(fit)[2]) # linear predictor for first observation
fit$linear.predictors[1]

library(boot)
inv.logit(eta_1) # apply inverse link function to get estimate of p for first observation
fitted(fit) # check the R code agrees


#######################
# log likelihood is
logLik(fit)

# we can see where this comes from as follows
sum(dbinom(dead, number, fitted(fit), log=TRUE))

#######################

(sat_loglik <- sum(dbinom(dead, number, propn.dead, log=TRUE)))

-2*(logLik(fit) - sat_loglik)
deviance(fit)


#####################

summary(fit2)
fit0 <- glm(cbind(dead, number-dead) ~1, family=binomial(logit), data=beetle)
deviance(fit0)

##############################

# we can use the deviance as a measure of fit

(fit3<- glm(cbind(dead, number-dead) ~conc, family=binomial(probit), data=beetle))
(fit4<- glm(cbind(dead, number-dead) ~conc, family=binomial(cloglog), data=beetle))
deviance(fit2)
deviance(fit3)
deviance(fit4)

# so cloglog link fits best.

##############################
# compare with the null model
anova(fit0, fit2)
(diff <- deviance(fit0)-deviance(fit2))
pchisq(diff, df = 1)

# compare with a quadratic model
(fit6 <- glm(cbind(dead, number-dead) ~conc+I(conc^2), family=binomial(logit), data=beetle))

anova(fit2, fit6)
pchisq(8.0373,1)
#so reject the simpler model in favour of the quadratic model

#compare with the saturated model
deviance(fit6)
pchisq(3.194,5)
# so don't reject H_0

### plot the different model predictions
plot(conc, propn.dead)
lines(conc,fitted(fit0), lty=2, col=2, lwd=2)
lines(conc,fitted(fit2), lty=3, col=3, lwd=2)
lines(conc,fitted(fit6), lty=4, col=4, lwd=2)
legend('topleft', c('null', 'linear', 'quadratic'), lty=2:4, col=2:4)


#############################################################################
##
## Bangladesh Fertility Survey data
#
# woman - Identifying code for each woman - a factor
# district - Identifying code for each district - a factor
# use - Contraceptive use at time of survey
# livch - Number of living children at time of survey - an ordered factor. Levels are 0, 1, 2, 3+
# age - Age of woman at time of survey (in years), centred around mean.
# urban - Type of region of residence - a factor. Levels are urban and rural

#
#############################################################################


library(mlmRev)
str(Contraception)

library(lattice)
xyplot(use~age|urban, Contraception, group=livch, type=('smooth'), lty=1:4, auto.key=list(lines=TRUE, lty=1:4, points=FALSE, text=c('0 children', '1 child', '2 children', '3+ children')), ylab='Use contraception')

# no point plotting the data, only the smooth
# women in urban setting more likely to use contraception
# women with no live children less likely to use contraception
# not a strong difference between women who have 1,2 or 3+ children and those who have none
# trend in age appears to be quadratic

# First model
glm1 <- glm(use ~ age + I(age^2)+urban+livch, data=Contraception, family=binomial)
summary(glm1)
# livch1, livch2, livch3+ seem very similar. Lets join those categories
Contraception <- within(Contraception,ch <- factor(livch != 0, labels = c("N", "Y")))
glm2 <- glm(use ~ age + I(age^2)+urban+ch, data=Contraception, family=binomial)
summary(glm2)

#compare the models - not nested, so 
deviance(glm1)
deviance(glm2)
anova(glm1, glm2)
pchisq(0.205,1)
# so we prefer the simpler model


sjp.glm(glm2, type='dots') # odds ratios?
sjp.glm(glm2, type='prob') # 
sjp.glm(glm2, type='ma') # 


#

plot(gm1)

## use random effects
glm2 <- glmer(use ~ age + I(age^2)+urban+livch + (1|district), data = Contraception,family=binomial)
print(glm2, corr=FALSE)
print(gm1)
sjp.glmer(glm2)
sjp.glmer(glm2, type='fe')
sjp.glmer(glm2, type='eff')
sjp.glmer(glm2, type='y.pc')
# not a big differencce between women with 1,2, and 3+ children. Summarize.
Contraception <- within(Contraception,ch <- factor(livch != 0, labels = c("N", "Y")))
glm3 <- glmer(use ~ age + I(age^2)+urban+ch + (1|district), data = Contraception,family=binomial)

anova(glm3, glm2)
# so reduced model is adequate

# add interaction effect between age and whether they have children
glm4 <- glmer(use~ age*ch + I(age^2)+urban+ch + (1|district), data = Contraception,family=binomial)
anova(glm3, glm4)
# yes it is significant improvement

# lets see also whether urban/rural differences vary significantly between districts.
glm5 <- glmer(use~ age*ch + I(age^2)+urban+ch + (1|urban:district), data = Contraception,family=binomial)
anova(glm4, glm5)
# yes, this makes a significant difference.
