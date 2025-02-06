rm(list=ls())
library(MuMIn)
library(DHARMa)
library(glmmTMB)
library(MASS)
library(car)
Dive <- read.csv("TawakiDiveDatasetComplete.csv", fileEncoding="UTF-8-BOM")

Dive$fbirdID<-as.factor(Dive$birdID)
Dive$Site<-as.factor(Dive$Site)
Dive$Year<-as.factor(Dive$Year)

#Centering the factors so you can reliably interpret type 3 ANOVA tables 
contrasts(Dive$Year) <- contr.sum
contrasts(Dive$Site) <- contr.sum


## Dive Depth Model

glmm.dep<-glmer(Depth ~ Site + Year + Site*Year + (1|fbirdID/TripNumber), family=Gamma(link="log"), control = glmerControl(optimizer = "bobyqa"), data=Dive)


#model checking

sim_res <- simulateResiduals(glmm.dep)
plot(sim_res)
plotResiduals(sim_res, Dive$Year)
plotResiduals(sim_res, Dive$Site)
testDispersion(sim_res)
performance::check_overdispersion(glmm.dep)
testOutliers(sim_res)

plot(fitted(glmm.dep), residuals(glmm.dep))
abline(h=0, lty=2)
lines(smooth.spline(fitted(glmm.dep), residuals(glmm.dep)))
x = residuals(glmm.dep)
hist(x, prob=TRUE, col="darkgray")
qqnorm(x)
qqline(x, col="red")

residuals <- residuals(glmm.dep, type = "pearson")
dispersion_ratio <- sum(residuals^2)/df.residual(glmm.dep)
dispersion_ratio

#significance testing output

summary(glmm.dep)
Anova(glmm.dep, type = 3)
confint(glmm.dep, method = "Wald")

interaction.plot(x.factor = Dive$Year, 
                 trace.factor = Dive$Site,
                 response = Dive$Depth)


## Number of Wiggles Model

glmm.nbwig<-glmmTMB(NoWiggles ~ Site + Year + Site*Year + (1|fbirdID) + (1|fbirdID:TripNumber), family=nbinom1(link="log"), data=Dive)

#model checking

sim_res <- simulateResiduals(glmm.nbwig)
plot(sim_res)
plotResiduals(sim_res, Dive$Year)
plotResiduals(sim_res, Dive$Site)
testDispersion(sim_res)
performance::check_overdispersion(glmm.nbwig)
testZeroInflation(sim_res)
testOutliers(sim_res)

residuals <- residuals(glmm.nbwig, type = "pearson")
dispersion_ratio <- sum(residuals^2)/df.residual(glmm.nbwig)
dispersion_ratio

#significance testing output

summary(glmm.nbwig)
Anova(glmm.nbwig, type = 3)
confint(glmm.nbwig, method = "Wald")

## Foraging efficiency model

glmm.FE <-glmmTMB(Foraging.Efficiency ~ Site + Year + Site*Year + (1|fbirdID) + (1|fbirdID:TripNumber), family = gaussian(link = "log"), data = Dive)

#model checking

sim_res <- simulateResiduals(glmm.FE)
plot(sim_res)
plotResiduals(sim_res, Dive$Year)
plotResiduals(sim_res, Dive$Site)
testDispersion(sim_res)
performance::check_overdispersion(glmm.FE)
testOutliers(sim_res)

#significance testing output

summary(glmm.FE)
Anova(glmm.FE, type = 3)
confint(glmm.FE, method = "Wald")

## Dive duration model

glmm.dur <-glmmTMB(Duration ~ Site + Year + Site*Year + (1|fbirdID) + (1|fbirdID:TripNumber), family=gaussian(link="log"), data=Dive)

#Model checking

sim_res <- simulateResiduals(glmm.dur)
plot(sim_res)
plotResiduals(sim_res, Dive$Year)
plotResiduals(sim_res, Dive$Site)
testDispersion(sim_res)
performance::check_overdispersion(glmm.dur)
testOutliers(sim_res)

#significance testing output

summary(glmm.dur)
Anova(glmm.dur, type = 3)
confint(glmm.dur, method = "Wald")


interaction.plot(x.factor = Dive$Year, 
                 trace.factor = Dive$Site,
                 response = Dive$Duration)


#Descent velocity model

glmm.DV <-glmmTMB(DescVelo ~ Site + Year + Site*Year + (1|fbirdID) + (1|fbirdID:TripNumber), family = gaussian(link = "log"), data = Dive)

#Model checking

sim_res <- simulateResiduals(glmm.DV)
plot(sim_res)
plotResiduals(sim_res, Dive$Year)
plotResiduals(sim_res, Dive$Site)
testDispersion(sim_res)
performance::check_overdispersion(glmm.DV)
testOutliers(glmm.DV)

#significance testing outputs

summary(glmm.DV)
Anova(glmm.DV, type = 3)
confint(glmm.DV, method = "Wald")

interaction.plot(x.factor = Dive$Year, 
                 trace.factor = Dive$Site,
                 response = Dive$DescVelo)


## Ascent velocity model

glmm.AV <-glmmTMB(AscVelo ~ Site + Year + Site*Year + (1|fbirdID) + (1|fbirdID:TripNumber), family = gaussian(link = "log"), data = Dive)

#model checking

sim_res <- simulateResiduals(glmm.AV)
plot(sim_res)
plotResiduals(sim_res, Dive$Year)
plotResiduals(sim_res, Dive$Site)
testDispersion(sim_res)
performance::check_overdispersion(glmm.AV)
testOutliers(glmm.AV)

#significance testing outputs

summary(glmm.AV)
Anova(glmm.AV, type = 3)
confint(glmm.AV, method = "Wald")

## Bottom time model

glmm.BT<-glmmTMB(BottomTime ~ Site + Year + Site*Year + (1|fbirdID) + (1|fbirdID:TripNumber), family=gaussian(link="log"), data=Dive)

#model checking

sim_res <- simulateResiduals(glmm.BT)
plot(sim_res) 
plotResiduals(sim_res, Dive$Year)
plotResiduals(sim_res, Dive$Site)
testDispersion(sim_res)
performance::check_overdispersion(glmm.AV)
testOutliers(glmm.BT)

#significance testing outputs

summary(glmm.BT)
Anova(glmm.BT, type = 3)
confint(glmm.BT, method = "Wald")

## Dives per hour model

perhr <- read.csv("Divesperhour.csv", fileEncoding="UTF-8-BOM")
perhr$ID<-as.factor(perhr$ID)
perhr$Site<-as.factor(perhr$Site)
perhr$Year<-as.factor(perhr$Year)

#Centering the factors so you can reliably interpret type 3 ANOVA tables 
contrasts(perhr$Year) <- contr.sum
contrasts(perhr$Site) <- contr.sum

glmm.nbdc<-glmmTMB(Dive.Count ~ Site*Year + (1|ID) + (1|ID:Trip), family=nbinom1(link="log"), data=perhr)

#model checking

sim_res <- simulateResiduals(glmm.nbdc)
plot(sim_res)

plotResiduals(sim_res, perhr$Year)
plotResiduals(sim_res, perhr$Site)
testDispersion(sim_res)
performance::check_overdispersion(glmm.nbdc)

testZeroInflation(glmm.nbdc)
testOutliers(sim_res, type = "bootstrap")

#significance testing outputs

summary(glmm.nbdc)
Anova(glmm.nbdc, type = 3)
confint(glmm.nbdc, method = "Wald")

#NOTE FOR DHARMA BY CREATOR TO USERS: If you have a lot of data points e.g. over 5000 (NB: there are over 30,000 dives analysed in this dataset), residual diagnostics will nearly inevitably become significant, because having a perfectly fitting model is very unlikely. That, however, doesn't necessarily mean that you need to change your model. The p-values confirm that there is a deviation from your null hypothesis. It is, however, in your discretion to decide whether this deviation is worth worrying about. 
