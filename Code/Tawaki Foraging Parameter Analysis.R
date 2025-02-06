library(heplots)
library(lme4)
library(car)
library(Matrix)
library(JWileymisc)
library(glmmTMB)
library(DHARMa)
  
Fora <- read.csv("Foraging Parameters Analysis.csv", fileEncoding="UTF-8-BOM")
Fora2 <- read.csv("Foraging Parameters Analysis no Pengu.csv", fileEncoding="UTF-8-BOM") #removing the data point with only dive count and dive duration

Fora$fbirdID<-as.factor(Fora$birdID)
Fora$Colony<-as.factor(Fora$Colony)
Fora$Year<-as.factor(Fora$Year)

Fora2$fbirdID<-as.factor(Fora2$birdID)
Fora2$Colony<-as.factor(Fora2$Colony)
Fora2$Year<-as.factor(Fora2$Year)

#Centering the factors so you can reliably interpret type 3 ANOVA tables 
contrasts(Fora$Year) <- contr.sum
contrasts(Fora$Colony) <- contr.sum
contrasts(Fora2$Year) <- contr.sum
contrasts(Fora2$Colony) <- contr.sum

#Exploring data distributions

hist(Fora2$Cumul.dist)
Fora2$sqrt_Cumul.dist<- sqrt(Fora2$Cumul.dist)
Fora2$log10_Cumul.dist<- log10(Fora2$Cumul.dist)
Fora2$log_Cumul.dist<- log(Fora2$Cumul.dist)
hist(Fora2$sqrt_Cumul.dist)
hist(Fora2$log10_Cumul.dist)
hist(Fora2$log_Cumul.dist)

hist(Fora2$Max.dist)
Fora2$sqrt_Max.dist<- sqrt(Fora2$Max.dist)
Fora2$sqrt4_Max.dist<- sqrt(sqrt(Fora2$Max.dist))
Fora2$log10_Max.dist<- log10(Fora2$Max.dist)
Fora2$log_Max.dist<- log(Fora2$Max.dist)
hist(Fora2$sqrt_Max.dist)
hist(Fora2$sqrt4_Max.dist)
hist(Fora2$log10_Max.dist)
hist(Fora2$log_Max.dist) #no

hist(Fora$Duration)
Fora$sqrt_Duration<- sqrt(Fora$Duration)
Fora$log10_Duration<- log10(Fora$Duration)
hist(Fora$sqrt_Duration)
hist(Fora$log10_Duration)

hist(Fora2$Seg.Speed)
Fora2$sqrt_Seg.Speed<- sqrt(Fora2$Seg.Speed)
Fora2$log10_Seg.Speed<- log10(Fora2$Seg.Speed)
Fora2$log_Seg.Speed<- log(Fora2$Seg.Speed)
hist(Fora2$sqrt_Seg.Speed)
hist(Fora2$log10_Seg.Speed)
hist(Fora2$log_Seg.Speed)

#Testing LMMs

lmmdist <- lmer(sqrt_Cumul.dist ~ Colony*Year + (1|fbirdID), data=Fora2)
lmmMdist <- lmer(sqrt_Max.dist ~ Colony*Year + (1|fbirdID), data=Fora2)
lmmDur <- lmer(log10_Duration ~ Colony*Year + (1|fbirdID), data=Fora)
lmmSpd <- lmer(log10_Seg.Speed ~ Colony*Year + (1|fbirdID), data=Fora2)

#Segment speed model checking

x = residuals(lmmSpd)
plot(x)
hist(x, prob=TRUE, col="darkgray")
qqnorm(x)
qqline(x, col="red")
confint(lmmSpd, method = "Wald")
shapiro.test(x)

#shapiro test is significant --> switch to GLMM for segment speed

glmm.SSpd <-glmmTMB(Seg.Speed ~ Colony*Year + (1|fbirdID), family = gaussian(link = "log"), data = Fora2)

sim_res <- simulateResiduals(glmm.SSpd)
plot(sim_res)
plotResiduals(sim_res, Fora2$Year)
plotResiduals(sim_res, Fora2$Colony)
testDispersion(sim_res)
performance::check_overdispersion(glmm.SSpd)

#significance testing output

summary(glmm.SSpd)
Anova(glmm.SSpd, type = 3)
confint(glmm.SSpd, method = "Wald")

#Distance Travelled per trip

#model checking

x = residuals(lmmdist)
plot(x)
hist(x, prob=TRUE, col="darkgray")
qqnorm(x)
qqline(x, col="red")
confint(lmmdist, method = "Wald")
shapiro.test(x)

#significance testing output

Anova(lmmdist, type = 3)
summary(lmmdist)
plot(lmmdist)
confint(lmmdist)

#Maximum distance from colony

#model checking

x = residuals(lmmMdist)
plot(x)
hist(x, prob=TRUE, col="darkgray")
qqnorm(x)
qqline(x, col="red")
confint(lmmMdist, method = "Wald")
shapiro.test(x)

#shapiro test is significant --> switch to GLMM for maximum distance

glmm.Mdist <-glmmTMB(Max.dist ~ Colony*Year + (1|fbirdID), family = Gamma(link = "log"), data = Fora2)

sim_res <- simulateResiduals(glmm.Mdist)
plot(sim_res)
plotResiduals(sim_res, Fora2$Year)
plotResiduals(sim_res, Fora2$Colony)
testDispersion(sim_res)
performance::check_overdispersion(glmm.Mdist)

summary(glmm.Mdist)
Anova(glmm.Mdist, type = 3)
confint(glmm.Mdist, method = "Wald")

#Trip duration

#model checking

x = residuals(lmmDur)
plot(x)
hist(x, prob=TRUE, col="darkgray")
qqnorm(x)
qqline(x, col="red")
confint(lmmDur, method = "Wald")
shapiro.test(x)

#significance testing output

Anova(lmmDur, type =3)
summary(lmmDur)
plot(lmmDur)
confint(lmmDur)


#interaction plots

interaction.plot(Fora2$Year, Fora2$Colony, Fora2$sqrt_Max.dist, type="b", col=c(1:3),
                 leg.bty="o", leg.bg="beige", lwd=2, pch=c(18,24,22),
                 xlab="Site",
                 ylab="sqrt_Max.dist",
                 main="Interaction Plot")

interaction.plot(Fora$Year, Fora$Colony, Fora$log10_Duration, type="b", col=c(1:3),
                 leg.bty="o", leg.bg="beige", lwd=2, pch=c(18,24,22),
                 xlab="Site",
                 ylab="log 10 Duration",
                 main="Interaction Plot")


interaction.plot(Fora2$Year, Fora2$Colony, Fora2$sqrt_Cumul.dist, type="b", col=c(1:3),
                 leg.bty="o", leg.bg="beige", lwd=2, pch=c(18,24,22),
                 xlab="Site",
                 ylab="sqrt_Cumul.dist",
                 main="Interaction Plot")


interaction.plot(Fora2$Year, Fora2$Colony, Fora2$sqrt_Max.dist, type="b", col=c(1:3),
                 leg.bty="o", leg.bg="beige", lwd=2, pch=c(18,24,22),
                 xlab="Site",
                 ylab="sqrt_Max.dist",
                 main="Interaction Plot")
