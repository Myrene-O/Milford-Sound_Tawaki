library(lme4) 
library(car)
library(data.table)
library(ggplot2)
library(ggpubr)
library(scales)
library(Rmisc)

##Note: Milford marina fjord salinity and temperature data was provided by Meridian Energy (and are thus, not uploaded to github).

se <- function(x) sd(x)/sqrt(length(x))

##FJORD VARIABLES

#load fjord temp data

EVF <- read.csv("Overall_Analysis_Temp_35days.csv", fileEncoding="UTF-8-BOM")
EVF = data.table(EVF)
EVF$Year<-as.factor(EVF$Year)
EVF$fbirdID<-as.factor(EVF$birdID)`

#summarise data

T1_summary = EVF[, .("mean" = mean(m0.5), "sd" = sd(m0.5)), by = c("Year")]
T2_summary = EVF[, .("mean" = mean(m0.5), "se" = se(m0.5)), by = c("Year")]

#Make linear model and perform anova

Temp <- lm(m0.5 ~ Year, data = EVF)
anova(Temp)
Anova(Temp)

#load fjord salinity data

EVF <- read.csv("Overall_Analysis_Salinity_35days.csv", fileEncoding="UTF-8-BOM")
EVF = data.table(EVF)
EVF$Year<-as.factor(EVF$Year)

#summarise data

S1_summary = EVF[, .("mean" = mean(m0.5), "sd" = sd(m0.5)), by = c("Year")]
S2_summary = EVF[, .("mean" = mean(m0.5), "se" = se(m0.5)), by = c("Year")]

#Make linear model and perform anova

Sal <- lm(m0.5 ~ Year, data = EVF)
anova(Sal)
Anova(Sal)
 
#load fjord rainfall data

EVF <- read.csv("Overall_Analysis_Rainfall_35days.csv", fileEncoding="UTF-8-BOM")
EVF = data.table(EVF)
EVF$Year<-as.factor(EVF$Year)

#summarise data

R1_summary = EVF[, .("mean" = mean(Rain), "sd" = sd(Rain)), by = c("Year")]
R2_summary = EVF[, .("mean" = mean(Rain), "se" = se(Rain)), by = c("Year")]

#Make linear model and perform anova

Rain <- lm(Rain ~ Year, data = EVF)
anova(Rain)
Anova(Rain)

#load fjord wind speed data

EVF <- read.csv("Overall_Analysis_Wind_Speed_35days.csv", fileEncoding="UTF-8-BOM")
EVF = data.table(EVF)
EVF$Year<-as.factor(EVF$Year)

#summarise data

W1_summary = EVF[, .("mean" = mean(AvgWindhr), "sd" = sd(AvgWindhr)), by = c("Year")]
W2_summary = EVF[, .("mean" = mean(AvgWindhr), "se" = se(AvgWindhr)), by = c("Year")]

#Make linear model and perform anova

AvgWindhr <- lm(AvgWindhr ~ Year, data = EVF)
anova(AvgWindhr)
Anova(AvgWindhr)


#Create graphs depicting means and standard errors for each environmental fjord variable 

#Salinity graph

pS <- ggplot(S2_summary, aes(x = Year, y= mean)) +
  geom_bar(position="dodge",stat="identity",width=0.8)+
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se),position=position_dodge(0.9), width=0.2)+
  labs(x="Year", y = "Salinity (PSU)")+
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + theme(axis.title.y = element_text(color = "black", size = 16, margin = margin(t = 15, r = 15, b = 15, l = 15))) + theme(axis.title.x = element_text(color = "black", size = 16, margin = margin(t = 15, r = 15, b = 15, l = 15))) + theme(legend.text = element_text(size = 14)) + theme(legend.title = element_text(size = 15)) + theme(axis.text.x=element_text(colour="black", size=14)) + theme(axis.text.y=element_text(colour="black", size=14)) + scale_y_continuous(limits=c(1,17),oob = rescale_none)

pS


#Wind speed graph

pW <- ggplot(W2_summary, aes(x = Year, y= mean)) +
  geom_bar(position="dodge",stat="identity",width=0.8)+
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se),position=position_dodge(0.9), width=0.2)+
  labs(x="Year", y = "Wind Speed (m/s)")+
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + theme(axis.title.y = element_text(color = "black", size = 16, margin = margin(t = 15, r = 15, b = 15, l = 15))) + theme(axis.title.x = element_text(color = "black", size = 16, margin = margin(t = 15, r = 15, b = 15, l = 15))) + theme(legend.text = element_text(size = 14)) + theme(legend.title = element_text(size = 15)) + theme(axis.text.x=element_text(colour="black", size=14)) + theme(axis.text.y=element_text(colour="black", size=14)) + scale_y_continuous(breaks = seq(0, 3, by=1.0), limits=c(0,3))

pW

#Temp graph

pT <- ggplot(T2_summary, aes(x = Year, y= mean)) +
  geom_bar(position="dodge",stat="identity",width=0.8)+
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se),position=position_dodge(0.9), width=0.2)+
  labs(x="Year", y = "Water Temperature (°C)")+
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + theme(axis.title.y = element_text(color = "black", size = 16, margin = margin(t = 15, r = 15, b = 15, l = 15))) + theme(axis.title.x = element_text(color = "black", size = 16, margin = margin(t = 15, r = 15, b = 15, l = 15))) + theme(legend.text = element_text(size = 14)) + theme(legend.title = element_text(size = 15)) + theme(axis.text.x=element_text(colour="black", size=14)) + theme(axis.text.y=element_text(colour="black", size=14)) + scale_y_continuous(limits=c(6,11),oob = rescale_none)

pT


#Rainfall graph

pR <- ggplot(R2_summary, aes(x = Year, y= mean)) +
  geom_bar(position="dodge",stat="identity",width=0.8)+
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se),position=position_dodge(0.9), width=0.2)+
  labs(x="Year", y = "Rainfall (mm)")+
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + theme(axis.title.y = element_text(color = "black", size = 16, margin = margin(t = 15, r = 15, b = 15, l = 15))) + theme(axis.title.x = element_text(color = "black", size = 16, margin = margin(t = 15, r = 15, b = 15, l = 15))) + theme(legend.text = element_text(size = 14)) + theme(legend.title = element_text(size = 15)) + theme(axis.text.x=element_text(colour="black", size=14)) + theme(axis.text.y=element_text(colour="black", size=14))

pR

#display plots in multi panel figure

multi_plot<- ggarrange(pT,pS,pR,pW) 
multi_plot 


##OCEAN VARIABLES 

EVO2 <- read.csv("OceanBirdsEV.csv", fileEncoding="UTF-8-BOM")

EVO2 = data.table(EVO2)
EVO2$fbirdID<-as.factor(EVO2$birdID)
EVO2$Year<-as.factor(EVO2$Year)

#Salinity 

#summarise data

Sal_summary <- summarySE(EVO2, measurevar="Salinity", groupvars=c("Year", "birdID","TripNumber"))
Sal_summary2 <- summarySE(Sal_summary, measurevar="Salinity", groupvars=c("Year","birdID"))
Sal_summary3 <- summarySE(Sal_summary2, measurevar="Salinity", groupvars=c("Year"))

#make graph for salinity

pS <- ggplot(Sal_summary3, aes(x = Year, y= Salinity)) + coord_cartesian(ylim=c(34.2,35.05)) +
  geom_bar(position="dodge",stat="identity")+
  geom_errorbar(aes(ymin=Salinity-se, ymax=Salinity+se),position=position_dodge(0.9), width=0.2)+
  labs(x="Year", y = "Salinity (PSU)")+
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + theme(axis.title.y = element_text(color = "black", size = 16, margin = margin(t = 15, r = 15, b = 15, l = 15))) + theme(axis.title.x = element_text(color = "black", size = 16, margin = margin(t = 15, r = 15, b = 15, l = 15))) + theme(legend.text = element_text(size = 14)) + theme(legend.title = element_text(size = 15)) + theme(axis.text.x=element_text(colour="black", size=14)) + theme(axis.text.y=element_text(colour="black", size=14))

pS

#make linear mixed model and perform anova

Salinity <- lmer(Salinity ~ Year + (1|birdID/TripNumber), data = EVO2)
anova(Salinity)
Anova(Salinity)

#SST

#summarise data

SST_summary <- summarySE(EVO2, measurevar="SST", groupvars=c("Year", "birdID","TripNumber"))
SST_summary2 <- summarySE(SST_summary, measurevar="SST", groupvars=c("Year","birdID"))
SST_summary3 <- summarySE(SST_summary2, measurevar="SST", groupvars=c("Year"))

#make graph for SST

pT <- ggplot(SST_summary3, aes(x = Year, y= SST)) + coord_cartesian(ylim=c(10,13)) +
  geom_bar(position="dodge",stat="identity")+
  geom_errorbar(aes(ymin=SST-se, ymax=SST+se),position=position_dodge(0.9), width=0.2)+
  labs(x="Year", y = "SST (°C)")+
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + theme(axis.title.y = element_text(color = "black", size = 16, margin = margin(t = 15, r = 15, b = 15, l = 15))) + theme(axis.title.x = element_text(color = "black", size = 16, margin = margin(t = 15, r = 15, b = 15, l = 15))) + theme(legend.text = element_text(size = 14)) + theme(legend.title = element_text(size = 15)) + theme(axis.text.x=element_text(colour="black", size=14)) + theme(axis.text.y=element_text(colour="black", size=14)) 

pT

#make linear mixed model and perform anova

SST <- lmer(SST ~ Year + (1|birdID/TripNumber), data = EVO2)
anova(SST)
Anova(SST)

#Chlorophyll

#summarise data

Chl_summary <- summarySE(EVO2, measurevar="Chl", groupvars=c("Year", "birdID","TripNumber"))
Chl_summary2 <- summarySE(Chl_summary, measurevar="Chl", groupvars=c("Year","birdID"))
Chl_summary3 <- summarySE(Chl_summary2, measurevar="Chl", groupvars=c("Year"))

#make graph for chlorophyll

pC <- ggplot(Chl_summary3, aes(x = Year, y= Chl)) + coord_cartesian(ylim=c(0.0,0.7)) +
  geom_bar(position="dodge",stat="identity")+
  geom_errorbar(aes(ymin=Chl-se, ymax=Chl+se),position=position_dodge(0.9), width=0.2)+
  labs(x="Year", y = "Chlorophyll (mg/m)")+
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + theme(axis.title.y = element_text(color = "black", size = 16, margin = margin(t = 15, r = 15, b = 15, l = 15))) + theme(axis.title.x = element_text(color = "black", size = 16, margin = margin(t = 15, r = 15, b = 15, l = 15))) + theme(legend.text = element_text(size = 14)) + theme(legend.title = element_text(size = 15)) + theme(axis.text.x=element_text(colour="black", size=14)) + theme(axis.text.y=element_text(colour="black", size=14)) 

pC

#make linear mixed model and perform anova

Chl <- lmer(Chl ~ Year + (1|birdID/TripNumber), data = EVO2)
anova(Chl)
Anova(Chl)

#Bathymetry

#summarise data

Bathy_summary <- summarySE(EVO2, measurevar="nzbathy2016", groupvars=c("Year", "birdID","TripNumber"))
Bathy_summary2 <- summarySE(Bathy_summary, measurevar="nzbathy2016", groupvars=c("Year","birdID"))
Bathy_summary3 <- summarySE(Bathy_summary2, measurevar="nzbathy2016", groupvars=c("Year"))

#make graph for bathymetry

pB <- ggplot(Bathy_summary3, aes(x = Year, y= nzbathy2016)) + coord_cartesian(ylim=c(0,1450)) +
  geom_bar(position="dodge",stat="identity")+
  geom_errorbar(aes(ymin=nzbathy2016-se, ymax=nzbathy2016+se),position=position_dodge(0.9), width=0.2)+
  labs(x="Year", y = "Seafloor Bathymetry (m)")+
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + theme(axis.title.y = element_text(color = "black", size = 16, margin = margin(t = 15, r = 15, b = 15, l = 15))) + theme(axis.title.x = element_text(color = "black", size = 16, margin = margin(t = 15, r = 15, b = 15, l = 15))) + theme(legend.text = element_text(size = 14)) + theme(legend.title = element_text(size = 15)) + theme(axis.text.x=element_text(colour="black", size=14)) + theme(axis.text.y=element_text(colour="black", size=14)) 

pB

#make linear mixed model and perform anova

Bathy <- lmer(nzbathy2016 ~ Year + (1|birdID/TripNumber), data = EVO2)
anova(Bathy)
Anova(Bathy)

#display plots in multi panel figure

multi_plot<- ggarrange(pT,pS,pC,pB) 
multi_plot 
