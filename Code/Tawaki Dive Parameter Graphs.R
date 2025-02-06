library(plyr)
library(dplyr)
library(data.table)
library(car)
library(ggplot2)
library(ggthemes)
library(ggpubr)
library(Rmisc)

rm(list=ls())
#DiveDataset
Dive <- read.csv("TawakiDiveDatasetComplete.csv", fileEncoding="UTF-8-BOM")
head(Dive)

#Split dataset by Year
both2019 <- Dive[c(2:15943),c(1:16)]
both2020 <- Dive[c(15943:32669),c(1:16)]

se <- function(x) sd(x)/sqrt(length(x))  # this code defines the function
#to calculate the standard error

Dive = data.table(Dive)

#define categorical varibales as factors
Dive$fbirdID<-as.factor(Dive$birdID)
Dive$Year<-as.factor(Dive$Year)
Dive$Site<-as.factor(Dive$Site)
both2019$fbirdID<-as.factor(both2019$birdID)
both2019$Site<-as.factor(both2019$Site)
both2020$fbirdID<-as.factor(both2020$birdID)
both2020$Site<-as.factor(both2020$Site)

#Foraging Efficiency
both2019 = data.table(both2019)
both2020 = data.table(both2020)
both2020$Year<-as.factor(both2020$Year)
both2019$Year<-as.factor(both2019$Year)

#filtering out rows/removing rows at hours where minimal foraging activity was occuring (nighttime)
library(dplyr)
both2020 = filter(both2020, Hour != "1" & Hour != "2"  & Hour != "0"  & Hour != "3"  & Hour != "4"  & Hour != "5"  & Hour != "21"  & Hour != "22" & Hour != "23" & Hour != "24")
both2019 = filter(both2019, Hour != "1" & Hour != "2"  & Hour != "0"  & Hour != "3"  & Hour != "4"  & Hour != "5"  & Hour != "21"  & Hour != "22" & Hour != "23" & Hour != "24")

#summarizing data for graph (2019)

Dive_summary = both2019[, .("Meanbird" = mean(Foraging.Efficiency, na.rm = TRUE), "sd" = sd(Foraging.Efficiency, na.rm = TRUE)), by = c("birdID", "Site", "Hour")]

data.summary2 <- ddply(Dive_summary, .(Site, Hour), 
                       summarise, mean = mean(Meanbird, na.rm = TRUE), se = se(Meanbird))

data.summary2$Site <- as.factor(data.summary2$Site)

#plotting graph

p1<- ggplot(data.summary2, aes(x=Hour, y=mean, color=Site, shape=Site)) + 
  geom_line() +
  geom_point(size = 3.5)+
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=0.8,
                position=position_dodge(0.05)) +labs(x="Hour of Day", y = "Foraging Efficiency")+
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + scale_color_manual(values=c('grey4','grey47')) + coord_cartesian(xlim = c(6, 20), ylim = c(0.15, 0.6)) + scale_x_continuous(breaks = round(seq(min(data.summary2$Hour), max(data.summary2$Hour), by = 2),1)) + theme(axis.title.y = element_text(color = "black", size = 12, margin = margin(t = 15, r = 15, b = 15, l = 15))) + theme(axis.title.x = element_text(color = "black", size = 12, margin = margin(t = 15, r = 15, b = 15, l = 15))) + theme(legend.text = element_text(size = 12)) + theme(legend.title = element_text(size = 15)) + theme(axis.text.x=element_text(colour="black")) + theme(axis.title.y = element_text(color = "black", size = 12, margin = margin(t = 15, r = 15, b = 15, l = 15))) + theme(axis.title.x = element_text(color = "black", size = 12, margin = margin(t = 15, r = 15, b = 15, l = 15))) + theme(legend.text = element_text(size = 11)) + theme(legend.title = element_text(size = 12)) + theme(axis.text.x=element_text(colour="black", size=12)) + theme(axis.text.y=element_text(colour="black", size=12)) + ggtitle("2019") + theme(plot.title = element_text(face = "bold", hjust = 0.5, size =18))

p1 

#summarizing data for graph (2020)

Dive_summary = both2020[, .("Meanbird" = mean(Foraging.Efficiency, na.rm = TRUE), "sd" = sd(Foraging.Efficiency, na.rm = TRUE)), by = c("birdID", "Site", "Hour")]

data.summary2 <- ddply(Dive_summary, .(Site, Hour), 
                       summarise, mean = mean(Meanbird, na.rm = TRUE), se = se(Meanbird))

data.summary2$Site <- as.factor(data.summary2$Site)

#plotting graph

p2<- ggplot(data.summary2, aes(x=Hour, y=mean, color=Site, shape=Site)) + 
  geom_line() +
  geom_point(size = 3.5)+
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=0.8,
                position=position_dodge(0.05)) +labs(x="Hour of Day", y = "Foraging Efficiency")+
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + scale_color_manual(values=c('grey4','grey47')) + coord_cartesian(xlim = c(6, 20), ylim = c(0.15, 0.6)) + scale_x_continuous(breaks = round(seq(min(data.summary2$Hour), max(data.summary2$Hour), by = 2),1)) + theme(axis.title.y = element_text(color = "black", size = 12, margin = margin(t = 15, r = 15, b = 15, l = 15))) + theme(axis.title.x = element_text(color = "black", size = 12, margin = margin(t = 15, r = 15, b = 15, l = 15))) + theme(legend.text = element_text(size = 12)) + theme(legend.title = element_text(size = 13)) + theme(axis.text.x=element_text(colour="black")) + theme(axis.title.y = element_text(color = "black", size = 12, margin = margin(t = 15, r = 15, b = 15, l = 15))) + theme(axis.title.x = element_text(color = "black", size = 12, margin = margin(t = 15, r = 15, b = 15, l = 15))) + theme(legend.text = element_text(size = 11)) + theme(legend.title = element_text(size = 12)) + theme(axis.text.x=element_text(colour="black", size=12)) + theme(axis.text.y=element_text(colour="black", size=12)) + ggtitle("2020") + theme(plot.title = element_text(face = "bold", hjust = 0.5, size =18))

p2 

multi_plot<- ggarrange(p1,p2,
                       labels = c("", ""),  
                       ncol = 2, nrow = 1, 
                       common.legend = T,
                       legend = "bottom") 

multi_plot

##########################################################


#Depth frequency graphing

both2019 <- read.csv("FreqDepth2019.csv", fileEncoding="UTF-8-BOM")
head(both2019)
both2020 <- read.csv("FreqDepth2020.csv", fileEncoding="UTF-8-BOM")
head(both2020)

both2020$Year <- as.factor(both2020$Year)
both2020$Site <- as.factor(both2020$Site)
both2020$Depth <- as.factor(both2020$Depth)
both2019$Year <- as.factor(both2019$Year)
both2019$Site <- as.factor(both2019$Site)
both2019$Depth <- as.factor(both2019$Depth)

p3 <-ggplot(both2019, aes(x=Depth, y=Frequency, fill=Site)) +
  geom_bar(position=position_dodge(), stat="identity",
           colour="black",
           size=.3) +      
  xlab("Max. Dive Depth (m)") +
  ylab("Frequency (%)")+
  scale_fill_grey(name="Site", # Legend label, use darker colors
                  breaks=c("Harrison Cove", "Moraine"),
                  labels=c("Harrison Cove", "Moraine")) + theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + scale_x_discrete("Max. Dive Depth (m)", labels = c("10" = "<10","20" = "10-20", "30" = "20-30","40" = "30-40","50" = "40-50", "60" = "50-60","70" = "60-70", "80" = "70-80",  "90" = "80-90", "100" = "90-100", "110" = ">100", line =5)) + theme(axis.title.y = element_text(margin = margin(t = 15, r = 15, b = 15, l = 15))) + coord_cartesian(ylim = c(0, 49)) + theme(axis.title.y = element_text(color = "black", size = 12, margin = margin(t = 15, r = 15, b = 15, l = 15))) + theme(axis.title.x = element_text(color = "black", size = 12, margin = margin(t = 15, r = 15, b = 15, l = 15))) + theme(legend.text = element_text(size = 12)) + theme(legend.title = element_text(size = 12)) + theme(axis.text.x=element_text(colour="black")) + theme(axis.title.y = element_text(color = "black", size = 12, margin = margin(t = 15, r = 15, b = 15, l = 15))) + theme(axis.title.x = element_text(color = "black", size = 12, margin = margin(t = 15, r = 15, b = 15, l = 15))) + theme(legend.text = element_text(size = 11)) + theme(legend.title = element_text(size = 13)) + theme(axis.text.x=element_text(colour="black", size=12, angle = 45, hjust = 1)) + theme(axis.text.y=element_text(colour="black", size=12)) + ggtitle("2019") + theme(plot.title = element_text(face = "bold", hjust = 0.5, size =30))

p3

p4 <-ggplot(both2020, aes(x=Depth, y=Frequency, fill=Site)) +
  geom_bar(position=position_dodge(), stat="identity",
           colour="black",
           size=.3) +      
  xlab("Max. Dive Depth (m)") +
  ylab("Frequency (%)")+
  scale_fill_grey(name="Site", # Legend label, use darker colors
                  breaks=c("Harrison Cove", "Moraine"),
                  labels=c("Harrison Cove", "Moraine")) + theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + scale_x_discrete("Max. Dive Depth (m)", labels = c("10" = "<10","20" = "10-20", "30" = "20-30","40" = "30-40","50" = "40-50", "60" = "50-60","70" = "60-70", "80" = "70-80",  "90" = "80-90", "100" = "90-100", "110" = ">100", line =5)) + theme(axis.title.y = element_text(margin = margin(t = 15, r = 15, b = 15, l = 15))) + coord_cartesian(ylim = c(0, 49)) + theme(axis.title.y = element_text(color = "black", size = 12, margin = margin(t = 15, r = 15, b = 15, l = 15))) + theme(axis.title.x = element_text(color = "black", size = 12, margin = margin(t = 15, r = 15, b = 15, l = 15))) + theme(legend.text = element_text(size = 12)) + theme(legend.title = element_text(size = 12)) + theme(axis.text.x=element_text(colour="black")) + theme(axis.title.y = element_text(color = "black", size = 12, margin = margin(t = 15, r = 15, b = 15, l = 15))) + theme(axis.title.x = element_text(color = "black", size = 12, margin = margin(t = 15, r = 15, b = 15, l = 15))) + theme(legend.text = element_text(size = 11)) + theme(legend.title = element_text(size = 15)) + theme(axis.text.x=element_text(colour="black", size=12, angle = 45, hjust = 1)) + theme(axis.text.y=element_text(colour="black", size=12)) + ggtitle("2020") + theme(plot.title = element_text(face = "bold", hjust = 0.5, size =30))

p4


############################################################


#Dive depth by hour of day graphing


Tawa <- read.csv("TawakiDiveDatasetComplete.csv", fileEncoding="UTF-8-BOM")
head(Tawa)

both2019 <- Dive[c(2:15943),c(1:16)]
both2020 <- Dive[c(15943:32669),c(1:16)]

#summarizing data for graph (2019)

hr <- summarySE(both2019, measurevar="Depth", groupvars=c("Hour", "Site","birdID","TripNumber"))
hr$Site <- as.factor(hr$Site)
hr2 <- summarySE(hr, measurevar="Depth", groupvars=c("Hour", "Site","birdID"))
hr3 <- summarySE(hr2, measurevar="Depth", groupvars=c("Hour", "Site"))

hr3$Site <- as.factor(hr3$Site)

#plotting graph

p5 <-ggplot(hr3, aes(x=Hour, y=Depth, fill=Site)) + geom_rect(aes(xmin= -0.5, xmax = 5.5, ymin = 0, ymax = Inf), fill = 'grey', alpha = 0.05) + geom_rect(aes(xmin= 19.5, xmax = 24.5, ymin = 0, ymax = Inf), fill = 'grey', alpha = 0.05) +
  geom_bar(position=position_dodge(0.9), stat="identity",
           colour="black",
           size=.3) +      
  geom_errorbar(aes(ymin=Depth, ymax=Depth+se),
                size=.3,
                position=position_dodge(0.9)) +
  xlab("Hour of Day") +
  ylab("Dive Depth (m)") +
  scale_fill_grey(name="Site", # Legend label, use darker colors
                  breaks=c("Harrison Cove", "Moraine"),
                  labels=c("Harrison Cove", "Moraine")) + theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + theme(axis.title.y = element_text(margin = margin(t = 15, r = 15, b = 15, l = 15))) + theme(legend.text = element_text(size = 11)) + theme(legend.title = element_text(size = 12)) + theme(axis.text.x=element_text(colour="black", size=12)) + theme(axis.text.y=element_text(colour="black", size=12)) + scale_y_continuous(breaks = seq(0,100,by = 10)) + coord_cartesian(ylim = c(0, 75)) + theme(axis.title.x = element_text(color = "black", size = 12, margin = margin(t = 15, r = 15, b = 15, l = 15)))


p5

#summarizing data for graph (2020)

hr <- summarySE(both2020, measurevar="Depth", groupvars=c("Hour", "Site","birdID","TripNumber"))
hr$Site <- as.factor(hr$Site)
hr2 <- summarySE(hr, measurevar="Depth", groupvars=c("Hour", "Site","birdID"))
hr3 <- summarySE(hr2, measurevar="Depth", groupvars=c("Hour", "Site"))

hr3$Site <- as.factor(hr3$Site)

#plotting graph

p6 <- ggplot(hr3, aes(x=Hour, y=Depth, fill=Site)) + geom_rect(aes(xmin= -0.5, xmax = 5.5, ymin = 0, ymax = Inf), fill = 'grey', alpha = 0.05) + geom_rect(aes(xmin= 19.5, xmax = 24.5, ymin = 0, ymax = Inf), fill = 'grey', alpha = 0.05) +
  geom_bar(position=position_dodge(0.9), stat="identity",
           colour="black",
           size=.3) +      
  geom_errorbar(aes(ymin=Depth, ymax=Depth+se),
                size=.3,
                position=position_dodge(0.9)) +
  xlab("Hour of Day") +
  ylab("Dive Depth (m)") +
  scale_fill_grey(name="Site", # Legend label, use darker colors
                  breaks=c("Harrison Cove", "Moraine"),
                  labels=c("Harrison Cove", "Moraine")) + theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + theme(axis.title.y = element_text(margin = margin(t = 15, r = 15, b = 15, l = 15))) + theme(legend.text = element_text(size = 11)) + theme(legend.title = element_text(size = 12)) + theme(axis.text.x=element_text(colour="black", size=12)) + theme(axis.text.y=element_text(colour="black", size=12)) + scale_y_continuous(breaks = seq(0,100,by = 10)) + coord_cartesian(ylim = c(0, 75)) + theme(axis.title.x = element_text(color = "black", size = 12, margin = margin(t = 15, r = 15, b = 15, l = 15)))


p6


#adding multiple graphs together to display in a panel


multi_plot<- ggarrange(p3,p4,p5,p6,
                       labels = c("A", "", "B", ""), 
                       ncol = 2, nrow = 2, 
                       common.legend = T,
                       legend = "bottom") 

multi_plot 
