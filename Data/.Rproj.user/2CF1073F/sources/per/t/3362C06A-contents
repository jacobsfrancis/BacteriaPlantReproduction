
library(tidyverse)
library(magrittr)
library(lme4)
library(emmeans)
library(emmGrid)
library(multcompView)
library(gridExtra)
####Analysis 2 Fake Plants####

# read in the data

data <- read.csv("PSG_INOC_2021_08 - Data_Final.csv")
str(data)

data<-data[,c("Plant","Microbe","Flower","EPI_STIGMA","HETERO_STIGMA","EPI_RINSE","HETERO_RINSE")]


data %<>% mutate(TOTAL_EPI = EPI_STIGMA+EPI_RINSE, TOTAL_HETERO=HETERO_STIGMA+HETERO_RINSE) 
data$Plant<-as.factor(data$Plant)



plot2 <- ggplot(data,aes(x=Microbe, y=TOTAL_HETERO))
plot2+geom_point(position=position_dodge(.5))+
  scale_y_log10()



model <- glmer(data=data, TOTAL_EPI~Microbe+(1|Plant),family="poisson")
summary(model)
drop1(model,test="Chisq")
emm1 <- emmeans(model, specs="Microbe",type="response")
modeledMeans <- data.frame(emm1)
pairs(emm1,adjust="bonferroni")

model2 <- glmer(data=data, TOTAL_HETERO~Microbe+(1|Plant),family="poisson")
summary(model2)
drop1(model2,test="Chisq")
emm2 <- emmeans(model2, specs="Microbe",type="response")
modeledMeans2 <- data.frame(emm2)
pairs(emm2,adjust="bonferroni")

control1 <- modeledMeans$rate[modeledMeans$Microbe=="C"]
control2 <- modeledMeans2$rate[modeledMeans2$Microbe=="C"]

plot1 <- ggplot(data,aes(x=Microbe, y=TOTAL_EPI))




ConspecificPlot <- plot1 + geom_point(aes(color=as.factor(Plant)), position=position_dodge(.9))+
  scale_y_log10("Conspecific Pollen Receipt (grains)")+
  geom_point(data=modeledMeans, aes(x=Microbe,y=rate),size=5)+
  scale_x_discrete("",labels=c(rep("",5)))+
  geom_errorbar(data=modeledMeans, aes(y=rate,ymin=asymp.LCL,ymax=asymp.UCL),width=.4, size=1.5)+
  geom_point(data=modeledMeans, aes(x=Microbe,y=rate),size=2,color="lightgrey")+
  theme(legend.position = "none", axis.text.y=element_text(angle=90,hjust = .5))+
  geom_text(data=data.frame(
    Microbe=c("112","124","187","477","C"), 
    TOTAL_EPI=rep(1000,5),
    label=c("A","B","C","D","D")),aes(label=label,y=TOTAL_EPI,x=Microbe),size=5)
ConspecificPlot
nocontrol<-data[data$Microbe!="C",]
plot1.2 <- ggplot(nocontrol,aes(x=Microbe, y=TOTAL_EPI-control1))

ConspecificPlot <- plot1.2 + 
  scale_y_continuous("Conspecific Pollen\n(modeled difference from control)")+
  geom_point(data=modeledMeans[modeledMeans$Microbe!="C",] ,aes(x=Microbe,y=rate-control1),size=5)+
  scale_x_discrete("",labels=c(rep("",4)))+
  geom_errorbar(data=modeledMeans[modeledMeans$Microbe!="C",], aes(y=rate-control1,ymin=asymp.LCL-control1,ymax=asymp.UCL-control1),width=.4, size=1.5)+
  geom_point(data=modeledMeans[modeledMeans$Microbe!="C",], aes(x=Microbe,y=rate-control1),size=2,color="lightgrey")+
  theme(legend.position = "none", axis.text.y=element_text(angle=90,hjust = .5))+
  geom_text(data=data.frame(
    Microbe=c("112","124","187","477"), 
    TOTAL_EPI=rep(42,4),
    label=c("A","B","C","D")),aes(label=label,y=TOTAL_EPI,x=Microbe),size=5)
ConspecificPlot



HeterospecificPlot <- plot2 + geom_point(aes(color=as.factor(Plant)), position=position_dodge(.9))+
  scale_y_log10("Heterospecific Pollen Receipt (grains)")+
  geom_point(data=modeledMeans2, aes(x=Microbe,y=rate),size=5)+
  geom_errorbar(data=modeledMeans2, aes(y=rate,ymin=asymp.LCL,ymax=asymp.UCL),width=.4, size=1.5)+
  scale_x_discrete(labels=c("Neokomagatea","Rosenbergiella","Pantoea","Acinetobacter","Control"))+
  geom_point(data=modeledMeans2, aes(x=Microbe,y=rate),size=2,color="lightgrey")+
  theme(legend.position = "none", axis.text.y=element_text(angle=90,hjust = .5))+
  geom_text(data=data.frame(
    Microbe=c("112","124","187","477","C"), 
    TOTAL_EPI=rep(100,5),
    label=c("A","B","C","D","C")),aes(label=label,y=TOTAL_EPI,x=Microbe),size=5)
HeterospecificPlot

grid.arrange(ConspecificPlot,HeterospecificPlot,nrow=2)
  

nocontrol<-data[data$Microbe!="C",]
plot1.2 <- ggplot(nocontrol,aes(x=Microbe, y=TOTAL_EPI-control1))

ConspecificPlot2 <- plot1.2 + 
  scale_y_continuous("Conspecific Pollen\n(modeled difference from control)",limits=c(-40,43))+
  geom_point(data=modeledMeans[modeledMeans$Microbe!="C",] ,aes(x=Microbe,y=rate-control1),size=5)+
  scale_x_discrete(labels=c("Neokomagatea","Rosenbergiella","Pantoea","Acinetobacter","Control"))+
  geom_errorbar(data=modeledMeans[modeledMeans$Microbe!="C",], aes(y=rate-control1,ymin=asymp.LCL-control1,ymax=asymp.UCL-control1),width=.4, size=1.5)+
  geom_point(data=modeledMeans[modeledMeans$Microbe!="C",], aes(x=Microbe,y=rate-control1),size=2,color="lightgrey")+
  theme(legend.position = "none", axis.text.y=element_text(angle=90,hjust = .5))+
  geom_text(data=data.frame(
    Microbe=c("112","124","187","477"), 
    TOTAL_EPI=rep(42,4),
    label=c("*","*","*","")),aes(label=label,y=TOTAL_EPI,x=Microbe),size=5)+
  geom_hline(yintercept = 0, linetype="dashed")
ConspecificPlot2

nocontrol2 <- modeledMeans2[modeledMeans2$Microbe!="C",]

plot2.2 <- ggplot(nocontrol2,aes(x=Microbe, y=rate))
HeterospecificPlot2 <- plot1.2 + 
  scale_y_continuous("Heterospecific Pollen\n(modeled difference from control)",limits = c(-40,43))+
  geom_point(data=nocontrol2 ,aes(x=Microbe,y=rate-control2),size=5)+
  scale_x_discrete(labels=c("Neokomagatea","Rosenbergiella","Pantoea","Acinetobacter","Control"))+
  geom_errorbar(data=nocontrol2, aes(y=rate-control2,ymin=asymp.LCL-control2,ymax=asymp.UCL-control2),width=.4, size=1.5)+
  geom_point(data=nocontrol2, aes(x=Microbe,y=rate-control2),size=2,color="lightgrey")+
  theme(legend.position = "none", axis.text.y=element_text(angle=90,hjust = .5))+
  geom_text(data=data.frame(
    Microbe=c("112","124","187","477"), 
    TOTAL_EPI=rep(42,4),
    label=c("*","*","","*")),aes(label=label,y=TOTAL_EPI,x=Microbe),size=10)+
  geom_hline(yintercept = 0, linetype="dashed")
HeterospecificPlot2

pdf("Plots/ChangeVControl.pdf",w=8.5,h=4)
grid.arrange(ConspecificPlot2,HeterospecificPlot2,nrow=1)
dev.off()

pdf("Plots/FakePlantsPlot.pdf",h=8,w=6)
grid.arrange(ConspecificPlot,HeterospecificPlot,nrow=2)
dev.off()


data%<>%mutate(COMBO = (TOTAL_EPI-TOTAL_HETERO)/(TOTAL_EPI+TOTAL_HETERO))
plot3<-ggplot(data=data, aes(x=Microbe, y=COMBO))
plot3+geom_point(position=position_jitter(.2))+geom_smooth()

mod3 <- lmer(data=data,COMBO~Microbe+(1|Plant))
drop1(mod3, test="Chisq")
emm3<-emmeans(mod3,specs="Microbe")
modeledMeans3<-data.frame(emm3)
pairs(emm3,correction="bonferroni")


CombinedPollenPlot <- plot3 + geom_point(aes(color=as.factor(Plant)),position=position_dodge(.9))+
  geom_errorbar(data=modeledMeans3,aes(y=emmean,ymin=asymp.LCL,ymax=asymp.UCL),width=.4,size=1.5)+
  geom_point(data=modeledMeans3,aes(y=emmean),size=5)+
  geom_point(data=modeledMeans3,aes(y=emmean),size=2,color="lightgrey")+
  scale_x_discrete(labels=c(
    expression(italic(Neokomagatea)),
    expression(italic(Rosenbergiella)),
    expression(italic(Pantoea)),
    expression(italic(Acinetobacter)),
    "Control"))+
  scale_y_continuous("Relative Pollen Receipt",breaks=c(-1,0,1),labels=c("All Hetero","Even","All Con"))+
  theme(legend.position = "none", 
        axis.text.y=element_text(angle=90,hjust = .5),
        axis.text.x=element_text(angle=90, hjust=1))+
  geom_text(data=data.frame(
    Microbe=c("112","124","187","477","C"), 
    TOTAL_EPI=rep(1.1,5),
    label=c("AC","AC","A","C","AC*")),aes(label=label,y=TOTAL_EPI,x=Microbe),size=5)
CombinedPollenPlot

pdf("Plots/CombinedPlot.pdf", h=6,w=6)
CombinedPollenPlot
dev.off()

pdf("Plots/SummaryPlot.pdf",h=10,w=4)
grid.arrange(ConspecificPlot +  theme(plot.margin = margin(t = 10,  # Top margin
                                                           r = 10,  # Right margin
                                                           b = -15,  # Bottom margin
                                                           l = 10)), # Left margin)
             
             HeterospecificPlot+scale_x_discrete("",labels=c("","","","",""))+
               theme(plot.margin = margin(t = 0,  # Top margin
                                          r = 10,  # Right margin
                                          b = -15,  # Bottom margin
                                          l = 10)), # Left margin)
             CombinedPollenPlot+  theme(plot.margin = margin(t = 0,  # Top margin
                                                             r = 10,  # Right margin
                                                             b = 0,  # Bottom margin
                                                             l = 10)),
             nrow=3,
              heights=c(3,3,4.2))
dev.off()

