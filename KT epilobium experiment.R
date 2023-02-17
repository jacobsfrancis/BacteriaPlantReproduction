# 2022 kt data

dat <- read.csv("SEBECR Data Sheet - fordownload.csv")
dat$Plant<-as.factor(dat$Plant)
dat<-dat[dat$Total>0,]
library(lme4)
library(emmeans)
library(multcomp)

head(dat)

fit <- lm(Seed.Pod.Weight ~ Microbe.Type + Plant, data=dat)
summary(fit)
library(ggplot2)
ggplot(dat, aes(x=Microbe.Type, y=Seed.Pod.Weight))+
  geom_boxplot()+facet_wrap(~Plant)
  



fit <- lm(X..Healthy.of.Total ~ Microbe.Type * Plant, data=dat)
summary(fit)
anova(fit)


# first lets see if the number of seeds differs!

seedNum <- glmer(data=dat, Total~Microbe.Type+(1|Plant),family=poisson)
drop1(seedNum,test="Chi")
summary(seedNum)

seedNumPlot <- ggplot(dat,aes(x=Microbe.Type,y=Total,color=Microbe.Type))

seedNumPlot + geom_point(position=position_jitter(.2))

healthy <- glm(data=dat,Unhealthy/Total~Microbe.Type*Plant,weights=Total,family=binomial)
drop1(healthy,.~.,test="F")
cld(emmeans(healthy,~.(Microbe.Type,Plant)))

healthy <- glm(data=dat,Unhealthy/Total~Microbe.Type+Plant+Total,weights=Total,family=binomial)
drop1(healthy,test="Chi")
cld(emmeans(healthy,~Microbe.Type))

weight <- lm(data=dat,Seed.Pod.Weight~Microbe.Type+Plant)
drop1(weight,.~.,test="Chi")

seedNumPlot <- ggplot(dat,aes(x=Microbe.Type,y=Seed.Pod.Weight,color=Microbe.Type))
seedNumPlot + geom_point(position=position_jitter(.2)) +facet_wrap(~Plant)


ggplot(data=dat, aes(x=Seed.Pod.Weight,y=Total))+geom_point() +geom_smooth(method="lm")
