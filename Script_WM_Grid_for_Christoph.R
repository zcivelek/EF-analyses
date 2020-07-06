##WM Grid - code for Christoph

#load data
WM.grid <-read.csv("WM_grid.csv",header=TRUE, sep = ";")

#recoding variables
names(WM.grid)[1] <- "ID"
names(WM.grid)[15] <- "SecondTaskCorrect"

WM.grid$Age_group <- as.factor(WM.grid$Age_group)#this converts a variable classified as continuous into a categorical variable
WM.grid$ID <- as.factor(WM.grid$ID)#this converts a variable classified as continuous into a categorical variable
levels(WM.grid$Age_group)

str(WM.grid)

#remove dropouts and select test trials
test.trials<-subset(WM.grid, Phase == "Test" & Dropout == "no")

#LMM
#Can the distance be predicted by age?
library(lme4)

test.trials$z.age.midtesting=as.vector(scale(test.trials$AgeMonths_midtesting))
mean(test.trials$z.age.midtesting)#check whether it has worked
sd(test.trials$z.age.midtesting)

test.trials$z.Trial=as.vector(scale(test.trials$Trial))#transform trial number to mean of 0 and SD of 1


#Final full model:
dist<-lmer(distance_c01 ~ AgeMonths_midtesting + Trial + (1|ID) + (0+Trial|ID),
           data=test.trials, REML=FALSE)#singular fit

null.dist<-lmer(distance_c01 ~ 1 + (1|ID) + (0+Trial|ID), 
                data=test.trials, REML=FALSE)#singular fit

anova(null.dist, dist, test="Chisq")#significant

summary(dist)

as.data.frame(drop1(dist, test="Chisq"))

#The model including age and trial number can explain the data on distance significantly better than a model only containing the intercept, X2(2) = 16.673, p < .001. 
#Both trial number and age have significant effects on children's performance: 
#Trial number X2(1) = 3.965, p = .046
#Age in months X2(1) = 12.727, p < .001

#plot
source("../../../R scripts/Roger/boot_glmm.r")#We are interested in both, so we z-transform both
test.trials$z.age.midtesting=as.vector(scale(test.trials$AgeMonths_midtesting))#transform age 
test.trials$z.Trial=as.vector(scale(test.trials$Trial)) 

dist<-lmer(distance_c01 ~ z.age.midtesting + z.Trial + (1|ID) + (0+z.Trial|ID), data=test.trials, REML=FALSE)#singular fit

boot.res=boot.glmm.pred(model.res=dist, excl.warnings=T,
                        nboots=1000, para=F, resol=12, use="z.Trial")

plot.xvals=seq(from=min(test.trials$Trial), to=max(test.trials$Trial),
               length.out=12)

library(tidyverse)
ggplot(test.trials, aes(x=z.Trial, y=distance_c01)) + geom_point() +
  geom_jitter()#+ 
  #theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  #geom_ribbon(data=boot.res$ci.predicted, aes(x = boot.res$ci.predicted$z.Trial, ymin=boot.res$ci.predicted$lower.cl,
                  #ymax=boot.res$ci.predicted$upper.cl), fill="red", alpha=0.5)

ggplot(test.trials, aes(x=plot.xvals, y=boot.res$ci.predicted$fitted)) +
  geom_jitter(aes(x=Trial, y=distance_c01, colour=Age_group))+ 
  geom_ribbon(data=boot.res$ci.predicted, aes(x = plot.xvals, ymin=boot.res$ci.predicted$lower.cl,
                                              ymax=boot.res$ci.predicted$upper.cl), fill="grey", alpha=0.5)+
geom_line(data=boot.res$ci.predicted, aes(x = plot.xvals, y=boot.res$ci.predicted$fitted), lty=2)+
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  xlab("Trial number")+
  ylab("Distance")+
  xlim(0,13)

save.image("WM_grid_plot_children.RDATA")
