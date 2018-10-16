rm(list=ls(all=TRUE)) # clean

library(tidyverse)
library(dplyr)
library(tidyr)
library(readr)
library(ggplot2) # plot the data
library(lme4); library(lmerTest) # used for mixed models
library(sjPlot)
##Study1 - Data import ################
study_raw = read_csv("D:/Experiment Data/Navigation and Self/GridCode_Analysis/Memory_data_fMRI_0502.csv")
study1 <- study_raw

study1$Condition = factor(study1$Condition)
study1$Group = ifelse((study1$Subject)%%2 == 1, 1, 2) # Body-first or Nobody-first
study1$Subject  = factor(study1$Subject)
study1$Round_fac = factor(study1$Round)
study1$Index_fac = factor(study1$Index)
study1$TimeSpent = study1$EndTime - study1$StartTime # Timespent during retrieval
study1$FBTimeSpent = study1$FB_EndTime - study1$EndTime # Timespent during Feedback
#study1$estimation_dError = study1$estimation_dError*2 # should be dobled (recorded value to displayed value)
study1$ErrorMetaCog = study1$dError - study1$estimation_dError
study1$Position = factor(study1$Position)

study_encoding = read_csv("D:/Experiment Data/Navigation and Self/GridCode_Analysis/Memory_data_fMRI_Encoding_0502.csv")

study_encoding$Subject  = factor(study_encoding$Subject)
study_encoding$Round_fac = factor(study_encoding$Round)

########################################
# Exclusion1 - bad participant, if exists 
#study1 <- subset(study1, Subject != 16)

# Exclusion2 - manipulation error - miss-clicked
study1_miss <- subset(study1,((dMoved < 5 | nManipulation < 1 | TimeSpent < 3.5) & (estimation_dError > 50 | dError > 25))| estimation_dError > 70)
study1_excl <- subset(study1,!(((dMoved < 5 | nManipulation < 1 | TimeSpent < 3.5) & (estimation_dError > 50 | dError > 25))| estimation_dError > 70))

# Exclusion3 - Round based (dError > 55)
threshold = 55;

## extract round info for Study1
a <- study1_excl
a <- subset(a,  Index != 1 & Index != 14) # first trial - start from the center/ last trial - with virtual threat

NormalizedData <-data.frame() ## Normalization within subject
for (i in 1:27)
{
    temp <- subset(a, Subject == i)
    temp2 <- cbind(scale(temp$dError),scale(temp$dMoved), scale(temp$estimation_dError))
    NormalizedData <- rbind(NormalizedData,temp2)
}

a$distNorm <- NormalizedData$V1
a$dMovedNorm <- NormalizedData$V2
a$estimationNorm <- NormalizedData$V3

study1_excl <- a

rounddata_median = aggregate(cbind(dError,distNorm,Condition,Group,estimation_dError,ErrorMetaCog,Q1,Q2,Q3,Q4,Q1z,Q2z,Q3z,Q4z) ~ Subject + Round_fac,a,median)
rounddata_mean = aggregate(cbind(dError,distNorm,Condition,Group,estimation_dError,ErrorMetaCog,Q1,Q2,Q3,Q4,Q1z,Q2z,Q3z,Q4z) ~ Subject + Round_fac,a,mean)

rounddata_median1 = aggregate(cbind(dError,distNorm,Condition,Group,estimation_dError,ErrorMetaCog) ~ Subject + Round_fac,a,median)
rounddata_mean1 = aggregate(cbind(dMoved,TimeSpent,directionChange,nManipulation,dMovedNorm,Q1,Q2,Q3,Q4,Q1z,Q2z,Q3z,Q4z) ~ Subject + Round_fac,a,mean) 
rounddata <- merge(rounddata_median1,rounddata_mean1) # should think which method(mean/median) is better for each category

round_excl1 <- subset(rounddata,dError >= threshold ) # threshold = 55
threshold_IQR1 = IQR(rounddata$dError)*2.5
round_aboveIQR1 <- subset(rounddata,dError >= threshold_IQR1)

round_info <- merge(study_encoding,rounddata)
round_info_exclIQR <- subset(round_info, round_info$dError < threshold_IQR1)

round_mean_sd <- round_info %>% group_by(Condition) %>% summarise_each(funs(mean,sd,se=sd(.)/sqrt(n())))

study1_exclIQR <- study1_excl
## round-wise exclusion (dError >= IQR*2.5 )
for (i in 1:length(round_aboveIQR1[[1]]))
{
  study1_exclIQR <- subset(study1_exclIQR, !(Subject == round_aboveIQR1[i,1] & Round == round_aboveIQR1[i,2]))
}

## confirm Round effect
round_info$Condition = ifelse(round_info$Condition == 1, 'No-body','Body')

ggplot(round_info, aes(y=distNorm, x=Round_fac, color=Round_fac)) + geom_boxplot(outlier.shape = NA) + geom_jitter(width=.1,height=0,alpha=.5,size=1)
ggplot(round_info, aes(y=dMovedNorm, x=Round_fac, color=Round_fac)) + geom_boxplot(outlier.shape = NA) + geom_jitter(width=.1,height=0,alpha=.5,size=1)
ggplot(round_info, aes(y=TimeSpent, x=Round_fac, color=Round_fac)) + geom_boxplot(outlier.shape = NA) + geom_jitter(width=.1,height=0,alpha=.5,size=1)

############## Assess Distribution
a <- study1_excl # study1_exclIQR
a$Condition = ifelse(a$Condition == 1, 'No-body','Body')
#a$Group =  ifelse(a$Group == 1, 'No-body First', 'Body First')

ggplot(a, aes(x=log(dError),y=..density.., group=Condition,  color=Condition)) + geom_density() #+ facet_wrap(~Subject,scales='free')

ggplot(a, aes(x=dError,y=..density.., group=Condition,  color=Condition)) + geom_density() #+ facet_wrap(~Subject,scales='free')
ggplot(a, aes(x=distNorm,y=..density.., group=Condition,  color=Condition)) + geom_density()# + facet_wrap(~Subject,scales='free')
ggplot(a, aes(x=dMoved,y=..density.., group=Condition,  color=Condition)) + geom_density()
ggplot(a, aes(x=estimation_dError,y=..density.., group=Condition,  color=Condition)) + geom_density()
ggplot(a, aes(x=ErrorMetaCog,y=..density.., group=Condition,  color=Condition)) + geom_density() # + facet_wrap(~Subject,scales='free')

############## Plot difference

indivdata_withoutQ = aggregate(cbind(Group,dError,distNorm,dMoved,dMovedNorm,TimeSpent,directionChange,nManipulation,totalAngle,estimation_dError,estimationNorm,ErrorMetaCog) ~ Condition + Subject,a,median)
indivdata_mean_withoutQ = aggregate(cbind(Group,dError,distNorm,dMoved,dMovedNorm,TimeSpent,directionChange,nManipulation,totalAngle, estimation_dError,estimationNorm,ErrorMetaCog) ~ Condition + Subject,a,mean)

indivdata = aggregate(cbind(Group,dError,distNorm,dMoved,dMovedNorm,TimeSpent,directionChange,nManipulation,totalAngle,estimation_dError,estimationNorm,ErrorMetaCog,Q1,Q2,Q3,Q4,Q1z,Q2z,Q3z,Q4z) ~ Condition + Subject,a,median)
indivdata_mean = aggregate(cbind(Group,dError,distNorm,dMoved,dMovedNorm,TimeSpent,directionChange,nManipulation,totalAngle, estimation_dError,estimationNorm,ErrorMetaCog,Q1,Q2,Q3,Q4,Q1z,Q2z,Q3z,Q4z) ~ Condition + Subject,a,mean)
indivdata_index = aggregate(cbind(dError,distNorm,dMoved,TimeSpent,directionChange,nManipulation,estimation_dError,estimationNorm,ErrorMetaCog,Q1,Q2,Q3,Q4,Q1z,Q2z,Q3z,Q4z) ~ Condition + Index,a,median)
indivdata_mean_index = aggregate(cbind(dError,distNorm,dMoved,TimeSpent,directionChange,nManipulation,estimation_dError,estimationNorm,ErrorMetaCog,Q1,Q2,Q3,Q4,Q1z,Q2z,Q3z,Q4z) ~ Condition + Index,a,mean)

# accord to trial Index (2 to 13)
ggplot(indivdata_index, aes(y=distNorm, x=Index, color=Condition)) + geom_line()+ geom_point()
ggplot(indivdata_mean_index, aes(y=dError, x=Index, color=Condition)) + geom_line()+ geom_point()
ggplot(indivdata_index, aes(y=estimation_dError, x=Index, color=Condition)) + geom_line()+ geom_point()
ggplot(indivdata_index, aes(y=ErrorMetaCog, x=Index, color=Condition)) + geom_line()+ geom_point()

ggplot(indivdata, aes(y=distNorm, x=Condition, color=Condition)) + geom_boxplot()+ facet_wrap(~Subject)
ggplot(indivdata, aes(y=distNorm, x=Condition, color=Condition)) + geom_boxplot()+ geom_jitter(width=.1,height=0,alpha=.5,size=2)

indivdata <- indivdata %>% group_by(Subject) %>% mutate(Body_Effect = (distNorm[Condition=='No-body'] - distNorm[Condition=='Body']))
indivdata_mean <- indivdata_mean %>% group_by(Subject) %>% mutate(Body_Effect = (distNorm[Condition=='No-body'] - distNorm[Condition=='Body']))

indivdata_withoutQ <- indivdata_withoutQ %>% group_by(Subject) %>% mutate(Body_Effect = (distNorm[Condition=='No-body'] - distNorm[Condition=='Body']))
indivdata_mean_withoutQ <- indivdata_mean_withoutQ %>% group_by(Subject) %>% mutate(Body_Effect = (distNorm[Condition=='No-body'] - distNorm[Condition=='Body']))
temp <- indivdata_mean_withoutQ
p <-ggplot(temp, aes(y=dError, x=Condition, group=Subject, color=Condition)) + geom_line(aes(color=Body_Effect>0),size = 0.8) + geom_point(size = 2.5) + scale_color_manual(values=c("#F8766D", "#00BFC4", "#00BFC4", "#F8766D")) #+ geom_point(aes(color=Condition), size=2.5)

p + theme(
  axis.title.x = element_text(size=0, face="bold"),
  axis.title.y = element_text(size=22, face="bold"),
  axis.text = element_text(size=20),
  legend.position = "none",
  
  legend.text = element_text(size=22),
  legend.title = element_text(size=18)
)

indiv_mean_sd <- indivdata_mean %>% group_by(Condition) %>% summarise_each(funs(mean,sd,se=sd(.)/sqrt(n())))
indiv_median_mean_sd <- indivdata %>% group_by(Condition) %>% summarise_each(funs(mean,sd,se=sd(.)/sqrt(n())))

ggplot(indiv_mean_sd, aes(y=dError_mean, x=Condition,fill=Condition)) + geom_bar(stat="identity",width=.6) + geom_errorbar(aes(ymin=dError_mean-dError_se,ymax=dError_mean+dError_se), size = .3, width=.2,position=position_dodge(.9)) #+ ylim(0,20)
ggplot(indiv_mean_sd, aes(y=distNorm_mean, x=Condition,fill=Condition)) + geom_bar(stat="identity",width=.6) + geom_errorbar(aes(ymin=distNorm_mean-distNorm_se,ymax=distNorm_mean+distNorm_se), size = .3, width=.2,position=position_dodge(.9)) # ylim(-0.17,0.17)
p <- ggplot(indiv_mean_sd, aes(y=ErrorMetaCog_mean, x=Condition,fill=Condition)) + geom_bar(stat="identity",width=.6) + geom_errorbar(aes(ymin=ErrorMetaCog_mean-ErrorMetaCog_se,ymax=ErrorMetaCog_mean+ErrorMetaCog_se), size = .3, width=.2,position=position_dodge(.9)) # ylim(-0.17,0.17)
p <- ggplot(indiv_mean_sd, aes(y=dMoved_mean, x=Condition,fill=Condition)) + geom_bar(stat="identity",width=.6) + geom_errorbar(aes(ymin=dMoved_mean-dMoved_se,ymax=dMoved_mean+dMoved_se), size = .3, width=.2,position=position_dodge(.9))  + coord_cartesian(ylim=c(70,90))
p <- ggplot(indiv_mean_sd, aes(y=TimeSpent_mean, x=Condition,fill=Condition)) + geom_bar(stat="identity",width=.6) + geom_errorbar(aes(ymin=TimeSpent_mean-TimeSpent_se,ymax=TimeSpent_mean+TimeSpent_se), size = .3, width=.2,position=position_dodge(.9))  + coord_cartesian(ylim=c(12.5,17.5))

ggplot(indiv_median_mean_sd, aes(y=dError_mean, x=Condition,fill=Condition)) + geom_bar(stat="identity",width=.6) + geom_errorbar(aes(ymin=dError_mean-dError_se,ymax=dError_mean+dError_se), size = .3, width=.2,position=position_dodge(.9)) 
ggplot(indiv_median_mean_sd, aes(y=distNorm_mean, x=Condition,fill=Condition)) + geom_bar(stat="identity",width=.6) + geom_errorbar(aes(ymin=distNorm_mean-distNorm_se,ymax=distNorm_mean+distNorm_se), size = .3, width=.2,position=position_dodge(.9)) # ylim(-0.17,0.17)
ggplot(indiv_median_mean_sd, aes(y=ErrorMetaCog_mean, x=Condition,fill=Condition)) + geom_bar(stat="identity",width=.6) + geom_errorbar(aes(ymin=ErrorMetaCog_mean-ErrorMetaCog_se,ymax=ErrorMetaCog_mean+ErrorMetaCog_se), size = .3, width=.2,position=position_dodge(.9)) # ylim(-0.17,0.17)
ggplot(indiv_median_mean_sd, aes(y=dMoved_mean, x=Condition,fill=Condition)) + geom_bar(stat="identity",width=.6) + geom_errorbar(aes(ymin=dMoved_mean-dMoved_se,ymax=dMoved_mean+dMoved_se), size = .3, width=.2,position=position_dodge(.9))  + coord_cartesian(ylim=c(70,90))

#Difference during Encoding
ggplot(round_mean_sd, aes(y=EN_TimeSpent_mean, x=Condition, fill=Condition)) + geom_bar(stat="identity",width=.6) + geom_errorbar(aes(ymin=EN_TimeSpent_mean-EN_TimeSpent_se,ymax=EN_TimeSpent_mean+EN_TimeSpent_se), size = .3, width=.2,position=position_dodge(.9)) 
ggplot(round_mean_sd, aes(y=EN_dMoved_mean, x=Condition,fill=Condition)) + geom_bar(stat="identity",width=.6) + geom_errorbar(aes(ymin=EN_dMoved_mean-EN_dMoved_se,ymax=EN_dMoved_mean+EN_dMoved_se), size = .3, width=.2,position=position_dodge(.9)) 
ggplot(round_mean_sd, aes(y=EN_nDirectionChange_mean, x=Condition,fill=Condition)) + geom_bar(stat="identity",width=.6) + geom_errorbar(aes(ymin=EN_nDirectionChange_mean-EN_nDirectionChange_se,ymax=EN_nDirectionChange_mean+EN_nDirectionChange_se), size = .3, width=.2,position=position_dodge(.9)) 
ggplot(round_mean_sd, aes(y=EN_nManipulation_mean, x=Condition,fill=Condition)) + geom_bar(stat="identity",width=.6) + geom_errorbar(aes(ymin=EN_nManipulation_mean-EN_nManipulation_se,ymax=EN_nManipulation_mean+EN_nManipulation_se), size = .3, width=.2,position=position_dodge(.9)) 

## MetaCognition
#a2 <- subset(a, a$estimation_dError > 1)
ggplot(a, aes(x=dError, y = estimation_dError, color = Condition)) + geom_point() + geom_smooth(method='lm') + facet_wrap(~Subject,scales='free')
ggplot(a, aes(x=dError, y = estimation_dError, color = Condition)) + geom_point() + geom_smooth(method='lm') 

ggplot(a,aes(x=estimation_dError,y=dError,color=Condition,fill=Condition)) + geom_point() + geom_smooth(method='lm') +
  geom_abline(slope=1,linetype='dashed') + xlim(0,110) + ylim(0,110) +
  ylab("Actual error (m)") + xlab('Estimated error (m)') #+ facet_wrap(~Subject)
ggplot(a,aes(x=dError,y=estimation_dError,color=Condition,fill=Condition)) + geom_point() + geom_smooth(method='lm') +
  geom_abline(slope=1,linetype='dashed') + xlim(0,110) + ylim(0,110) +
  ylab("Estimated error (m)") + xlab('Actual error(m)') #+ facet_wrap(~Subject)


## Body-first vs. Nobody-first : Group
ggplot(a, aes(x=dError,y=..density.., group=Condition,  color=Condition)) + geom_density() + facet_wrap(~Group,scales='free')
ggplot(a, aes(x=distNorm,y=..density.., group=Group,  color=Group)) + geom_density() 

#Other random factors
ggplot(a, aes(y=distNorm, x=Round_fac, color=Round_fac)) + geom_boxplot()
ggplot(a, aes(y=distNorm, x=Position, color=Position)) + geom_boxplot()
ggplot(a, aes(y=distNorm, x=Object, color=Object)) + geom_boxplot()

###################### Assess Difference : mixed model

binom.test(19,27)

m1 = lmer(dError ~ Condition +  (1|Subject),a)
m2 = lmer(dError ~ Condition +  (Condition|Subject),a)

a$Position <- factor(a$Position)
m1 = glmer(dError ~ Condition + Index + Round + (1|Subject),a, family=Gamma(link='log'))
m2 = glmer(dError ~ Condition + Index + Round + (Condition|Subject),a,family=Gamma(link='log'))

m1 = glmer(dError ~ Condition + Index + Round + (1|Subject),a, family=gaussian(link='log'))
m2 = glmer(dError ~ Condition + Index + Round + (Condition|Subject),a,family=gaussian(link='log'))


m1 = glmer(dError ~ Condition * Index + Round + (1|Subject),a, family=Gamma(link='log'))
m2 = glmer(dError ~ Condition * Index + Round + (Condition|Subject),a,family=Gamma(link='log'))
# m3 = glmer(dError ~ Condition*Index + Round + (Condition|Subject),a,family=Gamma(link='log'))

m1 = glmer(dError ~ Condition  + Round + Index + (1|Subject),a, family=Gamma())
m4 = glmer(dError ~ Condition  + Round + Index + (Condition|Subject),a,family=Gamma())

m1 = glmer(dError ~ Condition  + (1|Subject),a, family=Gamma())
m2 = glmer(dError ~ Condition  + (Condition|Subject),a,family=Gamma())

#TimeSpent
m1 = lmer(TimeSpent ~ Condition  + (1|Subject),a)
m2 = lmer(TimeSpent ~ Condition  + (Condition|Subject),a)

m1 = lmer(directionChange ~ Condition+  (1|Subject),a)
m2 = lmer(directionChange ~ Condition+ (Condition|Subject),a)

m1 = lmer(ErrorMetaCog ~ Condition+  (1|Subject),a)
m2 = lmer(ErrorMetaCog ~ Condition+ (Condition|Subject),a)

anova(m1,m2)
summary(m1)
anova(m1)
summary(m2)
anova(m2)

sjp.lmer(m1)
sjp.glmer(m2,type = "pred", var = "Condition")
sjp.glmer(m2,type = "rs.ri")

sjp.glmer(m2,type = "eff")
plot_model(m2)
plot_model(m2,type = "pred", terms = "Condition")


plot(fitted(m1),residuals(m1))
hist(residuals(m1))
qqnorm(residuals(m1))

plot(fitted(m2),residuals(m2))
hist(residuals(m2))
qqnorm(residuals(m2))

###################### Wilcoxon 
b = indivdata_withoutQ #indivdata_mean_withoutQ

wilcox.test(b$dError[b$Condition=='Body'],b$dError[b$Condition=='No-body'],paired=T)

wilcox.test(b$distNorm[b$Condition=='Body'],b$distNorm[b$Condition=='No-body'],paired=T)
wilcox.test(b$ErrorMetaCog[b$Condition=='Body'],b$ErrorMetaCog[b$Condition=='No-body'],paired=T)
wilcox.test(b$dMoved[b$Condition=='Body'],b$dMoved[b$Condition=='No-body'],paired=T)
wilcox.test(1/b$dError[b$Condition=='Body'],1/b$dError[b$Condition=='No-body'],paired=T)

###################### t.test
t.test(b$distNorm[b$Condition=='Body'],b$distNorm[b$Condition=='No-body'],paired=T)

####### Questionnaire
weird_subj <- subset(indivdata_mean, indivdata_mean$Q4 >= -1)
sub_round_info <- subset(round_info, Subject != 1 & Subject != 25 & Subject != 17 & Subject != 26) # 1, 11, 17, 25, 26
indivQ = aggregate(cbind(Q1z,Q2z,Q3z,Q4z) ~ Condition + Subject,sub_round_info,mean)
indivQ =  gather(indivQ,Quest,rating,Q1z:Q4z)
indivQ$Quest2 = ifelse(indivQ$Quest == 'Q1z', 'Q1',ifelse(indivQ$Quest == 'Q2z', 'Q2',ifelse(indivQ$Quest == 'Q3z', 'Q3','Q4')))

p <- ggplot(indivQ, aes(y=rating, x=Quest2, color = Condition)) + geom_boxplot() + geom_jitter(width=.1,height=0,alpha=.5,size=2) + labs(x = "", y = "Ratings (Z-scored)") #+ geom_jitter(width=.2,height=0,alpha=.5,size=3) #facet_wrap(~Subject)
p + theme(
  axis.title.x = element_text(size=0, face="bold"),
  axis.title.y = element_text(size=22, face="bold"),
  axis.text = element_text(size=20),
  legend.text = element_text(size=22),
  legend.title = element_text(size=18)
)

Questionnaire_mean <- indivQ %>% group_by(Condition,Quest) %>% summarise_each(funs(mean,sd,se=sd(.)/sqrt(n())))
ggplot(Questionnaire_mean, aes(y=rating_mean, x=Quest,fill=Condition)) + geom_bar(stat="identity",width=.6,position=position_dodge(.7)) + geom_errorbar(aes(ymin=rating_mean-rating_se,ymax=rating_mean+rating_se), size = .3, width=.2,position=position_dodge(.7)) + ylim(-1.3,1.3) +
  ylab("Rating (Z-scored)") + xlab('Questionnaire')

m1 = lmer(Q1z ~ Condition + (1|Subject),sub_round_info)
m2 = lmer(Q1z ~ Condition + (Condition|Subject),sub_round_info)
anova(m1,m2)
summary(m1)
anova(m1)
summary(m2)
anova(m2)

######### Correlations

ggplot(a, aes(y=dError, x=TimeSpent, color=Condition)) + geom_point(alpha=.7,size=0.3) + geom_smooth(method="lm", se = FALSE, fill = "NA")
ggplot(a, aes(y=dError, x=dMoved, color=Condition)) + geom_point(alpha=.7,size=0.3) + geom_smooth(method="lm", se = FALSE, fill = "NA")
cor.test(~dError + TimeSpent, data = a, method ="spearman", continuity = TRUE, conf.level = 0.95)

m1 = lmer(dError ~ dMoved + Condition + (1|Subject),a)
m2 = lmer(dError ~ dMoved + Condition + (Condition|Subject),a)
anova(m1,m2)
summary(m1)
anova(m1)
summary(m2)
anova(m2)

ggplot(round_info, aes(y=dError, x=EN_TimeSpent, color=Condition)) + geom_point(alpha=.7,size=0.3) + geom_smooth(method="lm", se = FALSE, fill = "NA")
cor.test(~EN_TimeSpent + dError, data = round_info, method ="spearman", continuity = FALSE, conf.level = 0.95)

ggplot(a, aes(x=dError, y=estimation_dError, color=Condition)) + geom_point(alpha=.7,size=0.3) + geom_smooth(method="lm", se = FALSE, fill = "NA") + geom_abline(slope=1,linetype='dashed') + xlim(0,100) + ylim(0,100) + ylab("Actual error (m)") + xlab('Estimated error (m)') #+ facet_wrap(~Subject)
cor.test(~estimation_dError + dError, data = a, method ="spearman", continuity = FALSE, conf.level = 0.95)

m1 = lmer(dError ~ estimation_dError * Condition + (1|Subject),a)
m2 = lmer(dError ~ estimation_dError * Condition + (Condition|Subject),a)
m1 = lmer(ErrorMetaCog ~ Condition + (1|Subject),a)
m2 = lmer(ErrorMetaCog ~ Condition + (Condition|Subject),a)

anova(m1,m2)
summary(m1)
anova(m1)
summary(m2)
anova(m2)

# With Questionnaire
Q5_raw = read_csv("D:/Experiment Data/Navigation and Self/GridCode_Analysis/Memory_data_fMRI_Q5_0502.csv")
Q5_raw$Condition = ifelse(Q5_raw$Condition == 1, 'No-body', 'Body')

indivdata <- merge(indivdata, Q5_raw)
indivdata_mean <- merge(indivdata_mean, Q5_raw)

ggplot(indivdata_withoutQ, aes(y=Body_Effect, x=dError, color=Condition)) + geom_point(alpha=.7,size=3.5) + geom_smooth(method="lm", se = FALSE, fill = "NA")

temp <- subset(indivdata_withoutQ,Condition == 'Body')
cor.test(~Body_Effect + dError, data = indivdata_withoutQ, method ="spearman", continuity = TRUE, conf.level = 0.95)

indivdata$Q_sum <- indivdata$Q1 + indivdata$Q2 + indivdata$Q5
ggplot(indivdata, aes(y=dError, x=Q1, color=Condition)) + geom_point(alpha=.7,size=3.5) + geom_smooth(method="lm", se = FALSE, fill = "NA")
ggplot(indivdata, aes(y=dError, x=Q2, color=Condition)) + geom_point(alpha=.7,size=3.5) + geom_smooth(method="lm", se = FALSE, fill = "NA")

temp <- subset(indivdata_mean,Condition == 'Body')
temp <- indivdata
cor.test(~dError + Q_sum, data = temp, method ="spearman", continuity = TRUE, conf.level = 0.95)

ggplot(a, aes(y=dError, x=Q2z, color=Condition)) + geom_point(alpha=.7,size=0.3) + geom_smooth(method="lm", se = FALSE, fill = "NA")
ggplot(round_info, aes(y=dError, x=Q2z, color=Condition)) + geom_point(alpha=.7,size=2) + geom_smooth(method="lm", se = FALSE, fill = "NA")
ggplot(indivdata, aes(y=dError, x=Q2z, color=Condition)) + geom_point(alpha=.7,size=2) + geom_smooth(method="lm", se = FALSE, fill = "NA")

cor.test(~Q2z + dError, data = indivdata, method ="spearman", continuity = TRUE, conf.level = 0.95)

m1 = lmer(dError ~ Q1*Condition + (1|Subject),round_info)
m2 = lmer(dError ~ Q1*Condition + (Q1*Condition|Subject),round_info)

anova(m1,m2)
summary(m1)
summary(m2)


summary(lm(round_info$dError[round_info$Condition=='Body'] ~ round_info$Q1z[round_info$Condition=='Body']))

m1 = lmer(dError ~ EN_TimeSpent*Condition+ (1|Subject),round_info)
anova(m1)
summary(m1)

# MetaCognition
m1=lmer(dError~estimation_dError * Condition + (1|Subject),data = a)
m2=lmer(dError~estimation_dError * Condition + (estimation_dError * Condition|Subject),data = a)
anova(m1,m2)
summary(m1)
anova(m1)
summary(m2)
anova(m2)

## permutation test
# reps=1000
# a$var=a$distNorm
# bodyError= a$var[a$Condition=='Body']
# objectError = a$var[a$Condition=='No-body']
# 
# realdiff=mean(bodyError) - mean(objectError)
# permdiff=c()
# for (r in 1:reps) {
#   randobject=sort(sample(length(bodyError), 
#                          round(length(bodyError)/2),replace=F))
#   randbody=which(!is.element(seq(1,length(bodyError),1),randobject))
#   permdiff[r]=mean(a$var[randobject])-mean(a$var[randbody])
# }
# diffs=data.frame(permdiff,realdiff)
# ggplot(diffs,aes(x=permdiff,y=..density..)) + geom_density() + 
#   geom_vline(xintercept = realdiff,linetype='dashed') +
#   ggtitle(paste("p =",round(length(which(realdiff>permdiff))/reps,2)))

