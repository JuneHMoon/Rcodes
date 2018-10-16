rm(list=ls(all=TRUE)) # clean

library(tidyverse)
library(dplyr)
library(tidyr)
library(readr)
library(ggplot2) # plot the data
library(lme4); library(lmerTest) # used for mixed models
library(sjPlot)
library(optimx)
##Study1 - Data import ################
study_raw = read_csv("D:/Experiment Data/Navigation and Self/GridCode_Analysis/Memory_data_fMRI_FINAL.csv")
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
study1$PathEfficiency = 1/(study1$PathEfficiency)
study1$TotalEfficiency = study1$PathEfficiency * study1$TimeEfficiency

study_encoding = read_csv("D:/Experiment Data/Navigation and Self/GridCode_Analysis/Memory_data_fMRI_Encoding_FINAL.csv")

study_encoding$Subject  = factor(study_encoding$Subject)
study_encoding$Round_fac = factor(study_encoding$Round)
study_encoding$EN_PathEfficiency = 800/(study_encoding$EN_PathEfficiency)

########################################
# Exclusion1 - manipulation error - miss-clicked
study1_miss <- subset(study1,((dMoved < 5 | nManipulation < 1 | TimeSpent < 3.5) & (estimation_dError > 50 | dError > 25))| estimation_dError > 70)
study1_excl <- subset(study1,!(((dMoved < 5 | nManipulation < 1 | TimeSpent < 3.5) & (estimation_dError > 50 | dError > 25))| estimation_dError > 70))

## extract round info for Study1
a <- study1_excl
a <- subset(a,  Index != 1 & Index != 14) # first trial - start from the center/ last trial - with virtual threat

NormalizedData <-data.frame() ## Normalization within subject
for (i in 1:27)
{
    temp <- subset(a, Subject == i)
    temp2 <- cbind(scale(temp$dError),scale(temp$dMoved), scale(temp$estimation_dError), scale(temp$MemoryPerform))
    # temp2 <- cbind(scale(temp$dError),scale(temp$dMoved), scale(temp$estimation_dError))
    NormalizedData <- rbind(NormalizedData,temp2)
}

a$distNorm <- NormalizedData$V1
a$dMovedNorm <- NormalizedData$V2
a$estimationNorm <- NormalizedData$V3
a$MemPNorm <- NormalizedData$V4

study1_excl <- a

study1_excl <- merge(study1_excl,study_encoding)
a <- study1_excl

# a <- merge(a,study_encoding)

rounddata_median = aggregate(cbind(dError,distNorm,Condition,Group,estimation_dError,ErrorMetaCog,MemoryPerform,Q1,Q2,Q3,Q4,Q1z,Q2z,Q3z,Q4z,PathEfficiency,TimeEfficiency,TotalEfficiency,tCenter,tPeri,dEndpoint,EN_TimeSpent,EN_dMoved,EN_PathEfficiency,EN_TimeEfficiency,EN_tCenter,EN_tPeri) ~ Subject + Round_fac,a,median)
rounddata_mean = aggregate(cbind(dError,distNorm,Condition,Group,estimation_dError,ErrorMetaCog,MemoryPerform, Q1,Q2,Q3,Q4,Q1z,Q2z,Q3z,Q4z,PathEfficiency,TimeEfficiency,TotalEfficiency,tCenter,tPeri,dEndpoint,EN_TimeSpent,EN_dMoved,EN_PathEfficiency,EN_TimeEfficiency,EN_tCenter,EN_tPeri) ~ Subject + Round_fac,a,mean)

rounddata_median1 = aggregate(cbind(dError,distNorm,Condition,Group,estimation_dError,ErrorMetaCog,MemoryPerform,EN_TimeSpent,EN_dMoved,EN_nDirectionChange,EN_nManipulation) ~ Subject + Round_fac,a,median)
rounddata_mean1 = aggregate(cbind(dMoved,TimeSpent,directionChange,nManipulation,dMovedNorm,Q1,Q2,Q3,Q4,Q1z,Q2z,Q3z,Q4z,PathEfficiency,TimeEfficiency,TotalEfficiency,tCenter,tPeri,dEndpoint,EN_PathEfficiency,EN_TimeEfficiency,EN_tCenter,EN_tPeri) ~ Subject + Round_fac,a,mean) 
rounddata <- merge(rounddata_median1,rounddata_mean1) # should think which method(mean/median) is better for each category

round_info <- rounddata
round_info$Condition = ifelse(round_info$Condition == 1, 'No-body','Body')
round_info$Group = ifelse(round_info$Group == 1, 'Nobody-First','Body-First')

## confirm Round effect
# ggplot(round_info, aes(y=dError, x=Round_fac, color=Round_fac)) + geom_boxplot(outlier.shape = NA) + geom_jitter(width=.1,height=0,alpha=.5,size=1)
# ggplot(round_info, aes(y=distNorm, x=Round_fac, color=Round_fac)) + geom_boxplot(outlier.shape = NA) + geom_jitter(width=.1,height=0,alpha=.5,size=1)
# ggplot(round_info, aes(y=dMoved, x=Round_fac, color=Round_fac)) + geom_boxplot(outlier.shape = NA) + geom_jitter(width=.1,height=0,alpha=.5,size=1)
# ggplot(round_info, aes(y=TimeSpent, x=Round_fac, color=Round_fac)) + geom_boxplot(outlier.shape = NA) + geom_jitter(width=.1,height=0,alpha=.5,size=1)

############## Assess Distribution
a <- study1_excl # study1_exclIQR
a$Condition = ifelse(a$Condition == 1, 'No-body','Body')
# a$Group =  ifelse(a$Group == 1, 'No-body First', 'Body First')

# ggplot(a, aes(x=MemoryPerform,y=..density.., group=Condition,  color=Condition)) + geom_density() #+ facet_wrap(~Subject,scales='free')
ggplot(a, aes(x=dError,y=..density.., group=Condition,  color=Condition)) + geom_density() #+ facet_wrap(~Subject,scales='free')
# ggplot(round_info, aes(x=dError,y=..density.., group=Condition,  color=Condition)) + geom_density() #+ facet_wrap(~Subject,scales='free')
# ggplot(indivdata, aes(x=dError,y=..density.., group=Condition,  color=Condition)) + geom_density() #+ facet_wrap(~Subject,scales='free')

ggplot(a, aes(x=MemPNorm,y=..density.., group=Condition,  color=Condition)) + geom_density() #+ facet_wrap(~Subject,scales='free')
ggplot(a, aes(x=distNorm,y=..density.., group=Condition,  color=Condition)) + geom_density() #+ facet_wrap(~Subject,scales='free')
ggplot(a, aes(x=PathEfficiency,y=..density.., group=Condition,  color=Condition)) + geom_density() #+ facet_wrap(~Subject,scales='free')
ggplot(a, aes(x=TimeEfficiency,y=..density.., group=Condition,  color=Condition)) + geom_density() #+ facet_wrap(~Subject,scales='free')
ggplot(a, aes(x=TotalEfficiency,y=..density.., group=Condition,  color=Condition)) + geom_density() #+ facet_wrap(~Subject,scales='free')

ggplot(a, aes(x=tCenter,y=..density.., group=Condition,  color=Condition)) + geom_density()
ggplot(a, aes(x=tPeri,y=..density.., group=Condition,  color=Condition)) + geom_density()
# ggplot(a, aes(x=tCenter/tPeri,y=..density.., group=Condition,  color=Condition)) + geom_density()

ggplot(a, aes(x=dMoved,y=..density.., group=Condition,  color=Condition)) + geom_density()
ggplot(a, aes(x=TimeSpent,y=..density.., group=Condition,  color=Condition)) + geom_density()
ggplot(a, aes(x=estimation_dError,y=..density.., group=Condition,  color=Condition)) + geom_density()
ggplot(a, aes(x=ErrorMetaCog,y=..density.., group=Condition,  color=Condition)) + geom_density() # + facet_wrap(~Subject,scales='free')
ggplot(a, aes(x=nManipulation,y=..density.., group=Condition,  color=Condition)) + geom_density() # + facet_wrap(~Subject,scales='free')

ggplot(round_info, aes(x=EN_TimeSpent,y=..density.., group=Condition,  color=Condition)) + geom_density()
ggplot(round_info, aes(x=EN_dMoved,y=..density.., group=Condition,  color=Condition)) + geom_density()
ggplot(round_info, aes(x=EN_PathEfficiency,y=..density.., group=Condition,  color=Condition)) + geom_density()
ggplot(round_info, aes(x=EN_TimeEfficiency,y=..density.., group=Condition,  color=Condition)) + geom_density()

############## Plot difference
indivdata_withoutQ = aggregate(cbind(dError,distNorm,Group,estimation_dError,ErrorMetaCog,MemoryPerform,PathEfficiency,TimeEfficiency,TotalEfficiency,tCenter,tPeri,dEndpoint,EN_TimeSpent,EN_nDirectionChange,EN_nManipulation,EN_dMoved,EN_PathEfficiency,EN_TimeEfficiency,EN_tCenter,EN_tPeri) ~ Condition + Subject,a,median)
indivdata_mean_withoutQ = aggregate(cbind(dError,distNorm,Group,estimation_dError,ErrorMetaCog,MemoryPerform,PathEfficiency,TimeEfficiency,TotalEfficiency,tCenter,tPeri,dEndpoint,EN_TimeSpent,EN_nDirectionChange,EN_nManipulation,EN_dMoved,EN_PathEfficiency,EN_TimeEfficiency,EN_tCenter,EN_tPeri) ~ Condition + Subject,a,mean)
indivdata_woCond = aggregate(cbind(dError,distNorm,Group,estimation_dError,ErrorMetaCog,MemoryPerform,PathEfficiency,TimeEfficiency,TotalEfficiency,tCenter,tPeri,dEndpoint,EN_TimeSpent,EN_nDirectionChange,EN_nManipulation,EN_dMoved,EN_PathEfficiency,EN_TimeEfficiency,EN_tCenter,EN_tPeri) ~ Subject,a,median)
indivdata_mean_woCond = aggregate(cbind(dError,distNorm,Group,estimation_dError,ErrorMetaCog,MemoryPerform,PathEfficiency,TimeEfficiency,TotalEfficiency,tCenter,tPeri,dEndpoint,EN_TimeSpent,EN_nDirectionChange,EN_nManipulation,EN_dMoved,EN_PathEfficiency,EN_TimeEfficiency,EN_tCenter,EN_tPeri) ~ Subject,a,mean)

indivdata = aggregate(cbind(dError,distNorm,Group,estimation_dError,ErrorMetaCog,MemoryPerform,Q1,Q2,Q3,Q4,Q1z,Q2z,Q3z,Q4z,PathEfficiency,TimeEfficiency,TotalEfficiency,tCenter,tPeri,dEndpoint,EN_TimeSpent,EN_nDirectionChange,EN_nManipulation,EN_dMoved,EN_PathEfficiency,EN_TimeEfficiency,EN_tCenter,EN_tPeri) ~ Condition + Subject,a,median)
indivdata_mean = aggregate(cbind(dError,distNorm,Group,estimation_dError,ErrorMetaCog,MemoryPerform,Q1,Q2,Q3,Q4,Q1z,Q2z,Q3z,Q4z,PathEfficiency,TimeEfficiency,TotalEfficiency,tCenter,tPeri,dEndpoint,EN_TimeSpent,EN_nDirectionChange,EN_nManipulation,EN_dMoved,EN_PathEfficiency,EN_TimeEfficiency,EN_tCenter,EN_tPeri) ~ Condition + Subject,a,mean)
indivdata_index = aggregate(cbind(dError,distNorm,Group,estimation_dError,ErrorMetaCog,MemoryPerform,Q1,Q2,Q3,Q4,Q1z,Q2z,Q3z,Q4z,PathEfficiency,TimeEfficiency,TotalEfficiency,tCenter,tPeri,dEndpoint,EN_TimeSpent,EN_nDirectionChange,EN_nManipulation,EN_dMoved,EN_PathEfficiency,EN_TimeEfficiency,EN_tCenter,EN_tPeri) ~ Condition + Index,a,median)
indivdata_mean_index = aggregate(cbind(dError,distNorm,Group,estimation_dError,ErrorMetaCog,MemoryPerform,Q1,Q2,Q3,Q4,Q1z,Q2z,Q3z,Q4z,PathEfficiency,TimeEfficiency,TotalEfficiency,tCenter,tPeri,dEndpoint,EN_TimeSpent,EN_nDirectionChange,EN_nManipulation,EN_dMoved,EN_PathEfficiency,EN_TimeEfficiency,EN_tCenter,EN_tPeri) ~ Condition + Index,a,mean)
# indivdata_index_subj = aggregate(cbind(dError,distNorm,dMoved,TimeSpent,directionChange,nManipulation,estimation_dError,estimationNorm,ErrorMetaCog,MemoryPerform,Q1,Q2,Q3,Q4,Q1z,Q2z,Q3z,Q4z) ~ Condition + Subject + Index,a,median)
# indivdata_mean_index_subj = aggregate(cbind(dError,distNorm,dMoved,TimeSpent,directionChange,nManipulation,estimation_dError,estimationNorm,ErrorMetaCog,MemoryPerform,Q1,Q2,Q3,Q4,Q1z,Q2z,Q3z,Q4z) ~ Condition + Subject + Index,a,mean)

indivdata <- indivdata %>% group_by(Subject) %>% mutate(Body_Effect = (distNorm[Condition=='No-body'] - distNorm[Condition=='Body']))
indivdata_mean <- indivdata_mean %>% group_by(Subject) %>% mutate(Body_Effect = (distNorm[Condition=='No-body'] - distNorm[Condition=='Body']))
indivdata_withoutQ <- indivdata_withoutQ %>% group_by(Subject) %>% mutate(Body_Effect = (distNorm[Condition=='No-body'] - distNorm[Condition=='Body']))
indivdata_mean_withoutQ <- indivdata_mean_withoutQ %>% group_by(Subject) %>% mutate(Body_Effect = (distNorm[Condition=='No-body'] - distNorm[Condition=='Body']))
Body_Effect_median = aggregate(cbind(Body_Effect) ~ Subject,indivdata_withoutQ,median)
Body_Effect_mean = aggregate(cbind(Body_Effect) ~ Subject,indivdata_mean_withoutQ,mean)
indivdata_mean_woCond <- merge(indivdata_mean_woCond,Body_Effect_mean)
indivdata_woCond <- merge(indivdata_woCond,Body_Effect_median)

# accord to trial Index (2 to 13)
ggplot(indivdata_index, aes(y=,dError, x=Index, color=Condition)) + geom_line()+ geom_point()
ggplot(indivdata_mean_index, aes(y=distNorm, x=Index, color=Condition)) + geom_line()+ geom_point() #+  facet_wrap(~Subject,scales='free')
ggplot(indivdata_index, aes(y=estimation_dError, x=Index, color=Condition)) + geom_line()+ geom_point()
ggplot(indivdata_index, aes(y=PathEfficiency, x=Index, color=Condition)) + geom_line()+ geom_point()
# ggplot(indivdata_mean_index_subj, aes(y=distNorm, x=Index, color=Condition)) + geom_line()+ geom_point() +  facet_wrap(~Subject,scales='free')
# ggplot(indivdata_index_subj, aes(y=dError, x=Index, color=Condition)) + geom_line()+ geom_point() +  facet_wrap(~Subject,scales='free')

ggplot(indivdata, aes(y=distNorm, x=Condition, color=Condition)) + geom_boxplot()+ facet_wrap(~Subject)
ggplot(indivdata, aes(y=EN_TimeEfficiency, x=Condition, color=Condition)) + geom_boxplot()+ geom_jitter(width=.1,height=0,alpha=.5,size=2)
ggplot(indivdata, aes(y=EN_TimeSpent, x=Condition, color=Condition)) + geom_boxplot()+ geom_jitter(width=.1,height=0,alpha=.5,size=2)
ggplot(indivdata, aes(y=tCenter, x=Condition, color=Condition)) + geom_boxplot()+ geom_jitter(width=.1,height=0,alpha=.5,size=2)
ggplot(indivdata, aes(y=EN_nDirectionChange, x=Condition, color=Condition)) + geom_boxplot()+ geom_jitter(width=.1,height=0,alpha=.5,size=2)
ggplot(indivdata, aes(y=EN_nManipulation, x=Condition, color=Condition)) + geom_boxplot()+ geom_jitter(width=.1,height=0,alpha=.5,size=2)

temp <- indivdata_mean
p <-ggplot(temp, aes(y=MemoryPerform, x=Condition, group=Subject, color=Condition)) + geom_line(aes(color=Body_Effect>0),size = 0.8) + geom_point(size = 2.5) + scale_color_manual(values=c("#F8766D", "#00BFC4", "#00BFC4", "#F8766D")) #+ geom_point(aes(color=Condition), size=2.5)
p <-ggplot(temp, aes(y=dError, x=Condition, group=Subject, color=Condition)) + geom_line(aes(color=Body_Effect>0),size = 0.8) + geom_point(size = 2.5) + scale_color_manual(values=c("#F8766D", "#00BFC4", "#00BFC4", "#F8766D")) #+ geom_point(aes(color=Condition), size=2.5)
p <-ggplot(temp, aes(y=distNorm, x=Condition, group=Subject, color=Condition)) + geom_line(aes(color=Body_Effect>0),size = 0.8) + geom_point(size = 2.5) + scale_color_manual(values=c("#F8766D", "#00BFC4", "#00BFC4", "#F8766D")) #+ geom_point(aes(color=Condition), size=2.5)

p + theme(
  axis.title.x = element_text(size=0, face="bold"),
  axis.title.y = element_text(size=22, face="bold"),
  axis.text = element_text(size=20),
  legend.position = "none",
  
  legend.text = element_text(size=22),
  legend.title = element_text(size=18)
)

wilcox.test(temp$dError[temp$Condition=='Body'],temp$dError[temp$Condition=='No-body'],paired=T, alternative = "less" )
wilcox.test(temp$TotalEfficiency[temp$Condition=='Body'],temp$TotalEfficiency[temp$Condition=='No-body'],paired=T, alternative = "greater" )
t.test(temp$dError[temp$Condition=='Body'],temp$dError[temp$Condition=='No-body'],paired=T, alternative = "less")

indiv_mean_sd <- indivdata_mean %>% group_by(Condition) %>% summarise_each(funs(mean,sd,se=sd(.)/sqrt(n())))
indiv_median_mean_sd <- indivdata %>% group_by(Condition) %>% summarise_each(funs(mean,sd,se=sd(.)/sqrt(n())))
round_mean_sd <- round_info %>% group_by(Condition) %>% summarise_each(funs(mean,sd,se=sd(.)/sqrt(n())))

ggplot(indiv_mean_sd, aes(y=MemoryPerform_mean, x=Condition,fill=Condition)) + geom_bar(stat="identity",width=.6) + geom_errorbar(aes(ymin=MemoryPerform_mean-MemoryPerform_se,ymax=MemoryPerform_mean+MemoryPerform_se), size = .3, width=.2,position=position_dodge(.9)) + coord_cartesian(ylim=c(0.925,1.0))
ggplot(indiv_mean_sd, aes(y=dError_mean, x=Condition,fill=Condition)) + geom_bar(stat="identity",width=.6) + geom_errorbar(aes(ymin=dError_mean-dError_se,ymax=dError_mean+dError_se), size = .3, width=.2,position=position_dodge(.9)) #+ ylim(0,20)
p <- ggplot(indiv_mean_sd, aes(y=distNorm_mean, x=Condition,fill=Condition)) + geom_bar(stat="identity",width=.6) + geom_errorbar(aes(ymin=distNorm_mean-distNorm_se,ymax=distNorm_mean+distNorm_se), size = .3, width=.2,position=position_dodge(.9)) # ylim(-0.17,0.17)
ggplot(indiv_mean_sd, aes(y=ErrorMetaCog_mean, x=Condition,fill=Condition)) + geom_bar(stat="identity",width=.6) + geom_errorbar(aes(ymin=ErrorMetaCog_mean-ErrorMetaCog_se,ymax=ErrorMetaCog_mean+ErrorMetaCog_se), size = .3, width=.2,position=position_dodge(.9)) # ylim(-0.17,0.17)
ggplot(indiv_mean_sd, aes(y=dMoved_mean, x=Condition,fill=Condition)) + geom_bar(stat="identity",width=.6) + geom_errorbar(aes(ymin=dMoved_mean-dMoved_se,ymax=dMoved_mean+dMoved_se), size = .3, width=.2,position=position_dodge(.9))  + coord_cartesian(ylim=c(70,90))
ggplot(indiv_mean_sd, aes(y=TimeSpent_mean, x=Condition,fill=Condition)) + geom_bar(stat="identity",width=.6) + geom_errorbar(aes(ymin=TimeSpent_mean-TimeSpent_se,ymax=TimeSpent_mean+TimeSpent_se), size = .3, width=.2,position=position_dodge(.9))  + coord_cartesian(ylim=c(12.5,17.5))
ggplot(indiv_mean_sd, aes(y=nManipulation_mean, x=Condition,fill=Condition)) + geom_bar(stat="identity",width=.6) + geom_errorbar(aes(ymin=nManipulation_mean-nManipulation_se,ymax=nManipulation_mean+nManipulation_se), size = .3, width=.2,position=position_dodge(.9))  #+ coord_cartesian(ylim=c(12.5,17.5))
ggplot(indiv_mean_sd, aes(y=directionChange_mean, x=Condition,fill=Condition)) + geom_bar(stat="identity",width=.6) + geom_errorbar(aes(ymin=directionChange_mean-directionChange_se,ymax=directionChange_mean+directionChange_se), size = .3, width=.2,position=position_dodge(.9))  #+ coord_cartesian(ylim=c(12.5,17.5))

ggplot(indiv_median_mean_sd, aes(y=dError_mean, x=Condition,fill=Condition)) + geom_bar(stat="identity",width=.6) + geom_errorbar(aes(ymin=dError_mean-dError_se,ymax=dError_mean+dError_se), size = .3, width=.2,position=position_dodge(.9)) 
ggplot(indiv_median_mean_sd, aes(y=distNorm_mean, x=Condition,fill=Condition)) + geom_bar(stat="identity",width=.6) + geom_errorbar(aes(ymin=distNorm_mean-distNorm_se,ymax=distNorm_mean+distNorm_se), size = .3, width=.2,position=position_dodge(.9)) # ylim(-0.17,0.17)
ggplot(indiv_median_mean_sd, aes(y=ErrorMetaCog_mean, x=Condition,fill=Condition)) + geom_bar(stat="identity",width=.6) + geom_errorbar(aes(ymin=ErrorMetaCog_mean-ErrorMetaCog_se,ymax=ErrorMetaCog_mean+ErrorMetaCog_se), size = .3, width=.2,position=position_dodge(.9)) # ylim(-0.17,0.17)
ggplot(indiv_median_mean_sd, aes(y=dMoved_mean, x=Condition,fill=Condition)) + geom_bar(stat="identity",width=.6) + geom_errorbar(aes(ymin=dMoved_mean-dMoved_se,ymax=dMoved_mean+dMoved_se), size = .3, width=.2,position=position_dodge(.9))  + coord_cartesian(ylim=c(70,90))

## Body-first vs. Nobody-first : Group
a$Group =  ifelse(a$Group == 1, 'No-body First', 'Body First')
indivdata$Group =  ifelse(indivdata$Group == 1, 'No-body First', 'Body First')

ggplot(a, aes(x=dError,y=..density.., group=Condition,  color=Condition)) + geom_density() + facet_wrap(~Group,scales='free')
ggplot(a, aes(x=PathEfficiency,y=..density.., group=Group,  color=Group)) + geom_density() 
ggplot(indivdata, aes(y=EN_PathEfficiency, x=Group, color=Group)) + geom_boxplot()+ geom_jitter(width=.1,height=0,alpha=.5,size=2)

#Other random factors
ggplot(a, aes(y=distNorm, x=Round_fac, color=Round_fac)) + geom_boxplot()
ggplot(a, aes(y=dError, x=Position, color=Position)) + geom_boxplot()
ggplot(a, aes(y=dError, x=Object, color=Object)) + geom_boxplot()
###################### Assess Difference : mixed model

binom.test(19,27)

# m1 = glmer(dError ~ Condition + (1|Subject/Round_fac),a,control = glmerControl(optimizer = "Nelder_Mead"), family=Gamma) #nested model 
# m2 = glmer(dError ~ Condition + (Condition|Subject/Round_fac),a,control = glmerControl(optimizer = "Nelder_Mead"), family=Gamma) #nested model 
# m1 = glmer(dError ~ Condition + (1|Subject/Round_fac),a,control = glmerControl(optimizer = "optimx", calc.derivs = FALSE, optCtrl = list(method ="nlminb")), family=Gamma) #nested model 
# m2 = glmer(dError ~ Condition + (Condition|Subject/Round_fac),a,control = glmerControl(optimizer = "optimx", calc.derivs = FALSE, optCtrl = list(method ="nlminb")), family=Gamma) #nested model 

# m1 = glmer(dError ~ Condition + (1|Subject/Object),a,control = glmerControl(optimizer = "Nelder_Mead"), family=Gamma) #nested model 
# m2 = glmer(dError ~ Condition + (Condition|Subject/Object),a,control = glmerControl(optimizer = "Nelder_Mead"), family=Gamma) #nested model 

# m1 = glmer(dError ~ EN_TimeSpent + (1|Subject),a,control = glmerControl(optimizer = "optimx", calc.derivs = FALSE, optCtrl = list(maxit = 10000,method ="nlminb")), family=Gamma) #nested model 
m1 = glmer(dError ~ EN_TimeSpent + (1|Subject),control = glmerControl(optimizer = "Nelder_Mead",calc.derivs = FALSE),a,family=Gamma) 
m2 = glmer(dError ~ EN_TimeSpent + (EN_TimeSpent|Subject),control = glmerControl(optimizer = "Nelder_Mead",calc.derivs = FALSE),a,family=Gamma) 

# dError ~ Condition
m1 = glmer(dError ~ Condition + (1|Subject/Object),a,control = glmerControl(optimizer = "optimx", calc.derivs = FALSE, optCtrl = list(method ="nlminb")), family=Gamma) #nested model 
m2 = glmer(dError ~ Condition + (Condition|Subject/Object),a,control = glmerControl(optimizer = "optimx", calc.derivs = FALSE, optCtrl = list(method ="nlminb")), family=Gamma) #nested model 

# TimeSpent ~ Condition x
m1 = glmer(TimeSpent ~ Condition + (1|Subject),a,control = glmerControl(optimizer = "Nelder_Mead"), family=Gamma) #nested model 
m2 = glmer(TimeSpent ~ Condition + (Condition|Subject),a,control = glmerControl(optimizer = "Nelder_Mead"), family=Gamma) #nested model

# dMoved ~ Condition
m1 = lmer(dMoved ~ Condition + (1|Subject),a,control = lmerControl(optimizer = "optimx", calc.derivs = FALSE, optCtrl = list(maxit = 10000,method ="nlminb"))) #nested model
m2 = lmer(dMoved ~ Condition + (Condition|Subject),a,control = lmerControl(optimizer = "optimx", calc.derivs = FALSE, optCtrl = list(maxit = 10000,method ="nlminb"))) #nested model

# No effect on manipulations
m1 = glmer(nManipulation ~ Condition + (Condition|Subject),a, family=Gamma)
m1 = glmer(directionChange ~ Condition + (Condition|Subject),a, family=Gamma)

#
m1 = lmer(ErrorMetaCog ~ Condition+  (1|Subject),a)
m2 = lmer(ErrorMetaCog ~ Condition+ (Condition|Subject),a)

anova(m1,m2)
summary(m1)
anova(m1)
summary(m2)
anova(m2)

sjp.glmer(m2)
sjp.glmer(m2,type = "pred", var = "Condition")
sjp.glmer(m2,type = "rs.ri")
sjp.glmer(m2,type = "eff")

sjp.lmer(m2)
sjp.lmer(m2,type = "pred", var = "Condition")
sjp.lmer(m2,type = "rs.ri")
sjp.lmer(m2,type = "eff")

plot_model(m4)
plot_model(m4,type = "pred",terms='EN_TimeSpent')
plot_model(m4,type = "slope",terms='EN_TimeSpent')

plot_model(m4,type = "pred",terms=c('Group','Condition'))

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
sub_round_info <- round_info
sub_round_info <- subset(round_info, Subject != 1)
sub_round_info <- subset(round_info, Subject != 1 & Subject != 25 & Subject != 17 & Subject != 26) # 1, 11, 17, 25, 26
indivQ = aggregate(cbind(Q1z,Q2z,Q3z,Q4z) ~ Condition + Subject,sub_round_info,mean)
indivQ =  gather(indivQ,Quest,rating,Q1z:Q4z)
indivQ = aggregate(cbind(Q1,Q2,Q3,Q4) ~ Condition + Subject,sub_round_info,mean)
indivQ =  gather(indivQ,Quest,rating,Q1:Q4)
indivQ$Quest2 = ifelse(indivQ$Quest == 'Q1z', 'Q1',ifelse(indivQ$Quest == 'Q2z', 'Q2',ifelse(indivQ$Quest == 'Q3z', 'Q3','Q4')))

p <- ggplot(indivQ, aes(y=rating, x=Quest, color = Condition)) + geom_boxplot() + geom_point(alpha =.5, size=2.,position=position_jitterdodge(jitter.width =.1 )) + labs(x = "", y = "Ratings") #+ geom_jitter(width=.2,height=0,alpha=.5,size=3) #facet_wrap(~Subject)
p + theme(
  axis.title.x = element_text(size=0, face="bold"),
  axis.title.y = element_text(size=22, face="bold"),
  axis.text = element_text(size=20),
  legend.text = element_text(size=22),
  legend.title = element_text(size=18)
)

Questionnaire_mean <- indivQ %>% group_by(Condition,Quest) %>% summarise_each(funs(mean,sd,se=sd(.)/sqrt(n())))
ggplot(Questionnaire_mean, aes(y=rating_mean, x=Quest,fill=Condition)) + geom_bar(stat="identity",width=.6,position=position_dodge(.7)) + geom_errorbar(aes(ymin=rating_mean-rating_se,ymax=rating_mean+rating_se), size = .3, width=.2,position=position_dodge(.7)) +
  ylab("Rating") + xlab('Questionnaire')

m1 = lmer(Q2 ~ Condition + (1|Subject),sub_round_info)
m2 = lmer(Q2 ~ Condition + (Condition|Subject),sub_round_info)
anova(m1,m2)
summary(m1)
anova(m1)
summary(m2)
anova(m2)

######### Correlations
ggplot(a, aes(y=dError, x=TimeSpent, color=Condition)) + geom_point(alpha=.7,size=0.3) + geom_smooth(method="lm", se = FALSE, fill = "NA")
ggplot(a, aes(y=dError, x=EN_TimeSpent, color=Condition)) + geom_point(alpha=.7,size=0.3) + geom_smooth(method="lm", se = FALSE, fill = "NA")
ggplot(indivdata_woCond, aes(y=dError, x=dMoved)) + geom_point(alpha=.7,size=0.3) + geom_smooth(method="lm", se = FALSE, fill = "NA")
ggplot(indivdata, aes(y=dError, x=TimeSpent, color=Condition)) + geom_point(alpha=.7,size=0.3) + geom_smooth(method="lm", se = FALSE, fill = "NA")

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
b <- merge(a,Q5_raw)

ggplot(indivdata_withoutQ, aes(y=Body_Effect, x=dError, color=Condition)) + geom_point(alpha=.7,size=2.5) + geom_smooth(method="lm", se = FALSE, fill = "NA")
ggplot(indivdata_woCond, aes(y=Body_Effect, x=dError)) + geom_point(alpha=.7,size=2.5) + geom_smooth(method="lm", se = FALSE, fill = "NA")

temp <- subset(indivdata_withoutQ,Condition == 'Body')
cor.test(~Body_Effect + dError, data = indivdata_woCond, method ="spearman", continuity = TRUE, conf.level = 0.95)

indivdata$Q_sum <- indivdata$Q1 + indivdata$Q2 + indivdata$Q5
ggplot(indivdata_mean, aes(y=dError, x=Q2, color=Condition)) + geom_point(alpha=.7,size=3.5) + geom_smooth(method="lm", se = FALSE, fill = "NA")
ggplot(indivdata, aes(y=distNorm, x=Q1, color=Condition)) + geom_point(alpha=.7,size=3.5) + geom_smooth(method="lm", se = FALSE, fill = "NA")
ggplot(indivdata_mean, aes(y=distNorm, x=Q2z, color=Condition)) + geom_point(alpha=.7,size=3.5) + geom_smooth(method="lm", se = FALSE, fill = "NA")
ggplot(indivdata, aes(y=distNorm, x=Q2z, color=Condition)) + geom_point(alpha=.7,size=3.5) + geom_smooth(method="lm", se = FALSE, fill = "NA")

ggplot(indivdata_mean, aes(y=distNorm, x=Q2z)) + geom_point(alpha=.7,size=3.5) + geom_smooth(method="lm", se = FALSE, fill = "NA")

ggplot(round_info, aes(y=dError, x=Q1, color=Condition)) + geom_point(alpha=.7,size=3.5) + geom_smooth(method="lm", se = FALSE, fill = "NA")

ggplot(indivdata, aes(y=dError, x=Q2, color=Condition)) + geom_point(alpha=.7,size=3.5) + geom_smooth(method="lm", se = FALSE, fill = "NA")

temp <- subset(indivdata_mean,Condition == 'Body')
temp <- indivdata
cor.test(~distNorm + Q2z, data = indivdata, method ="spearman", continuity = TRUE, conf.level = 0.95)

ggplot(indivdata, aes(y=distNorm, x=Q1z, color=Condition)) + geom_point(alpha=.7,size=0.3) + geom_smooth(method="lm", se = FALSE, fill = "NA")
ggplot(round_info, aes(y=distNorm, x=Q1z, color=Condition)) + geom_point(alpha=.7,size=2) + geom_smooth(method="lm", se = FALSE, fill = "NA")
ggplot(indivdata, aes(y=dError, x=Q2, color=Condition)) + geom_point(alpha=.7,size=2) + geom_smooth(method="lm", se = FALSE, fill = "NA")

cor.test(~Q2z + dError, data = indivdata, method ="spearman", continuity = TRUE, conf.level = 0.95)

m1 = lmer(dError ~ Q1*Condition + (1|Subject),a)
m2 = lmer(dError ~ Q1*Condition + (Q1*Condition|Subject),a)

m1 = lmer(distNorm ~ Q1*Condition + (1|Subject),a)
m2 = lmer(distNorm ~ Q1*Condition + (Q1*Condition|Subject),a)

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


m1 = glmer(dError ~ Q1*Condition + (1|Round_fac) + (1|Subject),b, family=Gamma(link='log'))
m2 = glmer(dError ~ Q1*Condition + (1|Round_fac) + (Q1*Condition|Subject),b,family=Gamma(link='log'))

m1 = glmer(dError ~ Condition + (1|Round_fac) + (1|Subject),a, family=poisson)
m2 = glmer(dError ~ Condition + (1|Round_fac) + (Condition|Subject),a,family=poisson)

m1 = glmer(dError ~ dMoved*Condition + (1|Subject),a, family=Gamma(link='log'))
m2 = glmer(dError ~ dMoved*Condition + (dMoved*Condition|Subject),a,family=Gamma(link='log'))

m1 = glmer(dError ~ Q1*Condition  + (1|Subject),a, family=poisson)
m2 = glmer(dError ~ Q1*Condition  + (Q1*Condition|Subject),a,family=poisson)

m1 = lmer(dError ~ Q5*Condition + (1|Round_fac) + (1|Subject),b)
m2 = lmer(dError ~ Q5*Condition + (1|Round_fac) + (Q5*Condition|Subject),b)

m1 = glmer(dError ~ dMoved*Condition  + (1|Subject),a, family=Gamma(link='log'))
m2 = glmer(dError ~ dMoved*Condition  + (dMoved*Condition|Subject),a,family=Gamma(link='log'))

m1 = lmer(dError ~ dMoved*Condition + (1|Round_fac) + (1|Subject),a)
m2 = lmer(dError ~ dMoved*Condition + (1|Round_fac) + (dMoved*Condition|Subject),a)

anova(m1,m2)
summary(m1)
anova(m1)
summary(m2)
anova(m2)

# require(sjPlot)
plot_model(m2,type = "pred",terms=c('Group','Condition'))
plot_model(m1,type = "pred",terms=c('Q1'))

sjp.glmer(m1)
sjp.glmer(m1,type = "pred", var = c('Q1','Condition'))
sjp.glmer(m1,type = "rs.ri")


sjp.glmer(m1,type = "eff")
plot_model(m1)


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

