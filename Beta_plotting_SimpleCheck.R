rm(list=ls(all=TRUE)) # clean
# 
# install.packages("tidyverse")
# install.packages("dplyr")
# install.packages("tidyr")
# install.packages("readr")
# install.packages("ggplot2")
# install.packages("lme4")
# install.packages("lmerTest")
library(tidyverse)
library(dplyr)
library(tidyr)
library(readr)
library(ggplot2) # plot the data
library(lme4); library(lmerTest) # used for mixed models
# 

# a_raw = read_csv("D:/Experiment Data/Navigation and Self/GridCode_Analysis/Betas_fMRI_pmod_HPF400_0821.csv")
# a_raw = read_csv("D:/Experiment Data/Navigation and Self/GridCode_Analysis/Betas_fMRI_align_resol66_0716.csv")
# a_raw = read_csv("D:/Experiment Data/Navigation and Self/GridCode_Analysis/Betas_fMRI_pmod_motionCorrection_0716.csv")
# a_raw = read_csv("D:/Experiment Data/Navigation and Self/GridCode_Analysis/Betas_fMRI_0626_pmod_resol66.csv")
a_raw = read_csv("D:/Experiment Data/Navigation and Self/GridCode_Analysis/Betas_fMRI_0604_pmod.csv")

nROI = 3

a <- a_raw
#exclude Subj 12/ round 4
# a <- subset(a, a$Subject != 16 & a$Subject != 20 & a$Subject != 4 & a$Subject != 22 & a$Subject != 27)
a <- subset(a, a$Subject !=22) # & a$Subject != 13) # 13
a <- subset(a, a$Subject !=13)
a <- subset(a, a$Subject !=2 & a$Subject !=4 & a$Subject !=5 & a$Subject !=6 & a$Subject !=12 & a$Subject !=13 & a$Subject !=26) 

a <- subset(a,!(a$Subject == 11 & a$Round ==5)) #& !(a$Subject == 3 & a$Round ==5)
a <- subset(a,!(a$Subject == 2 & a$Round ==6)) #& !(a$Subject == 3 & a$Round ==5)
a <- subset(a,!(a$Subject == 6 & a$Round ==2)) #& !(a$Subject == 3 & a$Round ==5)
a <- subset(a,!(a$Subject == 12 & a$Round ==4)) #  & a$Subject != 13 )
a <- subset(a,!(a$Subject == 26 & a$Round ==5))
a <- subset(a,!(a$Subject == 27 & a$Round ==2))
# a <- subset(a,!(a$Subject == 11 & a$Round ==5))
# a <- subset(a,!(a$Subject == 26 & a$Round ==5)) #  & a$Subject != 13 )

a$Condition = ifelse(a$Condition == 1, 'No-body', 'Body')

aNorm <-data.frame() #Subject-wise normalization
aRNorm <-data.frame() #Round-wise normalization

for (n in 1:nROI)
{
  a_temp <- subset(a,a$GLM1_ROI == n)
  for (i in 1:27)
  {
    tempSubj <- subset(a_temp, Subject == i)
    temp <- data.frame(Subject = tempSubj$Subject,GLM1_ROI = tempSubj$GLM1_ROI,xFold = tempSubj$xFold, Round = tempSubj$Round,BNorm_R = c(scale(tempSubj$Beta_R)), BNorm_B = c(scale(tempSubj$Beta_B)),Condition = tempSubj$Condition)
    
    # colnames(temp) <- c("Subject","GLM1_ROI","xFold","Round","BNorm_R","BNorm_L","BNorm_B","Condition")
    aNorm <- rbind(aNorm,temp)
  }
}

a <- merge(a,aNorm)

a_outlier <- subset(a, (a$BNorm_B > 3 | a$BNorm_B < -3) | (a$BNorm_R > 3 | a$BNorm_R < -3) )

a$Subject = factor(a$Subject)
a$xFold = factor(a$xFold)

# a_outlier_xFold = subset(a, a$Norm_x > 3 | a$Norm_x < -3)
# a_excl_xFold = subset(a, a$Norm_x < 3 & a$Norm_x > -3)

aR <- subset(a,a$GLM1_ROI == 1) # GLM1 with the right Entorhinal Cortex
aB <- subset(a,a$GLM1_ROI == nROI) # GLM1 with the both Entorhinal Cortexes

b<-aB
# b<-subset(b,b$Condition == 'Body')
# b<-subset(b,b$Condition == 'No-body')
#Raw Beta
ggplot(b, aes(y=Beta_R, x=xFold, color=Subject)) + geom_boxplot(outlier.shape = NA) + geom_jitter(width=.1,height=0,alpha=.5,size=1.5)# + facet_wrap(~Subject)
ggplot(b, aes(y=Beta_B, x=xFold, color=Subject)) + geom_boxplot(outlier.shape = NA) + geom_jitter(width=.1,height=0,alpha=.5,size=1.5)# + facet_wrap(~Subject)

# ggplot(b, aes(y=Beta_R, x=xFold, color=xFold)) + geom_boxplot(outlier.shape = NA) + geom_jitter(width=.1,height=0,alpha=.5,size=1.5) + facet_wrap(~Subject,scales='free')
# ggplot(b, aes(y=Beta_B, x=xFold, color=xFold)) + geom_boxplot(outlier.shape = NA) + geom_jitter(width=.1,height=0,alpha=.5,size=1.5) + facet_wrap(~Subject,scales='free')

# ggplot(a_excl2, aes(y=Beta, x=xFold, color=xFold)) + geom_boxplot(outlier.shape = NA) + geom_jitter(width=.1,height=0,alpha=.5,size=1.5)# + facet_wrap(~Subject)
# ggplot(a_excl, aes(y=Beta, x=xFold, color=xFold)) + geom_boxplot() + geom_jitter(width=.1,height=0,alpha=.5,size=1.5) + facet_wrap(~Subject)
# ggplot(a_excl, aes(y=BNorm, x=xFold, color=xFold)) + geom_boxplot(outlier.shape = NA) + geom_jitter(width=.1,height=0,alpha=.5,size=1.5)# + facet_wrap(~Subject)
# 
# ggplot(a_excl_xFold2, aes(y=Beta, x=xFold, color=xFold)) + geom_boxplot(outlier.shape = NA) + geom_jitter(width=.1,height=0,alpha=.5,size=1.5)# + facet_wrap(~Subject)
# ggplot(a_excl_xFold, aes(y=Beta, x=xFold, color=xFold)) + geom_boxplot() + geom_jitter(width=.1,height=0,alpha=.5,size=1.5) + facet_wrap(~Subject)
# ggplot(a_excl_xFold, aes(y=BNorm, x=xFold, color=xFold)) + geom_boxplot(outlier.shape = NA) + geom_jitter(width=.1,height=0,alpha=.5,size=1.5)# + facet_wrap(~Subject)
# 
# ggplot(a_excl_IQR, aes(y=Beta, x=xFold, color=xFold)) + geom_boxplot(outlier.shape = NA) + geom_jitter(width=.1,height=0,alpha=.5,size=1.5)# + facet_wrap(~Subject)
# ggplot(a_excl_IQR, aes(y=Beta, x=xFold, color=xFold)) + geom_boxplot() + geom_jitter(width=.1,height=0,alpha=.5,size=1.5) + facet_wrap(~Subject)
# ggplot(a_excl_IQR, aes(y=BNorm, x=xFold, color=xFold)) + geom_boxplot(outlier.shape = NA) + geom_jitter(width=.1,height=0,alpha=.5,size=1.5)# + facet_wrap(~Subject)

# a_6fold = subset(a, a$xFold == 6)
# ggplot(a_6fold, aes(y=Beta, x=Condition, color=Condition)) + geom_boxplot() + geom_jitter(width=.1,height=0,alpha=.5,size=3)
# ggplot(a_6fold, aes(y=BNorm, x=Condition, color=Condition)) + geom_boxplot() + geom_jitter(width=.1,height=0,alpha=.5,size=3) #+ facet_wrap(~Subject)
# 
b<-aB
b_6fold = subset(b, b$xFold == 6)
b_6fold_body = subset(b_6fold, b_6fold$Condition == "Body")
b_6fold_nbody = subset(b_6fold, b_6fold$Condition == "No-body")

# ggplot(b_6fold, aes(y=Beta_R, x=Condition, group=Round, color=Condition)) + geom_line(aes(group=interaction(Subject,Round)),size = 0.8)+ geom_point(size = 2.5) #+ scale_color_manual(values=c("#F8766D", "#00BFC4", "#00BFC4", "#F8766D"))
ggplot(b_6fold, aes(y=Beta_R, x=Condition, color=Condition)) + geom_boxplot(outlier.shape = NA) + geom_jitter(width=.1,height=0,alpha=.5,size=3) 
ggplot(b_6fold, aes(y=Beta_B, x=Condition, color=Condition)) + geom_boxplot(outlier.shape = NA) + geom_jitter(width=.1,height=0,alpha=.5,size=3) 

ggplot(b_6fold, aes(y=BNorm_R, x=Condition, color=Condition)) + geom_boxplot(outlier.shape = NA) + geom_jitter(width=.1,height=0,alpha=.5,size=3) 
ggplot(b_6fold, aes(y=BNorm_B, x=Condition, color=Condition)) + geom_boxplot(outlier.shape = NA) + geom_jitter(width=.1,height=0,alpha=.5,size=3) 

wilcox.test(b_6fold$Beta_R, mu = 0, alternative = 'greater')
wilcox.test(b_6fold$Beta_B, mu = 0, alternative = 'greater')

t.test(b_6fold$Beta_R, mu = 0, alternative = 'greater')
t.test(b_6fold$Beta_B, mu = 0, alternative = 'greater')

# t.test(b$Beta_R[b$xFold==6],b$Beta_R[b$xFold==4],paired=T)


indivdata = aggregate(cbind(Beta_R,Beta_B,BNorm_R,BNorm_B) ~ Subject + GLM1_ROI + xFold, a, mean)
indivdata_wCond = aggregate(cbind(Beta_R,Beta_B,BNorm_R,BNorm_B) ~ Subject + GLM1_ROI + xFold + Condition, a, mean)

# indivdata = aggregate(cbind(Beta_B,BNorm_B,BRNorm_B) ~ Subject + GLM1_ROI + xFold, a, mean)
# indivdata_wCond = aggregate(cbind(Beta_B,BNorm_B,BRNorm_B) ~ Subject + GLM1_ROI + xFold + Condition, a, mean)

indivwC_R <- subset(indivdata_wCond, indivdata_wCond$GLM1_ROI == 1)
indivwC_B <- subset(indivdata_wCond, indivdata_wCond$GLM1_ROI == nROI)

indiv_R <- subset(indivdata, indivdata$GLM1_ROI == 1)
indiv_B <- subset(indivdata, indivdata$GLM1_ROI == nROI)
indiv <- indiv_B
# ggplot(indiv, aes(y=Beta_R, x=xFold, color=xFold)) + geom_boxplot(outlier.shape = NA) + geom_jitter(width=.1,height=0,alpha=.5,size=3) #+ facet_wrap(~Subject)
# ggplot(indiv, aes(y=Beta_B, x=xFold, color=xFold)) + geom_boxplot(outlier.shape = NA) + geom_jitter(width=.1,height=0,alpha=.5,size=3) #+ facet_wrap(~Subject)

indiv_6fold = subset(indiv, indiv$xFold == 6 )# & indivdata$Subject != 12)
wilcox.test(indiv_6fold$Beta_R, mu = 0, alternative = 'greater')
wilcox.test(indiv_6fold$Beta_B, mu = 0, alternative = 'greater')

indiv <- indiv_B
indiv_mean_sd <- indiv %>% group_by(xFold) %>% summarise_each(funs(mean,sd,se=sd(.)/sqrt(n())))

p<- ggplot(indiv_mean_sd, aes(y=Beta_R_mean, x=xFold,fill=xFold)) + geom_bar(stat="identity",width=.6) + geom_errorbar(aes(ymin=Beta_R_mean-Beta_R_se,ymax=Beta_R_mean+Beta_R_se), size = .3, width=.2,position=position_dodge(.9)) #+ scale_fill_manual(values=c("#AAAAAA", "#AAAAAA", "#F8766D", "#AAAAAA"))#+ ylim(0,20)
p<- ggplot(indiv_mean_sd, aes(y=Beta_B_mean, x=xFold,fill=xFold)) + geom_bar(stat="identity",width=.6) + geom_errorbar(aes(ymin=Beta_B_mean-Beta_B_se,ymax=Beta_B_mean+Beta_B_se), size = .3, width=.2,position=position_dodge(.9)) #+ scale_fill_manual(values=c("#AAAAAA", "#AAAAAA", "#F8766D", "#AAAAAA"))#+ ylim(0,20)

p + theme(
  #plot.title = element_text(color="red", size=14, face="bold.italic"),
  axis.title.x = element_text(size=18, face="bold"),
  axis.title.y = element_text(size=22, face="bold"),
  axis.text = element_text(size=22),
  legend.position = "none",
  legend.text = element_text(size=22),
  legend.title = element_text(size=18)
)

indivwC <- indivwC_B
indivwC_6fold <- subset(indivwC,indivwC$xFold ==6 )
# indivwC_6fold <- indivwC_6fold %>% group_by(Subject) %>% mutate(Body_Effect = (BNorm_B[Condition=='Body'] - BNorm_B[Condition=='No-body']))
indivwC_6fold <- indivwC_6fold %>% group_by(Subject) %>% mutate(Body_Effect = (Beta_R[Condition=='Body'] - Beta_R[Condition=='No-body']))
# indivwC_6fold <- indivwC_6fold %>% group_by(Subject) %>% mutate(Body_Effect = (Beta_L[Condition=='Body'] - Beta_L[Condition=='No-body']))
# indivwC_6fold <- indivwC_6fold %>% group_by(Subject) %>% mutate(Body_Effect = (Beta_B[Condition=='Body'] - Beta_B[Condition=='No-body']))

indivwC_6fold <- indivwC_6fold[with(indivwC_6fold, order(Subject)),]
# test <- subset(indivwC_6fold, Beta_R > 0)
# ggplot(test, aes(y=Beta_R, x=Condition, group=Subject, color=Condition)) + geom_line(aes(color=Body_Effect>0),size = 0.8) + geom_point(size = 2.5) + scale_color_manual(values=c("#F8766D", "#00BFC4", "#00BFC4", "#F8766D"))
# 
# test <- subset(indivwC_6fold, Beta_L > -0.02)
# ggplot(test, aes(y=Beta_L, x=Condition, group=Subject, color=Condition)) + geom_line(aes(color=Body_Effect>0),size = 0.8) + geom_point(size = 2.5) + scale_color_manual(values=c("#F8766D", "#00BFC4", "#00BFC4", "#F8766D"))


ggplot(indivwC_6fold, aes(y=Beta_R, x=Condition, group=Condition, color=Condition)) + geom_boxplot(outlier.shape = NA) + geom_jitter(width=.1,height=0,alpha=.5,size=3)

ggplot(indivwC_6fold, aes(y=Beta_R, x=Condition, group=Subject, color=Condition)) + geom_line(aes(color=Body_Effect>0),size = 0.8) + geom_point(size = 2.5) + scale_color_manual(values=c("#F8766D", "#00BFC4", "#00BFC4", "#F8766D"))
ggplot(indivwC_6fold, aes(y=Beta_B, x=Condition, group=Subject, color=Condition)) + geom_line(aes(color=Body_Effect>0),size = 0.8) + geom_point(size = 2.5) + scale_color_manual(values=c("#F8766D", "#00BFC4", "#00BFC4", "#F8766D"))

indivwc_mean_sd <- indivwC %>% group_by(Condition) %>% summarise_each(funs(mean,sd,se=sd(.)/sqrt(n())))

p<- ggplot(indivwc_mean_sd, aes(y=Beta_R_mean, x=Condition,fill=Condition)) + geom_bar(stat="identity",width=.6) + geom_errorbar(aes(ymin=Beta_R_mean-Beta_R_se,ymax=Beta_R_mean+Beta_R_se), size = .3, width=.2,position=position_dodge(.9)) # + scale_fill_manual(values=c("#AAAAAA", "#AAAAAA", "#F8766D", "#AAAAAA"))#+ ylim(0,20)
p<- ggplot(indivwc_mean_sd, aes(y=Beta_B_mean, x=Condition,fill=Condition)) + geom_bar(stat="identity",width=.6) + geom_errorbar(aes(ymin=Beta_B_mean-Beta_B_se,ymax=Beta_B_mean+Beta_B_se), size = .3, width=.2,position=position_dodge(.9)) # + scale_fill_manual(values=c("#AAAAAA", "#AAAAAA", "#F8766D", "#AAAAAA"))#+ ylim(0,20)

p + theme(
  #plot.title = element_text(color="red", size=14, face="bold.italic"),
  axis.title.x = element_text(size=18, face="bold"),
  axis.title.y = element_text(size=22, face="bold"),
  axis.text = element_text(size=22),
  legend.position = "none",
  legend.text = element_text(size=22),
  legend.title = element_text(size=18)
)
# ggplot(indiv, aes(y=BNorm_R, x=Condition, group=Round, color=Condition)) + geom_line(aes(group=interaction(Subject,Round)),size = 0.8)+ geom_point(size = 2.5) #+ scale_color_manual(values=c("#F8766D", "#00BFC4", "#00BFC4", "#F8766D"))

wilcox.test(indivwC_6fold$Beta_R[indivwC_6fold$Condition == 'Body'], indivwC_6fold$Beta_R[indivwC_6fold$Condition == 'No-body'], paired=T)
wilcox.test(indivwC_6fold$Beta_B[indivwC_6fold$Condition == 'Body'], indivwC_6fold$Beta_B[indivwC_6fold$Condition == 'No-body'], paired=T)
# wilcox.test(indivwC_6fold$BNorm_R[indivwC_6fold$Condition == 'Body'], indivwC_6fold$BNorm_R[indivwC_6fold$Condition == 'No-body'], paired=T)
# wilcox.test(indivwC_6fold$BNorm_L[indivwC_6fold$Condition == 'Body'], indivwC_6fold$BNorm_L[indivwC_6fold$Condition == 'No-body'], paired=T)
# wilcox.test(indivwC_6fold$BNorm_B[indivwC_6fold$Condition == 'Body'], indivwC_6fold$BNorm_B[indivwC_6fold$Condition == 'No-body'], paired=T)
