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

a_raw = read_csv("D:/Experiment Data/Navigation and Self/GridCode_Analysis/Betas_fMRI_comparison_pmod_0716.csv")
# a_raw = read_csv("D:/Experiment Data/Navigation and Self/GridCode_Analysis/Betas_fMRI_comparison_align_0716.csv")
# a_raw = read_csv("D:/Experiment Data/Navigation and Self/GridCode_Analysis/Betas_fMRI_comparison_pmod_motionCorrection_0720.csv")


a <- a_raw[,-c(8:13)] 
a <- subset(a, a$Subject !=22)
a <- subset(a, a$Subject !=26)
a <- subset(a, a$Subject !=13)

#exclude Subj 12/ round 4
# a <- subset(a, a$Subject != 16 & a$Subject != 20 & a$Subject != 4 & a$Subject != 22 & a$Subject != 27)
# a <- subset(a, a$Subject !=22) # & a$Subject != 13) # 13
# a <- subset(a, a$Subject !=13)
# a <- subset(a, a$Subject !=2 & a$Subject !=4 & a$Subject !=5 & a$Subject !=6 & a$Subject !=12 & a$Subject !=13 & a$Subject !=26) 
# 
# a <- subset(a,!(a$Subject == 6 & a$Round ==2)) #& !(a$Subject == 3 & a$Round ==5)
# a <- subset(a,!(a$Subject == 12 & a$Round ==4)) #  & a$Subject != 13 )
# a <- subset(a,!(a$Subject == 26 & a$Round ==5))
# a <- subset(a,!(a$Subject == 27 & a$Round ==2))
# a <- subset(a,!(a$Subject == 11 & a$Round ==5))
# a <- subset(a,!(a$Subject == 26 & a$Round ==5)) #  & a$Subject != 13 )

a$Condition = ifelse(a$Condition == 1, 'No-body', 'Body')

aNorm <-data.frame() #Subject-wise normalization

for (i in 1:27)
{
  for (j in 1:4) # Type ([withCond indiv/all, w/oCond indiv/all])
  {
    for( k in 1:3)
    {
      for( l in 1:3)
      {
        tempSubj <- subset(a, Subject == i & Type == j & GLM1_ROI == k & GLM2_ROI == l)
        temp <- data.frame(Subject = tempSubj$Subject,Condition = tempSubj$Condition, Type = tempSubj$Type, GLM1_ROI = tempSubj$GLM1_ROI, GLM2_ROI = tempSubj$GLM2_ROI, Round = tempSubj$Round, BetaNorm = c(scale(tempSubj$Beta)))
        
        aNorm <- rbind(aNorm,temp)
      }
    }
  }
}
a <- merge(a,aNorm)

a_outlier <- subset(a, (a$BetaNorm > 2 | a$BetaNorm < -2))

a <- subset(a, !(a$Subject == 12 & a$Round ==4))
a$Subject = factor(a$Subject)
a$Type = factor(a$Type)
a$GLM1_ROI = factor(a$GLM1_ROI)
a$GLM2_ROI = ifelse(a$GLM2_ROI == 1, 'rEC',ifelse(a$GLM2_ROI == 2, 'lEC','bi_ECs'))

###
behav_raw = read_csv("D:/Experiment Data/Navigation and Self/GridCode_Analysis/Memory_data_fMRI_0502.csv")
behav_raw$TimeSpent = behav_raw$EndTime - behav_raw$StartTime # Timespent during retrieval

behav <- subset(behav_raw,  Index != 1 & Index != 14) # first trial - start from the center/ last trial - with virtual threat

NormalizedData <-data.frame() ## Normalization within subject
for (i in 1:27)
{
  temp <- subset(behav, Subject == i)
  temp2 <- cbind(scale(temp$dError), scale(temp$dMoved), scale(temp$estimation_dError))
  NormalizedData <- rbind(NormalizedData,temp2)
}

behav$distNorm <- NormalizedData$V1
behav$dMovedNorm <- NormalizedData$V2
behav$estimationNorm <- NormalizedData$V3

behav_data <- behav

RoundQ = aggregate(cbind(dError,dMoved,TimeSpent,distNorm, dMovedNorm,estimationNorm, Q1,Q2,Q3,Q4,Q1z,Q2z,Q3z,Q4z) ~  Subject + Condition +Round, behav_data, mean)
RoundQ$Condition = ifelse(RoundQ$Condition == 1, 'No-body', 'Body')

# Type [withCond indiv/all, w/oCond indiv/all]
a_wCond_indiv <- subset(a,a$Type == 1) 
a_wCond_all <- subset(a,a$Type == 2) 
a_woCond_indiv <- subset(a,a$Type == 3) 
a_woCond_all <- subset(a,a$Type == 4) 

temp_betas <- a_wCond_indiv

### GLM1 : Right EC / Left / Bilateral
aR <- subset(temp_betas, temp_betas$GLM1_ROI == 1)
aL <- subset(temp_betas, temp_betas$GLM1_ROI == 2)
aB <- subset(temp_betas, temp_betas$GLM1_ROI == 3)

#Raw Beta
ggplot(aR, aes(y=Beta, x=GLM2_ROI, color=Condition)) + geom_boxplot(outlier.shape = NA) + geom_point(position=position_jitterdodge(jitter.width =.1 ))# + facet_wrap(~Subject)
ggplot(aL, aes(y=Beta, x=GLM2_ROI, color=Condition)) + geom_boxplot(outlier.shape = NA) + geom_point(position=position_jitterdodge(jitter.width =.1 ))# + facet_wrap(~Subject)
ggplot(aB, aes(y=Beta, x=GLM2_ROI, color=Condition)) + geom_boxplot(outlier.shape = NA) + geom_point(position=position_jitterdodge(jitter.width =.1 ))# + facet_wrap(~Subject)

#Normalized - Subject-wise
ggplot(aR, aes(y=BetaNorm, x=GLM2_ROI, color=Condition)) + geom_boxplot(outlier.shape = NA) + geom_point(position=position_jitterdodge(jitter.width =.1 ))# + facet_wrap(~Subject)
ggplot(aL, aes(y=BetaNorm, x=GLM2_ROI, color=Condition)) + geom_boxplot(outlier.shape = NA) + geom_point(position=position_jitterdodge(jitter.width =.1 ))# + facet_wrap(~Subject)
ggplot(aB, aes(y=BetaNorm, x=GLM2_ROI, color=Condition)) + geom_boxplot(outlier.shape = NA) + geom_point(position=position_jitterdodge(jitter.width =.1 ))# + facet_wrap(~Subject)

b <- aB
bR <-subset(b, GLM2_ROI == 'rEC')
bL <-subset(b, GLM2_ROI == 'lEC')
bB <-subset(b, GLM2_ROI == 'both_ECs')

# ggplot(bR, aes(y=Beta, x=Condition, color=Condition)) + geom_boxplot(outlier.shape = NA) + geom_jitter(width=.1,height=0,alpha=.5,size=3) + facet_wrap(~Subject,scales='free')
# ggplot(bL, aes(y=Beta, x=Condition, color=Condition)) + geom_boxplot(outlier.shape = NA) + geom_jitter(width=.1,height=0,alpha=.5,size=3) + facet_wrap(~Subject,scales='free')
# ggplot(bB, aes(y=Beta, x=Condition, color=Condition)) + geom_boxplot(outlier.shape = NA) + geom_jitter(width=.1,height=0,alpha=.5,size=3) + facet_wrap(~Subject,scales='free')

temp_correl <- merge(bB,RoundQ)
temp_correl_body <- subset(temp_correl, Condition == 'Body')
temp_correl_nobody <- subset(temp_correl, Condition != 'Body')

ggplot(temp_correl, aes(y=Beta, x=Q1, color=Condition)) + geom_point(alpha=.7,size=1) + geom_smooth(method="lm", se = FALSE, fill = "NA")
cor.test(~Beta + Q1 , data = temp_correl, method ="spearman", continuity = TRUE, conf.level = 0.95)

m1 = lmer(Beta ~ Condition + (1|Subject),temp_correl)
m2 = lmer(Beta ~ Condition + (Condition|Subject),temp_correl)

m1 = lmer(Beta ~ Q1 + (1|Subject),temp_correl_nobody)
m2 = lmer(Beta ~ Q1 + (Q1|Subject),temp_correl_nobody)

anova(m1,m2)
summary(m1)
anova(m1)
summary(m2)
anova(m2)

sjp.lmer(m1)
sjp.lmer(m1,type = "pred", var = "Q1")
sjp.lmer(m2,type = "rs.ri")

sjp.lmer(m1,type = "eff")
plot_model(m1)
plot_model(m1,type = "pred", terms = "Q1")


plot(fitted(m1),residuals(m1))
hist(residuals(m1))
qqnorm(residuals(m1))


# wilcox.test(b_6fold$Beta_R, mu = 0, alternative = 'greater')
# wilcox.test(b$Beta_B[b$xFold==6],b$Beta_B[b$xFold==5],paired=T)
# 
# t.test(b_6fold$Beta_R, mu = 0, alternative = 'greater')
# t.test(b$Beta_R[b$xFold==6],b$Beta_R[b$xFold==4],paired=T)

Q5_raw = read_csv("D:/Experiment Data/Navigation and Self/GridCode_Analysis/Memory_data_fMRI_Q5_0502.csv")
Q5_raw$Condition = ifelse(Q5_raw$Condition == 1, 'No-body', 'Body')

indivdata = aggregate(cbind(Beta, BetaNorm) ~ Subject + Condition + Type + GLM1_ROI + GLM2_ROI, a, mean)
indivQ = aggregate(cbind(dError,dMoved,TimeSpent,distNorm, dMovedNorm,estimationNorm,Q1,Q2,Q3,Q4,Q1z,Q2z,Q3z,Q4z) ~ Subject + Condition, RoundQ, mean)
indivQ <- merge(indivQ,Q5_raw)

# Type [withCond indiv/all, w/oCond indiv/all]
indivdata_wCond_indiv <- subset(indivdata,indivdata$Type == 1) 
indivdata_wCond_all <- subset(indivdata,indivdata$Type == 2) 
indivdata_woCond_indiv <- subset(indivdata,indivdata$Type == 3) 
indivdata_woCond_all <- subset(indivdata,indivdata$Type == 4) 

indiv_temp <- subset(indivdata,Type == 1 | Type == 2)
indiv_temp <- subset(indiv_temp,GLM1_ROI == 3 & GLM2_ROI == 'bi_ECs')
indiv_temp$Type = ifelse(indiv_temp$Type == 1, 'singleRun', 'OverAll')

# indiv_temp <- subset(indivdata,Type == 1 | Type == 3)
# indiv_temp <- subset(indiv_temp,GLM1_ROI == 3 & GLM2_ROI == 'rEC')
# indiv_temp$Type = ifelse(indiv_temp$Type == 1, 'withCond', 'withoutCond')

ggplot(indiv_temp, aes(y=Beta, x=Type, color=Condition)) + geom_boxplot(outlier.shape = NA)+ geom_point(alpha =.7, size=2.5,position=position_jitterdodge(jitter.width =.1 )) #+ geom_jitter(width=.1,height=0,alpha=.5,size=3) #+ facet_wrap(~Subject)
ggplot(indiv_temp, aes(y=BetaNorm, x=Type, color=Condition)) + geom_boxplot(outlier.shape = NA)+ geom_point(alpha =.7, size=2.5,position=position_jitterdodge(jitter.width =.1 )) #+ geom_jitter(width=.1,height=0,alpha=.5,size=3) #+ facet_wrap(~Subject)


temp_indiv <- indivdata_wCond_indiv

indiv_R <- subset(temp_indiv, temp_indiv$GLM1_ROI == 1)
indiv_L <- subset(temp_indiv, temp_indiv$GLM1_ROI == 2)
indiv_B <- subset(temp_indiv, temp_indiv$GLM1_ROI == 3)

indiv <- indiv_B

ggplot(indiv, aes(y=Beta, x=GLM2_ROI, color=Condition)) + geom_boxplot(outlier.shape = NA)+ geom_point(alpha =.7, size=2.5,position=position_jitterdodge(jitter.width =.1 )) #+ geom_jitter(width=.1,height=0,alpha=.5,size=3) #+ facet_wrap(~Subject)
ggplot(indiv, aes(y=BetaNorm, x=GLM2_ROI, color=Condition)) + geom_boxplot(outlier.shape = NA) + geom_point(alpha =.7, size=2.5,position=position_jitterdodge(jitter.width =.1 )) #+ geom_jitter(width=.1,height=0,alpha=.5,size=3) #+ facet_wrap(~Subject)

indiv_GLM2_R <- subset(indiv,GLM2_ROI == 'rEC')
indiv_GLM2_L <- subset(indiv,GLM2_ROI == 'lEC')
indiv_GLM2_B <- subset(indiv,GLM2_ROI == 'bi_ECs')

indiv_GLM2 <- indiv_GLM2_B
indiv_GLM2 <- indiv_GLM2 %>% group_by(Subject) %>% mutate(Body_Effect = (Beta[Condition=='Body'] - Beta[Condition=='No-body']))

indiv_GLM2 <- indiv_GLM2[with(indiv_GLM2, order(Subject)),]

ggplot(indiv_GLM2, aes(y=BetaNorm, x= Condition, color=Condition)) + geom_boxplot(outlier.shape = NA) + geom_jitter(width=.1,height=0,alpha=.5,size=3) #+ facet_wrap(~Subject)
ggplot(indiv_GLM2, aes(y=Beta, x=Condition, group=Condition, color=Condition)) + geom_boxplot(outlier.shape = NA) + geom_jitter(width=.1,height=0,alpha=.5,size=3)

ggplot(indiv_GLM2, aes(y=Beta, x=Condition, group=Subject, color=Condition)) + geom_line(aes(color=Body_Effect>0),size = 0.8) + geom_point(size = 2.5) + scale_color_manual(values=c("#F8766D", "#00BFC4", "#00BFC4", "#F8766D"))
ggplot(indiv_GLM2, aes(y=BetaNorm, x=Condition, group=Subject, color=Condition)) + geom_line(aes(color=Body_Effect>0),size = 0.8) + geom_point(size = 2.5) + scale_color_manual(values=c("#F8766D", "#00BFC4", "#00BFC4", "#F8766D"))

indiv_mean_sd <- indiv_GLM2 %>% group_by(Condition) %>% summarise_each(funs(mean,sd,se=sd(.)/sqrt(n())))

p<- ggplot(indiv_mean_sd, aes(y=Beta_mean, x=Condition,fill=Condition)) + geom_bar(stat="identity",width=.6) + geom_errorbar(aes(ymin=Beta_mean-Beta_se,ymax=Beta_mean+Beta_se), size = .3, width=.2,position=position_dodge(.9)) + ylab("Grid-like Activity") #+ scale_fill_manual(values=c("#AAAAAA", "#AAAAAA", "#F8766D", "#AAAAAA"))#+ ylim(0,20)
# p<- ggplot(indiv_mean_sd, aes(y=BetaNorm_mean, x=Condition,fill=Condition)) + geom_bar(stat="identity",width=.6) + geom_errorbar(aes(ymin=BetaNorm_mean-BetaNorm_se,ymax=BetaNorm_mean+BetaNorm_se), size = .3, width=.2,position=position_dodge(.9)) #+ scale_fill_manual(values=c("#AAAAAA", "#AAAAAA", "#F8766D", "#AAAAAA"))#+ ylim(0,20)

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

wilcox.test(indiv_GLM2$Beta[indiv_GLM2$Condition == 'Body'], indiv_GLM2$Beta[indiv_GLM2$Condition == 'No-body'], paired=T)
wilcox.test(indiv_GLM2$BetaNorm[indiv_GLM2$Condition == 'Body'], indiv_GLM2$BetaNorm[indiv_GLM2$Condition == 'No-body'], paired=T)

indiv_GLM2 <- indiv_GLM2_B
temp_correl <- merge(indiv_GLM2,indivQ)
temp_correl$QSum <- temp_correl$Q1 + temp_correl$Q2 + temp_correl$Q5
temp_correl_body <- subset(temp_correl, Condition == 'Body')
temp_correl_Nobody <- subset(temp_correl, Condition != 'Body')

ggplot(temp_correl, aes(y=Beta, x=Q5, color=Condition)) + geom_point(alpha=.7,size=3) + geom_smooth(method="lm", se = FALSE, fill = "NA")
cor.test(~Beta + Q2 , data = temp_correl_body, method ="spearman", continuity = TRUE, conf.level = 0.95)
