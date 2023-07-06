library(ggplot2)
library(MASS)
library(readxl)
library(car)
library(psych)
library(lmtest)
library(lubridate)
library(dplyr)
library(tidyr)
library(tidyverse)





#### loading data ###
setwd("/Users/letchkov/Dropbox/01_Letchkov/05 Arbeit/2020-heute_VU Amsterdam/2_Lehre/8_Amsterdam Summer School (VU)/3_Logistic Regression/Tutorial Logistic Regression")
ssf <- read_excel("./2_LogisticRegression_Tutorial_Data.xlsx")

#### Question 1. Summary descriptive statistics ++++++++++++++++++++++++++++++++++++++++++++++++++++++
str(ssf)
summary(ssf)
ssf %>% select(-SUC, -LTIME) %>% cor() %>% round(3) #because SUC and LTIME are only observed if an offer was made

# Number of technicians
obs_per_technician <- ssf %>% group_by(technician) %>% dplyr::summarize(obs = n(), act = sum(ACT), rel_act = sum(ACT)/n())
average_obs <- obs_per_technician %>% summarize(avg_obs = mean(obs), avg_act = mean(act), avg_rel_act=mean(act)/mean(obs))




















#### Question 2: Model-free evidence ++++++++++++++++++++++++++++++++++++++++++++++++++++
# Number of cases per service situation
ssf$servSituation <- as.factor(ifelse(ssf$OWSF==1,"OWSF",ifelse(ssf$OWSS==1,"OWSS",ifelse(ssf$WWSF==1,"WWSF","WWSS"))))
obs_per_situation <- ssf %>% group_by(servSituation) %>% dplyr::summarize(obs = n(), act = sum(ACT), rel_act = sum(ACT)/n())

modelFree <- ggplot(data=obs_per_situation, aes(y=rel_act, x=servSituation)) + #global plot settings
  geom_bar(width = 0.5, stat='identity') +
  scale_y_continuous(expand = c(0,0), limits = c(0, .20))+ #removes the white space underneath the bars
  geom_text(aes(label=round(rel_act,2)), position=position_dodge(width=0.9), vjust=-1)+
  labs(y="Relative Sales Activity", x="Service Situation")+
  theme(  panel.grid.major = element_blank(), #eliminates the grid lines
          panel.grid.minor = element_blank(), #eliminates the grid lines
          axis.line = element_line(colour = "black", size = 0.4, #changes settings for the y- and x-axes lines
                                   linetype = 1, lineend = "butt"),
          panel.background = element_blank(),
          axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0), size=14),
          axis.title.x = element_text(size=14),
          text = element_text(family = "Times New Roman")) #changes the font
print(modelFree)





#### Question 3: Linear regression ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
linReg <- lm(ACT ~ OWSF + OWSS  + WWSS, data=ssf)
summary(linReg)









#### Question 4: Nonlinear (logistic) regression +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
logReg <- glm(ACT ~ OWSF + OWSS  + WWSS, data=ssf, family=binomial(link="logit"))
summary(logReg)
# Interpretation of coefficients (odds)
odds_owsf <- exp(logReg$coefficients[2])
odds_owss <- exp(logReg$coefficients[3])
odds_wwsf <- exp(logReg$coefficients[4])

#### Question 5: Logistic regression with covariates +++++++++++++++++++++++++++++++++++++++++++++++++++++
logReg2 <- glm(ACT ~ OWSF + OWSS + WWSF + WWSS + SPEC + TRAIN 
              + LEAD + TEXP + TSERV + JOIN + LEAV + SIZE 
              + OPEN + CEXP + CSERV + FAM + ASTAT + DUR + PAY
              , data=ssf, family=binomial(link="logit"))
summary(logReg2)

#### Question 6: Moderation effects in the service situations
logRegInt <- glm(ACT ~ OWSF + OWSS + WWSF + WWSS + SPEC + TRAIN + LEAD + TEXP + TSERV + JOIN + LEAV + SIZE + OPEN + CEXP
              + CSERV + FAM + ASTAT + DUR + PAY
              + OWSF:SPEC
              + OWSS:SPEC
              + WWSF:SPEC
              + OWSF:TRAIN
              + OWSS:TRAIN
              + WWSF:TRAIN
              , data=ssf, family="binomial")
summary(logRegInt)
