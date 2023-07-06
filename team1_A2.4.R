#####TUTORIAL 2: LOGISTOCAL REGRESSION#####
install.packages('readxl')
library('readxl')
Salesforce <- read_excel('/Users/el/Documents/SS-VUA/Data/Data2.xlsx')

###A2.1: Descriptives###
summary(Salesforce)

cor(Salesforce)
round((cor(Salesforce)),2)

install.packages('tidyverse')
library(tidyverse)
library(dplyr)
library(ggplot2)
library(tidyverse)
head(Salesforce)

perTech <- Salesforce %>% group_by(technician) %>% summarise(count = n(),
act = sum(ACT), rel_act=sum(ACT)/n())
av_obsperTech <- perTech %>% summarise(avg_obs = mean(obs), avg_act = mean(act), avg_rel_act=mean(act)/mean(obs))


###A2.2: 
perSit <- Salesforce %>% group_by(OWSF,OWSS,WWSF,WWSS) %>% summarise(count = n(), act = sum(ACT), rel_act = sum(ACT/n())) 

barplot(perSit)
str(perSit)
vect_perSit <- as.vector(perSit)

barplot(perSit$rel_act)
barplot(perSit$rel_act,main = 'activity level per service situation', xlab = 'service situation', ylab = 'activity level')

###A2.3:
Lin_reg1 <- lm(Salesforce$ACT~Salesforce$OWSF+Salesforce$OWSS+Salesforce$WWSF+Salesforce$WWSS)
summary(Lin_reg1)

Log_reg1 <- glm(Salesforce$ACT~Salesforce$OWSF+Salesforce$OWSS+Salesforce$WWSF+Salesforce$WWSS, 
                family = binomial(link = 'logit') )
summary(Log_reg1)




###If you dont control = gets sucked up in the error ? ok, adding IV = controlling for IV###
###A2.4:
Log_reg2 <- glm(Salesforce$ACT ~ Salesforce$OWSF+Salesforce$WWSS+Salesforce$WWSS+Salesforce$SPEC+Salesforce$TRAIN+Salesforce$LEAD+Salesforce$TEXP+Salesforce$TSERV+Salesforce$JOIN+Salesforce$LEAV+Salesforce$SIZE+Salesforce$OPEN+Salesforce$PAY+Salesforce$DUR+Salesforce$ASTAT+Salesforce$FAM+Salesforce$CSERV+Salesforce$CEXP,family = binomial(link = 'logit'))
summary(Log_reg2)

