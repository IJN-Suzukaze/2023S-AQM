
library("readxl")
library("car")
library("tidyverse")

options(digits=3)


sale_data = read_excel("./Tutorial/2_LogisticRegression_Tutorial_Data.xlsx")

sale_data = sale_data %>% select(-ASTAT, -LTIME, -SUC)

log_reg = glm(ACT ~ OWSF + OWSS + WWSS, family = binomial(link = 'logit'), data = sale_data )
summary(log_reg)


log_reg2 = glm(ACT ~ OWSF+OWSS+WWSS+SPEC+TRAIN+LEAD+TEXP+TSERV+JOIN+LEAV+SIZE+OPEN+PAY+DUR+FAM+CSERV+CEXP,family = binomial(link = 'logit'), data = sale_data)
summary(log_reg2)


log_reg3 = glm(ACT ~ OWSF + OWSS + WWSS, family = binomial(link = 'logit'), data = sale_data)
summary(log_reg3)




