
library("readxl")
library("car")
library("tidyverse")

options(digits=3)


sale_data = read_excel("./Tutorial/2_LogisticRegression_Tutorial_Data.xlsx")

sale_data = sale_data %>% select(-ASTAT, -LTIME)

log_reg = glm(ACT ~ OWSF + OWSS + WWSS, family = binomial(link = 'logit'), data = sale_data )
summary(log_reg)

log_reg2 = glm(ACT ~ OWSF + OWSS + WWSS + SPEC + TRAIN + , family = binomial(link = 'logit'), data = sale_data)


		LEAD	TEXP	TSERV	JOIN	LEAV	SIZE	OPEN	CEXP	CSERV	FAM	ASTAT	DUR	ITEMS	LTIME	PAY
