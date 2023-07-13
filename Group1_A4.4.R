library(plyr)
library(stargazer)
library(sandwich) 
library(xts)
library(pander)
library(lmtest)
library(formattable)
library(data.table)
library(mvtnorm)
library(ggplot2)
library(MASS)
library(tidyverse)
library(readxl)
library(car)
library(psych)
library(lubridate)
library(stringr)
library(rmarkdown)
library(knitr)
library(DT)
library(magrittr)
library(png)
library(plm)

options(digits=3)

panelds = read_excel("./Tutorial/3_PanelTutorial_Data_Indulgence.xlsx",col_names = TRUE)

panelds = panelds[,2:16]

price_by_redp = panelds %>% group_by(redemption) %>% summarize(avg_price = sum(avg_item_price_dollar)/n())

ln_panelds = log(panelds)
colnames(ln_panelds) = paste("ln", colnames(ln_panelds), sep = '_')
ln_panelds = cbind(panelds,ln_panelds)


poollm = plm(ln_avg_item_price_dollar~
              redemption
            + n_items_guest
            + post_redemption
            + points_rule_15
            + n_guests_final
            + time_since_last
            + membership_length
            + weekend
            + lunch
            + birthday
            + as.numeric(visit_id)
            + as.factor(Brand_id)
            , data = ln_panelds, model = "pooling", index = c("CustomerID", "visit_id"))

summary(poollm)


fixlm = plm(ln_avg_item_price_dollar~
              redemption
            + n_items_guest
            + post_redemption
            + points_rule_15
            + n_guests_final
            + time_since_last
            + membership_length
            + weekend
            + lunch
            + birthday
            + as.numeric(visit_id)
            + as.factor(Brand_id)
            , data = ln_panelds, model = "within", index = c("CustomerID", "visit_id") )

summary(fixlm)

randlm = plm(ln_avg_item_price_dollar~
              redemption
            + n_items_guest
            + post_redemption
            + points_rule_15
            + n_guests_final
            + time_since_last
            + membership_length
            + weekend
            + lunch
            + birthday
            + as.numeric(visit_id)
            + as.factor(Brand_id)
            , data = ln_panelds, model = "random", index = c("CustomerID", "visit_id") )

summary(randlm)



fixpvcm = plm(ln_avg_item_price_dollar~
              redemption
            + n_items_guest
            + post_redemption
            + points_rule_15
            + n_guests_final
            + time_since_last
            + membership_length
            + weekend
            + lunch
            + birthday
            + as.numeric(visit_id)
            + as.factor(Brand_id)
            , data = ln_panelds, model = "within", index = c("CustomerID", "visit_id") )

summary(fixpvcm)

#bptest(poollm)
#bptest(fixlm)

#bptest(poollm, fixlm)

pooltest(poollm, fixpvcm)
pooltest(fixlm, fixpvcm)

phtest(fixlm, randlm)



# lag_ln_ppanelds = pdata.frame(ln_panelds, index = c("CustomerID","visit_id"))
# pseriesfy(lag_ln_ppanelds)
# 
# lag_ln_ppanelds = plm::lag(lag_ln_ppanelds, k = 1L, shift = "time")



#regress residuals on their own lagged values to confirm serial correlation
fixlmres = fixlm$residuals
lag_fixlmres = plm::lag(fixlmres, k = 1L, shift = "time")
fixreslm = lm(fixlmres ~ lag_fixlmres)
summary(fixreslm)

#Wooldridge-Drukker Test for serial correlation
pwartest(fixlm)


fixlmse = coeftest(fixlm, vcov = plm::vcovHC(fixlm, cluster = "group", method = "arellano"))

summary(fixlmse)









