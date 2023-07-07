library(ggplot2)
library(MASS)
library(readxl)
library(car)
library(psych)
library(lmtest)
library(lubridate)
library(tseries)
library(dplyr)
library(tidyr)
library(tidyverse)


tdf = read_excel("./Tutorial/3_TimeSeries_Tutorial_Data.xlsx")

tdf = tdf %>% mutate(months = ifelse(month_01 == 1, "month_01", ifelse(month_02 == 1, "month_02", ifelse(month_03 == 1, "month_03", ifelse(month_04 == 1, "month_04", ifelse(month_05 == 1, "month_05", ifelse(month_06 == 1, "month_06", ifelse(month_06 == 1, "month_06", ifelse(month_07 == 1, "month_07", ifelse(month_08 == 1, "month_08", ifelse(month_09 == 1, "month_09", ifelse(month_10 == 1, "month_10", ifelse(month_11 == 1, "month_11", "month_12")))))))))))))

#apply function/loop/transform dummy to factor


per_month = tdf %>%
  group_by(months) %>%
  summarize(
    fcal = mean(focal_calls)
  )
#View(per_month)






#use stargazer instead of summary in RMD output



#lag all IV by 1 day and use as focal variable




adf.test(tdf$focal_contacts)

#product_new_devices, community_unique_visitors, community_answers_cum are non-stationary




tdflm = lm(focal_calls ~ trend + month_01 + month_02+month_03+month_04+month_05+month_06+month_07+month_08+month_09+month_10+month_11+month_12 + community_answers_cum + focal_waittime_sec + community_questions +community_unique_visitors, data = tdf)
summary(tdflm)

tdflm2 = lm(focal_calls ~ trend + season_1 + season_2 + season_3 + season_4 + community_answers_cum + focal_waittime_sec + community_questions +community_unique_visitors, data = tdf)
summary(tdflm2)

tdflm3 = lm(focal_calls ~ trend+ weekday_tu+weekday_we+weekday_th+weekday_fr+weekend + community_answers_cum + focal_waittime_sec + community_questions + community_unique_visitors, data = tdf)
summary(tdflm3)






