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


tdf = read_excel("./Tutorial/3_TimeSeries_Tutorial_Data.xlsx")

tdflm = lm(focal_calls ~ trend + month_01 + month_02+month_03+month_04+month_05+month_06+month_07+month_08+month_09+month_10+month_11+month_12 + community_answers_cum + focal_waittime_sec + community_questions +community_unique_visitors, data = tdf)
summary(tdflm)

tdflm2 = lm(focal_calls ~ trend + season_1 + season_2 + season_3 + season_4 + community_answers_cum + focal_waittime_sec + community_questions +community_unique_visitors, data = tdf)
summary(tdflm2)

tdflm3 = lm(focal_calls ~ trend+ weekday_tu+weekday_we+weekday_th+weekday_fr+weekend + community_answers_cum + focal_waittime_sec + community_questions + community_unique_visitors, data = tdf)
summary(tdflm3)






