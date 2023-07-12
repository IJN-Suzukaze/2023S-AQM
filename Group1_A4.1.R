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

options(digits=3)

panelds = read_excel("./Tutorial/3_PanelTutorial_Data_Indulgence.xlsx",col_names = TRUE)


price_by_redp = panelds %>% group_by(redemption) %>% summarize(priceredp = sum(avg_item_price_dollar)/n())




poollm = lm(avg_item_price_dollar~
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
            + visit_id
            
            , data = panelds)

summary(poollm)

#bptest(poollm)














