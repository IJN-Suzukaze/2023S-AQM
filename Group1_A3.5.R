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
library(lmtest)
library(lubridate)
library(stringr)
library(rmarkdown)
library(knitr)
library(DT)
library(magrittr)
library(png)
library(dLagM)


knitr::opts_chunk$set(echo = TRUE)
#setwd("/Users/letchkov/Dropbox/01_Letchkov/05 Arbeit/2020-heute_VU Amsterdam/2_Lehre/8_Amsterdam Summer School (VU)/4_Time Series Analysis/Tutorial Time Series Analysis/")
options(mc.cores = parallel::detectCores())
options(max.print = 10000)

osc <- read_excel("./Tutorial/3_TimeSeries_Tutorial_Data.xlsx",col_names = TRUE) 
hist(osc$community_answers_cum)
hist(osc$focal_calls)
hist(osc$firstcontact_fix_perc, breaks=20) 

# Transformation of variables
ln_osc <- log(osc+1)
#colnames(ln_osc) <- 
colnames(ln_osc) <- paste("ln",colnames(ln_osc),sep='_') #change the names to ln_X in the new table
#ln_osc$ln_firstcontact_fix_perc <- log(osc$firstcontact_fix_perc+.01) #because adding 1 to a variable from 0 to 1 could massively distort the variable
osc <- cbind(osc, ln_osc)
rm(ln_osc)

hist(osc$ln_community_answers_cum, breaks=20)
hist(osc$ln_focal_calls, breaks=20)
hist(osc$ln_firstcontact_fix_perc, breaks=20) 

plot(osc$trend,osc$community_answers_cum, type="l")
plot(osc$trend, osc$focal_calls, type="l")
plot(osc$trend, osc$firstcontact_fix_perc, type="l")

## With gg-plot
library(ggplot2)
scale = 100
index = 1:nrow(osc)

callComparison <- ggplot(data=osc) + #global plot settings
  geom_line(aes(y=community_answers_cum, x=trend, colour="community_answers_cum")) +
  geom_line(aes(y=focal_calls*scale, x=trend, colour="focal_calls")) + 
  scale_y_continuous(labels=function(x) format(x, big.mark=",", scientific=FALSE), sec.axis=sec_axis(~., name="KnowledgeBase/Call volume", labels=function(x) format(x/scale, big.mark=",", scientific=FALSE)))+
  scale_color_manual(name=element_blank(), values=c("community_answers_cum" = "steelblue", "focal_calls" = "darkgreen"), labels=c("Digital knowledge base", "Call volume"))+
  labs(y="Digital knowledge base", x="")+
  theme(  panel.grid.major = element_blank(), #eliminates the grid lines
          panel.grid.minor = element_blank(), #eliminates the grid lines
          axis.line = element_line(colour = "black", size = 0.4, #changes settings for the y- and x-axes lines
                                   linetype = 1, lineend = "butt"),
          panel.background = element_blank(),
          legend.position = c(.5,1),
          legend.justification = c("right", "top"),
          legend.direction = "vertical",
          legend.box.just = "right",
          legend.key=element_blank(), #hides the grey background for the legend
          axis.text.x=element_text(angle=90, hjust=1), #tilts the x-axis labels by 90 degrees
          axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0), size=20),
          axis.title.y.right = element_text(margin = margin(t = 0, r = 0, b = 0, l = 20), size=20),
          legend.text=element_text(size=15),
          text = element_text(family = "Times New Roman")) #changes the font
print(callComparison)


corMatrix <- osc[,1:18] %>% cor() %>% as.data.frame() %>% mutate_if(is.numeric, round, digits = 2)
DT::datatable(corMatrix, 
              options = list(pageLength = 16, autoWidth = TRUE,  searching=FALSE, paging=FALSE,
                             columnDefs = list(list(width = '10px', targets = c(1, 3))), scrollX=T, scrollY="400px", scrollCollapse=T), 
              callback = JS("return table;"), 
              class= c("hover", "compact","stripe", "row-border"), style="bootstrap")


lag_osc <- dplyr::lag(osc)
colnames(lag_osc) <- paste("lag",colnames(lag_osc),sep='_') 
osc <- cbind(osc,lag_osc)
rm(lag_osc)

regCall2 <- lm(ln_focal_calls ~  
                 lag_ln_community_answers_cum
               + ln_product_new_devices
               + ln_product_issues_all
               + callcenter_closed
               + product_campaign_days
               + product_test + first_weeks_problem + ln_community_questions + ln_community_new_users + ln_community_unique_visitors
               + season_1 + season_2 + season_3 + weekday_mo + weekday_tu + weekday_we + weekday_th + weekday_fr
               + trend,
               data=osc)

summary(regCall2)

#test different bg
bgtest(regCall2, order = 4)



regCall2coef = coeftest(regCall2, vcov = vcovHAC(regCall2, weights = weightsAndrews))
summary(regCall2coef)

fdllm = dlm(ln_focal_calls ~  
      ln_community_answers_cum
    + ln_product_new_devices
    + ln_product_issues_all
    + callcenter_closed
    + product_campaign_days
    + product_test + first_weeks_problem + ln_community_questions + ln_community_new_users + ln_community_unique_visitors
    + season_1 + season_2 + season_3 + weekday_mo + weekday_tu + weekday_we + weekday_th + weekday_fr
    + trend,
    data=osc, y = ln_focal_calls, x = ln_community_answers_cum, q = 1)
summary(fdllm)
















