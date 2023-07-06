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
head(Salesforce)

perTech <- Salesforce %>% group_by(technician) %>% summarise(count = n(), )
  
hist(Salesforce$ACT,Salesforce$observation)

