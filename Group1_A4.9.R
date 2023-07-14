

library(sampleSelection)









disney = read.csv("./Tutorial/4_PanelTutorial_Data_Disney1.csv")

adult = disney$people-disney$children

disney = cbind(disney,adult)






ftvlm = lm(freeTVMedia_timemeanprop ~ 
             disneyadopter
           + netflix_timemeanprop
           + amazon_timemeanprop
             + gender_f
             + age 
           + educ_1 
           + educ_2 
           + educ_3 
           + income_1 
           + income_2 
           + income_3 
           + income_4 
           + income_5 
           + income_6 
           + internet_speed2_5 
           + internet_speed2_4 
           + internet_speed2_3 
           + internet_speed2_2 
           + internet_speed2_1 
           + quality 
           + overload 
           + fragmentation 
           + nostalgia 
           + criticalinfra 
           + shortwork 
           + corona_4 
           + corona_3 
           + corona_5 
           + involvement 
           + innovation_cat 
           + adult
           + children 
             ,data = disney)
summary(ftvlm)

strclm = lm(streamingcomp_timemeanprop ~ 
              disneyadopter
            + freeTVMedia_timemeanprop
          + gender_f
           + age 
           + educ_1 
           + educ_2 
           + educ_3 
           + income_1 
           + income_2 
           + income_3 
           + income_4 
           + income_5 
           + income_6 
           + internet_speed2_5 
           + internet_speed2_4 
           + internet_speed2_3 
           + internet_speed2_2 
           + internet_speed2_1 
           + quality 
           + overload 
           + fragmentation 
           + nostalgia 
           + criticalinfra 
           + shortwork 
           + corona_4 
           + corona_3 
           + corona_5 
           + involvement 
           + innovation_cat 
          + adult
           + children 
           ,data = disney)
summary(strclm)




strctm = treatReg(disneyadopter ~ 
            + gender_f
            + age 
            + educ_1 
            + educ_2 
#            + income_1 
            + income_2 
            + income_3 
            + income_4 
            + income_5 
            + income_6
            + quality 
            + overload 
            + fragmentation 
            + nostalgia 
            + criticalinfra 
            + shortwork 
            + corona_4 
            + corona_3 
            + corona_5 
            + involvement 
            + innovation_cat 
            + adult
            + children 
            + disney_sym
            + disneyinterest
            ,
            streamingcomp_timemeanprop ~ 
              disneyadopter
            + netflix_timemeanprop
            + amazon_timemeanprop
            + gender_f
            + age 
            + educ_1 
            + educ_2 
#            + income_1 
            + income_2 
            + income_3 
            + income_4 
            + income_5 
            + income_6
            + quality 
            + overload 
            + fragmentation 
            + nostalgia 
            + criticalinfra 
            + shortwork 
            + corona_4 
            + corona_3 
            + corona_5 
            + involvement 
            + innovation_cat 
            + adult
            + children 
            ,data = disney)
summary(strctm)


for(i in seq_along(disney)){
  disney[[i]] = as.numeric(disney[[i]])
}























