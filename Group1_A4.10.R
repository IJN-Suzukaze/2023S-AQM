library(sampleSelection)
library(did)


disney2 = read.csv("./Tutorial/3_PanelTutorial_Data_Disney2.csv")

adult = disney2$people-disney2$children
disney2 = cbind(disney2,adult)



strcdid = lm(streamingcomp_timemeanprop ~ 
              disneyadopter
            + time
            + disneyadopter * time
            + gender_f
            + age 
            + educ_1 
            + educ_2 
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
            ,data = disney2)
summary(strcdid)


ftvdid = lm(freeTVMedia_timemeanprop ~ 
               disneyadopter
             + time
             + disneyadopter * time
             + gender_f
             + age 
             + educ_1 
             + educ_2 
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
             ,data = disney2)
summary(ftvdid)



strctmdid = treatReg(disneyadopter ~ 
                    + gender_f
                  + age 
                  + educ_1 
                  + educ_2 
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
                  + time
                  + disneyadopter * time
                  + gender_f
                  + age 
                  + educ_1 
                  + educ_2 
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
                  ,data = disney2)
summary(strctmdid)


ftvtmdid = treatReg(disneyadopter ~ 
                       + gender_f
                     + age 
                     + educ_1 
                     + educ_2 
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
                     freeTVMedia_timemeanprop ~ 
                       disneyadopter
                     + time
                     + disneyadopter * time
                     + gender_f
                     + age 
                     + educ_1 
                     + educ_2 
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
                     ,data = disney2)
summary(ftvtmdid)





















