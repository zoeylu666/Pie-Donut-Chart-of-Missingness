library(ggplot2)
library(tidyverse)
library(dplyr)
library(RColorBrewer)

require(moonBook)
require(webr)

data <-  read_csv("./R/DataViz1/Donut-Pie/math courses.csv")

#data wrangling

head(data)
tail(data)

df <- as.data.frame(data)

df[is.na(df)] = 0

df1 <-  df

#Set the appropriate type of variables

df1$Level1 <- as.factor(df1$Level1)
df1$Level2 <- as.factor(df1$Level2)
df1$Level3 <- as.factor(df1$Level3)
df1$Level4 <- as.factor(df1$Level4)


unique(df$Level1)
 df1 <- df1 %>% mutate(Level1 = recode(Level1, 
                  "0" = "0", 
                  "CP_Math1" = "1",
                  "H_Math2" = "1",
                  "H_Math1" = "1",
                  "H_Math3" = "1",
                    "CP_Math2"= "1"
                  ))
 
 unique(df$Level2)
 
 df1 <- df1 %>% mutate(Level2 = recode(Level2,  
                                       "0" = "0", 
                    "CP_Math1" = "1",
                    "CP_GeomA"  = "1",
                    "CP_Math2" = "1",
                    "H_Math3" = "1",
                    "H_Math2" = "1",
                    "H_Math4"  = "1",
                    "AP_Stats" = "1",
                    "CP_Math3" = "1",
                    "H_Calc"  = "1",
                    "CP_IMP2"  = "1" 
                    ))
 
 
 unique(df$Level3)
 df1 <- df1 %>% mutate(Level3 = recode(Level3,  
                                       "0"= "0", 
                                       "CP_Math3" = "1",
                                       "CP_Math3A"   = "1",
                                       "H_Math4" = "1",
                                       "CP_Math2" = "1",
                                       "CP_Math4" = "1",
                                       "AP_Stats" = "1",
                                       "H_Math3"  = "1",
                                       "CP_AdvAlgFin"= "1",
                                       "CP_StatsProb"  = "1",
                                       "AP_CalcAB"  = "1",
                                       "CP_Alg2A"= "1",
                                       "H_Calc" = "1",
                                       "DE_DSU_MTSC122" = "1"
 ))
 
 unique(df$Level4)
 
 df1 <- df1 %>% mutate(Level4 = recode(Level4,  
                                       "0" = "0", 
                                       "CP_Math4"  = "1",
                                       "AP_CalcBC"    = "1",
                                       "CP_StatsSports" = "1",
                                       "H_Calc" = "1",
                                       "CP_StatsProb" = "1",
                                       "CP_Math3B" = "1",
                                       "H_Math4"   = "1",
                                       "DE_DSU_MTSC122" = "1",
                                       "AP_CalcAB"  = "1",
                                       "AP_Stats"  = "1",
                                       "CP_Math3" = "1",
                                       "CP_AdvAlgFin"  = "1",
                                       "CP_Math3A"  = "1"
 ))
 

df2 <- data.frame(df1)


df2$Level1 <- as.numeric(as.character(df2$Level1))
df2$Level2 <- as.numeric(as.character(df2$Level2))
df2$Level3 <- as.numeric(as.character(df2$Level3))
df2$Level4 <- as.numeric(as.character(df2$Level4))

df2 <-  df2  %>% mutate(sum = rowSums(.[1:4]))
df2 <-  df2 %>% mutate(incomplete = 4-sum)

unique(df2$incomplete)

df2$missing <- as.factor(ifelse(df2$incomplete > 0, 'Missing', "No Missing"))

df2$missing_level <- as.factor(ifelse(df2$incomplete == 1, 'Missing 1',
                                      ifelse(df2$incomplete == 2, 'Missing 2',
                                             ifelse(df2$incomplete == 3, 'Missing 3',
                                             "Complete"))))


df3 <-  df2 %>% select(incomplete:missing_level)


 ###Pie-Donut Chart
 
 PieDonut(df3, aes(pies=missing,donuts=missing_level),selected=1,labelposition=1, explode=1,
          title="The % Class 2020 Students @ XX HS Missing Math Courses")

 

 

 
 