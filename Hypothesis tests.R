#The purpose of this script is to use hypothesis test to determine if there are differences between ECOTOPES 


library(dplyr)
library(ggplot2)
library(tidyr)
library(stringr)
library(lubridate)
library(scales)
library(RColorBrewer)
library(viridis)
library(Hmisc)
library(ggpmisc)
library(ggrepel)
library(zoo)
library(readxl)
library(corrplot)
library(GGally)
library(readxl)
library(readr)
library(BSDA)

# Import Data -------------------------------------------------------------


WQ_Data <- read_excel("Data/WQ Data/WQ Data.xlsx",sheet = "Sheet1")
WQ_Upstream_Downstream_Tidy <- read_csv("./Data/WQ Data/WQ_Upstream_Downstream_Tidy.csv")
WQ_Data_Tidy <- read_csv("./Data/WQ Data/WQ_Data_Tidy.csv")


# Wilcoxon Ranked Sign Test ---Test upstream vs downstream values for each ecotope------------------------------------
Sign_Test_TP <- WQ_Upstream_Downstream_Tidy %>%  
filter(TEST_NAME=="TPO4")        #filter to only TPO4

#run Wilcoxon ranked sign test fro each ecotope
HPT_Naiad <-SIGN.test(x = filter(Sign_Test_TP,Ecotope=="Naiad")$`Upstream Values`, y = filter(Sign_Test_TP,Ecotope=="Naiad")$`Downstream Values`,md = 0,alternative = "two.sided",conf.level = 0.95)
HPT_Bare <-SIGN.test(x = filter(Sign_Test_TP,Ecotope=="Bare")$`Upstream Values`, y = filter(Sign_Test_TP,Ecotope=="Bare")$`Downstream Values`,md = 0,alternative = "two.sided",conf.level = 0.95)
HPT_Mixed <-SIGN.test(x = filter(Sign_Test_TP,Ecotope=="Mixed")$`Upstream Values`, y = filter(Sign_Test_TP,Ecotope=="Mixed")$`Downstream Values`,md = 0,alternative = "two.sided",conf.level = 0.95)
HPT_Typha <-SIGN.test(x = filter(Sign_Test_TP,Ecotope=="Typha")$`Upstream Values`, y = filter(Sign_Test_TP,Ecotope=="Typha")$`Downstream Values`,md = 0,alternative = "two.sided",conf.level = 0.95)
HPT_Chara <-SIGN.test(x = filter(Sign_Test_TP,Ecotope=="Chara")$`Upstream Values`, y = filter(Sign_Test_TP,Ecotope=="Chara")$`Downstream Values`,md = 0,alternative = "two.sided",conf.level = 0.95)

#extract hypothesis test parameters from each test run
P.values <- c(HPT_Naiad$p.value,HPT_Bare$p.value,HPT_Mixed$p.value,HPT_Typha$p.value,HPT_Chara$p.value)
Ecotope <- c("Naiad","Bare","Mixed","Typha","Chara")
CI_low <- c(HPT_Naiad$conf.int[1],HPT_Bare$conf.int[1],HPT_Mixed$conf.int[1],HPT_Typha$conf.int[1],HPT_Chara$conf.int[1])
CI_high <- c(HPT_Naiad$conf.int[2],HPT_Bare$conf.int[2],HPT_Mixed$conf.int[2],HPT_Typha$conf.int[2],HPT_Chara$conf.int[2])

HPT_table <- data.frame(Ecotope,P.values,CI_low,CI_high)  #combine extracted parameters in a table

write.csv(HPT_table,"./Data/Publish Tables/Upstream vs Downstream by Ecotope.csv",row.names = FALSE)


# Anova Test TP --test for differences between each station from all Ecotopes ----------------------https://www.scribbr.com/statistics/anova-in-r/

AOV_TP <- WQ_Data %>%
filter(SAMPLE_TYPE=="SAMP")  %>%
filter(TEST_NAME=="TPO4")       #filter to only TPO4 
  
one.way <- aov(VALUE ~ STATION, data = AOV_TP)

summary(one.way)

TukeyHSD(one.way)
