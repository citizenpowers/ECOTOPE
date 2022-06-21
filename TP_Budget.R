#TP Budget- Goal is to calculate P balance in order to calculate flow weighted mean 
library(dplyr)
library(ggplot2)
library(readr)
library(readxl)
library(tidyr)
library(stringr)
library(lubridate)
library(scales)
library(gghighlight)
library(RColorBrewer)
library(viridis)
library(Hmisc)
library(ggpmisc)
library(ggrepel)
library(zoo)
library(dbhydroR)



# Import data -------------------------------------------------------------

Water_budget<- read_csv("./Data/Water Budget/Water Budget.csv")
WQ_Data_Tidy <- read_csv("./Data/WQ Data/WQ_Data_Tidy.csv")



# Tidy TP data ------------------------------------------------------------

TP_Tidy <- WQ_Data_Tidy %>%
filter(TEST_NAME=="TPO4",SAMPLE_TYPE=="SAMP",Position=="Downstream") %>%
select(Ecotope,COLLECT_DATE,VALUE) %>%
rename(`Date Time`="COLLECT_DATE") %>%
pivot_wider(values_from=VALUE,names_from=Ecotope)

# Calculate TP Budget -----------------------------------------------------

TP_Budget <-Water_budget %>%
select(`Date Time`,`Outflow (cfs)`) %>%  
left_join(TP_Tidy,by="Date Time") %>%
arrange(`Date Time`) %>%
mutate(`Outflow (cfs)`=`Outflow (cfs)`*-1) %>% #discharged values are negative to reflect water leaving the cell. Converted to positive for TP budget  
mutate(`Chara Int`=na.approx(Chara,na.rm = TRUE,rule=2)) %>%
mutate(`Bare Int`=na.approx(Bare,na.rm = TRUE,rule=2)) %>%
mutate(`Typha Int`=na.approx(Typha,na.rm = TRUE,rule=2)) %>%
mutate(`Mixed Int`=na.approx(Mixed,na.rm = TRUE,rule=2)) %>%
mutate(`Naiad Int`=na.approx(Naiad,na.rm = TRUE,rule=2)) %>%
filter(`Date Time`>"2021-06-01 00:00:00") %>%
filter(`Date Time`<"2022-05-24 24:00:00") %>%
mutate(`Load Chara`=`Outflow (cfs)`*28.316847*60/1000000) %>% #convert to P load kg. 28.31 L/cf *60 sec * 1g/1000mg * 1kg/1000g
mutate(`Cumulative TP Chara`=cumsum(`Load Chara`))

ggplot(Water_budget,aes(`Date Time`,`Outflow (cfs)`))+geom_point()




