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


Water_budget<- read_csv("./Data/Water Budget/Water Budget.csv") #import if TP budget needs to recalculated
WQ_Data_Tidy <- read_csv("./Data/WQ Data/WQ_Data_Tidy.csv")  #import if TP budget needs to recalculated




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
filter(`Date Time`>"2021-06-01 00:00:00") %>%
filter(`Date Time`<"2022-05-24 24:00:00") %>%  
mutate(`Outflow (cfs)`=`Outflow (cfs)`*-1) %>% #discharged values are negative to reflect water leaving the cell. Converted to positive for TP budget 
mutate(`Cumulative Outflow`=cumsum(`Outflow (cfs)`))  %>%
mutate(`Chara Int`=na.approx(Chara,na.rm = TRUE,rule=2)) %>%
mutate(`Bare Int`=na.approx(Bare,na.rm = TRUE,rule=2)) %>%
mutate(`Typha Int`=na.approx(Typha,na.rm = TRUE,rule=2)) %>%
mutate(`Naiad Int`=na.approx(Naiad,na.rm = TRUE,rule=2)) %>%
mutate(`Mixed Int`=na.approx(Mixed,na.rm = TRUE,rule=2)) %>%  
mutate(`Load Chara`=`Outflow (cfs)`*`Chara Int`*28.316847*60/1000000) %>% #convert to P load kg. 28.31 L/cf *60 sec * 1g/1000mg * 1kg/1000g
mutate(`Load Bare`=`Outflow (cfs)`*`Bare Int`*28.316847*60/1000000) %>% #convert to P load kg. 28.31 L/cf *60 sec * 1g/1000mg * 1kg/1000g
mutate(`Load Typha`=`Outflow (cfs)`*`Typha Int`*28.316847*60/1000000) %>% #convert to P load kg. 28.31 L/cf *60 sec * 1g/1000mg * 1kg/1000g
mutate(`Load Naiad`=`Outflow (cfs)`*`Naiad Int`*28.316847*60/1000000) %>% #convert to P load kg. 28.31 L/cf *60 sec * 1g/1000mg * 1kg/1000g
mutate(`Load Mixed`=`Outflow (cfs)`*`Mixed Int`*28.316847*60/1000000) %>% #convert to P load kg. 28.31 L/cf *60 sec * 1g/1000mg * 1kg/1000g
mutate(`Cumulative TP Chara`=cumsum(`Load Chara`)) %>%
mutate(`Cumulative TP Bare`=cumsum(`Load Bare`)) %>%  
mutate(`Cumulative TP Typha`=cumsum(`Load Typha`)) %>%
mutate(`Cumulative TP Naiad`=cumsum(`Load Naiad`)) %>% 
mutate(`Cumulative TP Mixed`=cumsum(`Load Mixed`)) %>%
mutate(`FWM Chara`=`Cumulative TP Chara`/(`Cumulative Outflow`*28.316847*60)*1000000)  %>%  #convert to FWM TP mg/L
mutate(`FWM Bare`=`Cumulative TP Bare`/(`Cumulative Outflow`*28.316847*60)*1000000)  %>%
mutate(`FWM Typha`=`Cumulative TP Typha`/(`Cumulative Outflow`*28.316847*60)*1000000) %>%
mutate(`FWM Naiad`=`Cumulative TP Naiad`/(`Cumulative Outflow`*28.316847*60)*1000000) %>%
mutate(`FWM Mixed`=`Cumulative TP Mixed`/(`Cumulative Outflow`*28.316847*60)*1000000) 


write.csv(TP_Budget,"./Data/P Budget/TP_Budget.csv",row.names = FALSE)


