#TP Budget- Goal is to calculate P balance in order to calculate flow weighted mean 
library(dplyr)
library(ggplot2)
library(readr)
library(readxl)
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
library(dbhydroR)



# Import data -------------------------------------------------------------


Water_budget<- read_csv("./Data/Water Budget/Water Budget.csv") #import if TP budget needs to recalculated
WQ_Data_Tidy <- read_csv("./Data/WQ Data/WQ_Data_Tidy.csv")  #import if TP budget needs to recalculated
G379_WQ <- read_csv("Data/WQ Data/G379_WQ.csv")



# Tidy TP data ------------------------------------------------------------

TP_Tidy <- WQ_Data_Tidy %>%
filter(TEST_NAME=="TPO4",SAMPLE_TYPE=="SAMP",Position=="Downstream",STA=="STA-3/4 Cell 2B") %>%
select(Ecotope,COLLECT_DATE,VALUE) %>%
rename(`Date Time`="COLLECT_DATE") %>%
pivot_wider(values_from=VALUE,names_from=Ecotope)

G379_Tidy <-G379_WQ %>%
filter(`Test Name`=="PHOSPHATE, TOTAL AS P",`Sample Type New`=="SAMP",`Collection Method`=="G") %>%   #Choose grab or ACF. Will make large difference to FWM
mutate(`Date Time`=mdy_hm(Collection_Date)) %>%  
select(`Date Time`,`Station ID`,Value) %>%
pivot_wider(names_from="Station ID",values_from="Value",values_fn=~mean(.x, na.rm = TRUE))


# Calculate TP Budget using 1-minute increments-----------------------------------------------------

TP_Budget <-Water_budget %>%
select(`Date Time`,`Outflow (cfs)`) %>%  
left_join(TP_Tidy,by="Date Time") %>%
left_join(G379_Tidy,by="Date Time") %>%
arrange(`Date Time`) %>%
#filter(`Date Time`>"2021-06-01 00:00:00") %>% #Year 1
#filter(`Date Time`<"2022-06-01 24:00:00") %>% #Year 1
filter(`Date Time`>"2022-09-15 24:00:00" ) %>% #Year 2 
filter(`Date Time`<"2023-07-02 24:00:00") %>%
#filter(`Date Time`<"2022-06-01 24:00:00" | `Date Time`>"2022-09-15 24:00:00" ) %>% #Filter out days when there was no monitoring. for period of record calculations 
mutate(`Outflow (cfs)`=`Outflow (cfs)`*-1) %>% #discharged values are negative to reflect water leaving the cell. Converted to positive for TP budget 
mutate(`Cumulative Outflow`=cumsum(`Outflow (cfs)`))  %>%
mutate(`Chara Int`=na.approx(Chara,na.rm = TRUE,rule=2)) %>%
mutate(`Bare Int`=na.approx(Bare,na.rm = TRUE,rule=2)) %>%
mutate(`Typha Int`=na.approx(Typha,na.rm = TRUE,rule=2)) %>%
#mutate(`Naiad Int`=na.approx(Naiad,na.rm = TRUE,rule=2)) %>%
mutate(`Mixed Int`=na.approx(Mixed,na.rm = TRUE,rule=2)) %>%  
mutate(`G379B Int`=na.approx(G379B,na.rm = TRUE,rule=2)) %>%
mutate(`G379D Int`=na.approx(G379D,na.rm = TRUE,rule=2)) %>%    
mutate(`Load Chara`=`Outflow (cfs)`*`Chara Int`*28.316847*60/1000000) %>% #convert to P load kg. 28.31 L/cf *60 sec * 1g/1000mg * 1kg/1000g
mutate(`Load Bare`=`Outflow (cfs)`*`Bare Int`*28.316847*60/1000000) %>% #convert to P load kg. 28.31 L/cf *60 sec * 1g/1000mg * 1kg/1000g
mutate(`Load Typha`=`Outflow (cfs)`*`Typha Int`*28.316847*60/1000000) %>% #convert to P load kg. 28.31 L/cf *60 sec * 1g/1000mg * 1kg/1000g
#mutate(`Load Naiad`=`Outflow (cfs)`*`Naiad Int`*28.316847*60/1000000) %>% #convert to P load kg. 28.31 L/cf *60 sec * 1g/1000mg * 1kg/1000g
mutate(`Load Mixed`=`Outflow (cfs)`*`Mixed Int`*28.316847*60/1000000) %>% #convert to P load kg. 28.31 L/cf *60 sec * 1g/1000mg * 1kg/1000g
mutate(`Load G379B`=`Outflow (cfs)`*`G379B Int`*28.316847*60/1000000) %>% #convert to P load kg. 28.31 L/cf *60 sec * 1g/1000mg * 1kg/1000g
mutate(`Load G379D`=`Outflow (cfs)`*`G379D Int`*28.316847*60/1000000) %>% #convert to P load kg. 28.31 L/cf *60 sec * 1g/1000mg * 1kg/1000g
mutate(`Cumulative TP Chara`=cumsum(`Load Chara`)) %>%
mutate(`Cumulative TP Bare`=cumsum(`Load Bare`)) %>%  
mutate(`Cumulative TP Typha`=cumsum(`Load Typha`)) %>%
#mutate(`Cumulative TP Naiad`=cumsum(`Load Naiad`)) %>% 
mutate(`Cumulative TP Mixed`=cumsum(`Load Mixed`)) %>%
mutate(`Cumulative TP G379B`=cumsum(`Load G379B`)) %>% 
mutate(`Cumulative TP G379D`=cumsum(`Load G379D`)) %>%  
mutate(`FWM Chara`=`Cumulative TP Chara`/(`Cumulative Outflow`*28.316847*60)*1000000)  %>%  #convert to FWM TP mg/L
mutate(`FWM Bare`=`Cumulative TP Bare`/(`Cumulative Outflow`*28.316847*60)*1000000)  %>%
mutate(`FWM Typha`=`Cumulative TP Typha`/(`Cumulative Outflow`*28.316847*60)*1000000) %>%
#mutate(`FWM Naiad`=`Cumulative TP Naiad`/(`Cumulative Outflow`*28.316847*60)*1000000) %>%
mutate(`FWM Mixed`=`Cumulative TP Mixed`/(`Cumulative Outflow`*28.316847*60)*1000000) %>%
mutate(`FWM G379B`=`Cumulative TP G379B`/(`Cumulative Outflow`*28.316847*60)*1000000) %>%
mutate(`FWM G379D`=`Cumulative TP G379D`/(`Cumulative Outflow`*28.316847*60)*1000000) 


write.csv(TP_Budget,"./Data/P Budget/TP_Budget.csv",row.names = FALSE)


# Calculate TP budget using daily averages --------------------------------

TP_Budget_Daily <-Water_budget %>%
select(`Date Time`,`Outflow (cfs)`) %>%  
left_join(TP_Tidy,by="Date Time") %>%
left_join(G379_Tidy,by="Date Time") %>%
arrange(`Date Time`) %>%
filter(`Date Time`>"2021-06-01 00:00:00") %>%
filter(`Date Time`<"2023-07-02 24:00:00") %>%  
filter(`Date Time`<"2022-06-01 24:00:00" | `Date Time`>"2022-09-15 24:00:00" ) %>% #Filter out days when there was no monitoring
mutate(Date=as.Date(`Date Time`)) %>%
group_by(Date) %>%
summarise(across(where(is.numeric),~ mean(.x, na.rm = TRUE))) %>%
mutate(`Outflow (cfs)`=`Outflow (cfs)`*-1) %>% #discharged values are negative to reflect water leaving the cell. Converted to positive for TP budget 
mutate(`Cumulative Outflow`=cumsum(`Outflow (cfs)`))  %>%
mutate(`Chara Int`=na.approx(Chara,na.rm = TRUE,rule=2)) %>%
mutate(`Bare Int`=na.approx(Bare,na.rm = TRUE,rule=2)) %>%
mutate(`Typha Int`=na.approx(Typha,na.rm = TRUE,rule=2)) %>%
mutate(`Naiad Int`=na.approx(Naiad,na.rm = TRUE,rule=2)) %>%
mutate(`Mixed Int`=na.approx(Mixed,na.rm = TRUE,rule=2)) %>%  
mutate(`G379B Int`=na.approx(G379B,na.rm = TRUE,rule=2)) %>%
mutate(`G379D Int`=na.approx(G379D,na.rm = TRUE,rule=2)) %>%    
mutate(`Load Chara`=`Outflow (cfs)`*`Chara Int`*28.316847*60*60*24/1000000) %>% #convert to P load kg. 28.31 L/cf *60 sec * 1g/1000mg * 1kg/1000g
mutate(`Load Bare`=`Outflow (cfs)`*`Bare Int`*28.316847*60*60*24/1000000) %>% #convert to P load kg. 28.31 L/cf *60 sec * 1g/1000mg * 1kg/1000g
mutate(`Load Typha`=`Outflow (cfs)`*`Typha Int`*28.316847*60*60*24/1000000) %>% #convert to P load kg. 28.31 L/cf *60 sec * 1g/1000mg * 1kg/1000g
mutate(`Load Naiad`=`Outflow (cfs)`*`Naiad Int`*28.316847*60*60*24/1000000) %>% #convert to P load kg. 28.31 L/cf *60 sec * 1g/1000mg * 1kg/1000g
mutate(`Load Mixed`=`Outflow (cfs)`*`Mixed Int`*28.316847*60*60*24/1000000) %>% #convert to P load kg. 28.31 L/cf *60 sec * 1g/1000mg * 1kg/1000g
mutate(`Load G379B`=`Outflow (cfs)`*`G379B Int`*28.316847*60*60*24/1000000) %>% #convert to P load kg. 28.31 L/cf *60 sec * 1g/1000mg * 1kg/1000g
mutate(`Load G379D`=`Outflow (cfs)`*`G379D Int`*28.316847*60*60*24/1000000) %>% #convert to P load kg. 28.31 L/cf *60 sec * 1g/1000mg * 1kg/1000g
mutate(`Cumulative TP Chara`=cumsum(`Load Chara`)) %>%
mutate(`Cumulative TP Bare`=cumsum(`Load Bare`)) %>%  
mutate(`Cumulative TP Typha`=cumsum(`Load Typha`)) %>%
mutate(`Cumulative TP Naiad`=cumsum(`Load Naiad`)) %>% 
mutate(`Cumulative TP Mixed`=cumsum(`Load Mixed`)) %>%
mutate(`Cumulative TP G379B`=cumsum(`Load G379B`)) %>% 
mutate(`Cumulative TP G379D`=cumsum(`Load G379D`)) %>%  
mutate(`FWM Chara`=`Cumulative TP Chara`/(`Cumulative Outflow`*28.316847*60*60*24)*1000000)  %>%  #convert to FWM TP mg/L
mutate(`FWM Bare`=`Cumulative TP Bare`/(`Cumulative Outflow`*28.316847*60*60*24)*1000000)  %>%
mutate(`FWM Typha`=`Cumulative TP Typha`/(`Cumulative Outflow`*28.316847*60*60*24)*1000000) %>%
mutate(`FWM Naiad`=`Cumulative TP Naiad`/(`Cumulative Outflow`*28.316847*60*60*24)*1000000) %>%
mutate(`FWM Mixed`=`Cumulative TP Mixed`/(`Cumulative Outflow`*28.316847*60*60*24)*1000000) %>%
mutate(`FWM G379B`=`Cumulative TP G379B`/(`Cumulative Outflow`*28.316847*60*60*24)*1000000) %>%
mutate(`FWM G379D`=`Cumulative TP G379D`/(`Cumulative Outflow`*28.316847*60*60*24)*1000000) 


write.csv(TP_Budget_Daily ,"./Data/P Budget/TP_Budget_Daily.csv",row.names = FALSE)







