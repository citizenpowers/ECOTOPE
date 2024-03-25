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
#filter(`Date Time`>"2022-09-15 24:00:00" ) %>% #Year 2 
#filter(`Date Time`<"2023-07-02 24:00:00") %>%
filter(`Date Time`<"2022-06-01 24:00:00" | `Date Time`>"2022-09-15 24:00:00" ) %>% #Filter out days when there was no monitoring. for period of record calculations 
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
filter(`Date Time`<"2023-09-30 24:00:00") %>%  
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



# Calculate TP budget using daily averages ---(STA3/4 Study Year 1)-----------

TP_Budget_Daily_sta34_yr1 <-Water_budget %>%
  select(`Date Time`,`Outflow (cfs)`) %>%  
  left_join(TP_Tidy,by="Date Time") %>%
  left_join(G379_Tidy,by="Date Time") %>%
  arrange(`Date Time`) %>%
  filter(`Date Time`>"2021-06-01 00:00:00") %>%
  filter(`Date Time`<"2022-05-31 00:00:00") %>% #Filter out days when there was no monitoring
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
  mutate(`Running_FWM Chara`=`Cumulative TP Chara`/(`Cumulative Outflow`*28.316847*60*60*24)*1000000)  %>%  #convert to FWM TP mg/L
  mutate(`Running_FWM Bare`=`Cumulative TP Bare`/(`Cumulative Outflow`*28.316847*60*60*24)*1000000)  %>%
  mutate(`Running_FWM Typha`=`Cumulative TP Typha`/(`Cumulative Outflow`*28.316847*60*60*24)*1000000) %>%
  mutate(`Running_FWM Naiad`=`Cumulative TP Naiad`/(`Cumulative Outflow`*28.316847*60*60*24)*1000000) %>%
  mutate(`Running_FWM Mixed`=`Cumulative TP Mixed`/(`Cumulative Outflow`*28.316847*60*60*24)*1000000) %>%
  mutate(`Running_FWM G379B`=`Cumulative TP G379B`/(`Cumulative Outflow`*28.316847*60*60*24)*1000000) %>%
  mutate(`Running_FWM G379D`=`Cumulative TP G379D`/(`Cumulative Outflow`*28.316847*60*60*24)*1000000) %>%
  mutate(`Daily_FWM Chara`=if_else(`Outflow (cfs)`<0.01,0,`Load Chara`/(`Outflow (cfs)`*28.316847*60*60*24)*1000000))  %>%  #convert to FWM TP mg/L
  mutate(`Daily_FWM Bare`=if_else(`Outflow (cfs)`<0.01,0,`Load Bare`/(`Outflow (cfs)`*28.316847*60*60*24)*1000000))  %>%
  mutate(`Daily_FWM Typha`=if_else(`Outflow (cfs)`<0.01,0,`Load Typha`/(`Outflow (cfs)`*28.316847*60*60*24)*1000000)) %>%
  mutate(`Daily_FWM Mixed`=if_else(`Outflow (cfs)`<0.01,0,`Load Mixed`/(`Outflow (cfs)`*28.316847*60*60*24)*1000000)) %>%
  mutate(`Daily_FWM Naiad`=if_else(`Outflow (cfs)`<0.01,0,`Load Naiad`/(`Outflow (cfs)`*28.316847*60*60*24)*1000000)) %>%
  mutate(`Daily_FWM G379B`=if_else(`Outflow (cfs)`<0.01,0,`Load G379B`/(`Outflow (cfs)`*28.316847*60*60*24)*1000000)) %>%
  mutate(`Daily_FWM G379D`=if_else(`Outflow (cfs)`<0.01,0,`Load G379D`/(`Outflow (cfs)`*28.316847*60*60*24)*1000000)) 


write.csv(TP_Budget_Daily_sta34_yr1 ,"./Data/P Budget/TP_Budget_Daily_STA34_Study_Year_1.csv",row.names = FALSE)




# Calculate TP budget using daily averages ---(STA3/4 Study Year 2)-----------

TP_Budget_Daily_sta34_yr2 <-Water_budget %>%
  select(`Date Time`,`Outflow (cfs)`) %>%  
  left_join(TP_Tidy,by="Date Time") %>%
  left_join(G379_Tidy,by="Date Time") %>%
  arrange(`Date Time`) %>%
  filter(`Date Time`>"2022-09-20 00:00:00") %>%
  filter(`Date Time`<"2023-09-19 00:00:00") %>% #filter out overlapping dates
  mutate(Date=as.Date(`Date Time`)) %>%
  group_by(Date) %>%
  summarise(across(where(is.numeric),~ mean(.x, na.rm = TRUE))) %>%
  mutate(`Outflow (cfs)`=`Outflow (cfs)`*-1) %>% #discharged values are negative to reflect water leaving the cell. Converted to positive for TP budget 
  mutate(`Cumulative Outflow`=cumsum(`Outflow (cfs)`))  %>%
  mutate(`Chara Int`=na.approx(Chara,na.rm = TRUE,rule=2)) %>%
  mutate(`Bare Int`=na.approx(Bare,na.rm = TRUE,rule=2)) %>%
  mutate(`Typha Int`=na.approx(Typha,na.rm = TRUE,rule=2)) %>%
  mutate(`Mixed Int`=na.approx(Mixed,na.rm = TRUE,rule=2)) %>%  
  mutate(`G379B Int`=na.approx(G379B,na.rm = TRUE,rule=2)) %>%
  mutate(`G379D Int`=na.approx(G379D,na.rm = TRUE,rule=2)) %>%    
  mutate(`Load Chara`=`Outflow (cfs)`*`Chara Int`*28.316847*60*60*24/1000000) %>% #convert to P load kg. 28.31 L/cf *60 sec * 1g/1000mg * 1kg/1000g
  mutate(`Load Bare`=`Outflow (cfs)`*`Bare Int`*28.316847*60*60*24/1000000) %>% #convert to P load kg. 28.31 L/cf *60 sec * 1g/1000mg * 1kg/1000g
  mutate(`Load Typha`=`Outflow (cfs)`*`Typha Int`*28.316847*60*60*24/1000000) %>% #convert to P load kg. 28.31 L/cf *60 sec * 1g/1000mg * 1kg/1000g
  mutate(`Load Mixed`=`Outflow (cfs)`*`Mixed Int`*28.316847*60*60*24/1000000) %>% #convert to P load kg. 28.31 L/cf *60 sec * 1g/1000mg * 1kg/1000g
  mutate(`Load G379B`=`Outflow (cfs)`*`G379B Int`*28.316847*60*60*24/1000000) %>% #convert to P load kg. 28.31 L/cf *60 sec * 1g/1000mg * 1kg/1000g
  mutate(`Load G379D`=`Outflow (cfs)`*`G379D Int`*28.316847*60*60*24/1000000) %>% #convert to P load kg. 28.31 L/cf *60 sec * 1g/1000mg * 1kg/1000g
  mutate(`Cumulative TP Chara`=cumsum(`Load Chara`)) %>%
  mutate(`Cumulative TP Bare`=cumsum(`Load Bare`)) %>%  
  mutate(`Cumulative TP Typha`=cumsum(`Load Typha`)) %>%
  mutate(`Cumulative TP Mixed`=cumsum(`Load Mixed`)) %>%
  mutate(`Cumulative TP G379B`=cumsum(`Load G379B`)) %>% 
  mutate(`Cumulative TP G379D`=cumsum(`Load G379D`)) %>%  
  mutate(`Running_FWM Chara`=`Cumulative TP Chara`/(`Cumulative Outflow`*28.316847*60*60*24)*1000000)  %>%  #convert to FWM TP mg/L
  mutate(`Running_FWM Bare`=`Cumulative TP Bare`/(`Cumulative Outflow`*28.316847*60*60*24)*1000000)  %>%
  mutate(`Running_FWM Typha`=`Cumulative TP Typha`/(`Cumulative Outflow`*28.316847*60*60*24)*1000000) %>%
  mutate(`Running_FWM Mixed`=`Cumulative TP Mixed`/(`Cumulative Outflow`*28.316847*60*60*24)*1000000) %>%
  mutate(`Running_FWM G379B`=`Cumulative TP G379B`/(`Cumulative Outflow`*28.316847*60*60*24)*1000000) %>%
  mutate(`Running_FWM G379D`=`Cumulative TP G379D`/(`Cumulative Outflow`*28.316847*60*60*24)*1000000) %>%
  mutate(`Daily_FWM Chara`=if_else(`Outflow (cfs)`<0.01,0,`Load Chara`/(`Outflow (cfs)`*28.316847*60*60*24)*1000000))  %>%  #convert to FWM TP mg/L
  mutate(`Daily_FWM Bare`=if_else(`Outflow (cfs)`<0.01,0,`Load Bare`/(`Outflow (cfs)`*28.316847*60*60*24)*1000000))  %>%
  mutate(`Daily_FWM Typha`=if_else(`Outflow (cfs)`<0.01,0,`Load Typha`/(`Outflow (cfs)`*28.316847*60*60*24)*1000000)) %>%
  mutate(`Daily_FWM Mixed`=if_else(`Outflow (cfs)`<0.01,0,`Load Mixed`/(`Outflow (cfs)`*28.316847*60*60*24)*1000000)) %>%
  mutate(`Daily_FWM G379B`=if_else(`Outflow (cfs)`<0.01,0,`Load G379B`/(`Outflow (cfs)`*28.316847*60*60*24)*1000000)) %>%
  mutate(`Daily_FWM G379D`=if_else(`Outflow (cfs)`<0.01,0,`Load G379D`/(`Outflow (cfs)`*28.316847*60*60*24)*1000000)) 



write.csv(TP_Budget_Daily_sta34_yr2 ,"./Data/P Budget/TP_Budget_Daily_STA34_Study_Year_2.csv",row.names = FALSE)


TP_Budget_Daily_Combined <-mutate(TP_Budget_Daily_sta34_yr1,`Study Period`="Year 1: STA-3/4") %>%
bind_rows(mutate(TP_Budget_Daily_sta34_yr2,`Study Period`="Year 2: STA-3/4")) %>%
mutate(`Figure Label Date`= make_date(year=2000,month=month(Date),day=day(Date))) %>%
select(1:9,18:24,32:47) %>%  
#select(1:6,18:20,22,32:34,36,39:42,45:46) %>%
rename(`Measured Chara`="Chara",`Measured Mixed`="Mixed",`Measured Typha`="Typha",`Measured Bare`="Bare",`Measured G379B`="G379B",`Measured G379D`="G379D",`Measured Naiad`="Naiad") %>%
pivot_longer(names_to = c(".value","Ecotope"),3:30,names_sep="\\ ") %>%
mutate(Ecotope=factor(Ecotope,labels = c("Bare","italic(Chara)","Mixed","italic(Typha)","Naiad","G379B","G379D"),levels=c("Bare","Chara","Mixed","Typha","Naiad","G379B","G379D")))

write.csv(TP_Budget_Daily_Combined ,"./Data/P Budget/TP_Budget_Daily_Combined.csv",row.names = FALSE)

FWM_Weekly <-mutate(TP_Budget_34_yr1,`Study Period`="Year 1: STA-3/4") %>%
bind_rows(mutate(TP_Budget_34_yr2,`Study Period`="Year 2: STA-3/4")) %>%
mutate(`Figure Label Date`= make_date(year=2000,month=month(Date),day=day(Date)),Week=week(Date)) %>%
group_by(`Study Period`,Week) %>%
select(1:2,11:13,15,39:41) %>%
mutate(`Load Chara`=`Outflow (cfs)`*`Chara Int`*28.316847*60*60*24/1000000) %>% #convert to P load kg. 28.31 L/cf *60 sec * 1g/1000mg * 1kg/1000g
mutate(`Load Bare`=`Outflow (cfs)`*`Bare Int`*28.316847*60*60*24/1000000) %>% #convert to P load kg. 28.31 L/cf *60 sec * 1g/1000mg * 1kg/1000g
mutate(`Load Typha`=`Outflow (cfs)`*`Typha Int`*28.316847*60*60*24/1000000) %>% #convert to P load kg. 28.31 L/cf *60 sec * 1g/1000mg * 1kg/1000g
mutate(`Load Mixed`=`Outflow (cfs)`*`Mixed Int`*28.316847*60*60*24/1000000) %>% #convert to P load kg. 28.31 L/cf *60 sec * 1g/1000mg * 1kg/1000g
summarise(n(),`Cumulative Outflow`=sum(`Outflow (cfs)`,na.rm=T),
          `FWM Chara`=if_else(`Cumulative Outflow`<0.01,0,sum(`Load Chara`,na.rm=T)/sum(`Outflow (cfs)`,na.rm=T)),
          `FWM Bare`=if_else(`Cumulative Outflow`<0.01,0,sum(`Load Bare`,na.rm=T)/sum(`Outflow (cfs)`,na.rm=T)),
          `FWM Typha`=if_else(`Cumulative Outflow`<0.01,0,sum(`Load Typha`,na.rm=T)/sum(`Outflow (cfs)`,na.rm=T)),
          `FWM Mixed`=if_else(`Cumulative Outflow`<0.01,0,sum(`Load Mixed`,na.rm=T)/sum(`Outflow (cfs)`,na.rm=T))) %>%
pivot_longer(names_to = "Ecotope",values_to = "FWM-TP",5:8) %>%
mutate(Ecotope=factor(Ecotope,labels = c("Bare","italic(Chara)","Mixed","italic(Typha)")))

write.csv(FWM_Weekly,"./Data/P Budget/FWM_Weekly.csv",row.names = FALSE)

FWM_Daily <-mutate(TP_Budget_34_yr1,`Study Period`="Year 1: STA-3/4") %>%
bind_rows(mutate(TP_Budget_34_yr2,`Study Period`="Year 2: STA-3/4")) %>%
mutate(`Figure Label Date`= make_date(year=2000,month=month(Date),day=day(Date))) %>%
group_by(`Study Period`,`Figure Label Date`) %>%
select(1:2,11:13,15,39:40) %>%
mutate(`Load Chara`=`Outflow (cfs)`*`Chara Int`*28.316847*60*60*24/1000000) %>% #convert to P load kg. 28.31 L/cf *60 sec * 1g/1000mg * 1kg/1000g
mutate(`Load Bare`=`Outflow (cfs)`*`Bare Int`*28.316847*60*60*24/1000000) %>% #convert to P load kg. 28.31 L/cf *60 sec * 1g/1000mg * 1kg/1000g
mutate(`Load Typha`=`Outflow (cfs)`*`Typha Int`*28.316847*60*60*24/1000000) %>% #convert to P load kg. 28.31 L/cf *60 sec * 1g/1000mg * 1kg/1000g
mutate(`Load Mixed`=`Outflow (cfs)`*`Mixed Int`*28.316847*60*60*24/1000000) %>% #convert to P load kg. 28.31 L/cf *60 sec * 1g/1000mg * 1kg/1000g
summarise(n(),`Cumulative Outflow`=sum(`Outflow (cfs)`,na.rm=T)*28.316847*60*60*24/1000000,
            `FWM Chara`=if_else(`Cumulative Outflow`<0.01,0,sum(`Load Chara`,na.rm=T)/`Cumulative Outflow`),
            `FWM Bare`=if_else(`Cumulative Outflow`<0.01,0,sum(`Load Bare`,na.rm=T)/`Cumulative Outflow`),
            `FWM Typha`=if_else(`Cumulative Outflow`<0.01,0,sum(`Load Typha`,na.rm=T)/`Cumulative Outflow`),
            `FWM Mixed`=if_else(`Cumulative Outflow`<0.01,0,sum(`Load Mixed`,na.rm=T)/`Cumulative Outflow`)) %>%
pivot_longer(names_to = "Ecotope",values_to = "FWM-TP",5:8) %>%
mutate(Ecotope=factor(Ecotope,labels = c("Bare","italic(Chara)","Mixed","italic(Typha)")))

write.csv(FWM_Weekly,"./Data/P Budget/FWM_Weekly.csv",row.names = FALSE)



