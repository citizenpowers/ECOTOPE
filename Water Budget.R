#Water Budget
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

#STA34 Data
Flow_Data_1_min <- read_csv("./Data/Flow Data/Flow_Data_1_min.csv")  #1-minute frequency for STA 3/4
s7_ET_BK <- get_hydro(dbkey = "IY948", date_min="2021-06-01",date_max="2023-10-27")  #DBHYDRO data for Evap at S7
s7_R_BK <- get_hydro(dbkey = "RQ465", date_min="2021-06-01",date_max="2023-10-27")  #DBHYDRO data for Rain at S7

#STA1W data
Flow_Data_1_min_1W <- read_csv("./Data/Flow Data/Flow_Data_1_min_STA_1W.csv")  #1-minute frequency for STA 1w
S5A_R_BK <- get_hydro(dbkey = "90611", date_min="2022-10-01",date_max="2023-10-01")  #DBHYDRO data for Rain at S5. 134 inches over POR. Quite high but within reasonable doubt.
Belle_GL_ETPI_DA <- get_hydro(dbkey = "TW080", date_min="2022-10-01",date_max="2023-10-01")  #DBHYDRO data for Evap at Bellegade weather station
Belle_GL__R_BK <- get_hydro(dbkey = "IW850", date_min="2022-10-01",date_max="2023-10-01")  #DBHYDRO data for Rain at Belleglade. 98 inches over POR, still high. 



# Create Water Budget -----------------------------------------------------

Water_budget <- Flow_Data_1_min %>%
arrange(`Date Time`) %>%
left_join(rename(s7_ET_BK,`Date Time`="date" ),by="Date Time") %>%
left_join(rename(s7_R_BK,`Date Time`="date" ),by="Date Time") %>%
fill(S7_R_RAIN_Inches,S7_E_EVAP_Inches) %>%  
mutate(`Outflow (cfs)`=`Outflow (cfs)`*-1) %>%  
mutate(`Cumulative Inflow (acre-ft)`=cumsum(`Inflow (cfs)`)*60/43560) %>% #cfs*60 to acre-ft 
mutate(`Cumulative Outflow (acre-ft)`=cumsum(`Outflow (cfs)`)*60/43560) %>% #cfs*60 to acre-ft 
mutate(`Cumulative ET (inches)`=cumsum(ifelse(is.na(S7_E_EVAP_Inches), 0, S7_E_EVAP_Inches)) + S7_E_EVAP_Inches*0 ) %>%
mutate(`Cumulative ET (acre-ft)`=-`Cumulative ET (inches)`*2843/12/24/60) %>%   #convert inches to acre-ft. Need accurate measurement of 2B size. 2843 includes PSTA cells. ET data is given as daily average
mutate(`Cumulative Rain (inches)`=cumsum(ifelse(is.na(S7_R_RAIN_Inches), 0, S7_R_RAIN_Inches)) + S7_R_RAIN_Inches*0 ) %>%
mutate(`Cumulative Rain (acre-ft)`=`Cumulative Rain (inches)`*2843/12) %>% #convert inches to acre-ft. Need accurate measurement of 2B size. 2843 includes PSTA cells
group_by(`Date Time`) %>%
distinct(`Date Time`,.keep_all = TRUE) #remove duplicate dates from daylight savings time fall back

ymd_hms(paste("2022-10-03","08:00:00"))

# Water Budget 1W ---------------------------------------------------------
Water_budget_1W <- Flow_Data_1_min_1W %>%
arrange(`Date Time`) %>%
left_join(select(mutate(Belle_GL_ETPI_DA,`Date Time`=ymd_hms(paste(`date`,"08:00:00"))),`Date Time`,`BELLE GL_ETPI_Inches`),by="Date Time")  %>%
left_join(rename(S5A_R_BK,`Date Time`="date" ),by="Date Time") %>%
#left_join(rename(Belle_GL__R_BK,`Date Time`="date" ,S5A_R_RAIN_Inches="BELLE GL_RAIN_Inches"),by="Date Time") %>%  If using the rain measurement made at Belle glade
fill(`S5A_R_RAIN_Inches`,`BELLE GL_ETPI_Inches`) %>%  
mutate(`Outflow (cfs)`=`Outflow (cfs)`*-1) %>%  
mutate(`Cumulative Inflow (acre-ft)`=cumsum(`Inflow (cfs)`)*60/43560) %>% #cfs*60 to acre-ft 
mutate(`Cumulative Outflow (acre-ft)`=cumsum(`Outflow (cfs)`)*60/43560) %>% #cfs*60 to acre-ft 
mutate(`Cumulative ET (inches)`=cumsum(ifelse(is.na(`BELLE GL_ETPI_Inches`), 0, `BELLE GL_ETPI_Inches`)) + `BELLE GL_ETPI_Inches`*0 ) %>%
mutate(`Cumulative ET (acre-ft)`=-`Cumulative ET (inches)`*2451/12/24/60) %>%   #convert inches to acre-ft. ET data is given as daily average
mutate(`Cumulative Rain (inches)`=cumsum(ifelse(is.na(`S5A_R_RAIN_Inches`), 0, `S5A_R_RAIN_Inches`)) + `S5A_R_RAIN_Inches`*0 ) %>%
mutate(`Cumulative Rain (acre-ft)`=`Cumulative Rain (inches)`*2541/12) %>% #convert inches to acre-ft.
group_by(`Date Time`) %>%
distinct(`Date Time`,.keep_all = TRUE) #remove duplicate dates from daylight savings time fall back


# Save Data ---------------------------------------------------------------

write.csv(Water_budget,"./Data/Water Budget/Water Budget.csv",row.names = FALSE)

write.csv(Water_budget_1W,"./Data/Water Budget/Water Budget 1W.csv",row.names = FALSE)

# Visualize ---------------------------------------------------------------

#Long DF to visualize Water budget by sources  (STA3/4)
Water_budget_long <- Water_budget %>%
mutate(`Storage`=`Cumulative Inflow (acre-ft)`+`Cumulative Outflow (acre-ft)`+`Cumulative ET (acre-ft)`+`Cumulative Rain (acre-ft)`)  %>%
gather("Source","Contribution Volume (ac-ft)",`Cumulative Inflow (acre-ft)`,`Cumulative Outflow (acre-ft)`,`Cumulative ET (acre-ft)`,`Cumulative Rain (acre-ft)`) %>%
mutate(Source= factor(Source,levels=c("Cumulative Inflow (acre-ft)","Cumulative Outflow (acre-ft)","Cumulative ET (acre-ft)","Cumulative Rain (acre-ft)")))   

#Water by source (STA3/4)
ggplot(Water_budget_long,aes(`Date Time`,`Contribution Volume (ac-ft)`,fill=Source,color=Source))+geom_col( position = "stack")+theme_bw()+
geom_line(aes(`Date Time`,Storage,color="red"))+
scale_fill_viridis( discrete = TRUE,option="D")+scale_color_viridis( discrete = TRUE,option="D")+theme(axis.text.x=element_text(angle=90,hjust=1))+
scale_y_continuous(breaks=pretty_breaks(n=10),label=comma)+
#scale_x_date(limits =as.Date(c("2021-06-01","2022-05-01")),date_breaks = "3 months", date_labels = "%b %y")+
labs(y="Cummulative Water Budget (acre-ft)",x="Date")+theme(legend.position = "bottom")

#Long DF to visualize Water budget by sources  (STA1W)
Water_budget_long_1W <- Water_budget_1W %>%
mutate(`Storage`=`Cumulative Inflow (acre-ft)`+`Cumulative Outflow (acre-ft)`+`Cumulative ET (acre-ft)`+`Cumulative Rain (acre-ft)`)  %>%
gather("Source","Contribution Volume (ac-ft)",`Cumulative Inflow (acre-ft)`,`Cumulative Outflow (acre-ft)`,`Cumulative ET (acre-ft)`,`Cumulative Rain (acre-ft)`) %>%
mutate(Source= factor(Source,levels=c("Cumulative Inflow (acre-ft)","Cumulative Outflow (acre-ft)","Cumulative ET (acre-ft)","Cumulative Rain (acre-ft)")))   

#Water by source  (STA1W)
ggplot(Water_budget_long_1W,aes(`Date Time`,`Contribution Volume (ac-ft)`,fill=Source,color=Source))+geom_col( position = "stack")+theme_bw()+
geom_line(aes(`Date Time`,Storage,color="red"))+
scale_fill_viridis( discrete = TRUE,option="D")+scale_color_viridis( discrete = TRUE,option="D")+theme(axis.text.x=element_text(angle=90,hjust=1))+
scale_y_continuous(breaks=pretty_breaks(n=10),label=comma)+
#scale_x_date(limits =as.Date(c("2021-06-01","2022-05-01")),date_breaks = "3 months", date_labels = "%b %y")+
labs(y="Cummulative Water Budget (acre-ft)",x="Date")+theme(legend.position = "bottom")

