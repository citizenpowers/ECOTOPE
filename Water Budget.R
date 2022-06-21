#Water Budget
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

Flow_Data_1_min <- read_csv("./Data/Flow Data/Flow_Data_1_min.csv")
s7_ET_BK <- get_hydro(dbkey = "IY948", date_min="2021-06-01",date_max=as.character(today()))  #DBHYDRO data for inflow to Cell 2B of STA34
s7_R_BK <- get_hydro(dbkey = "RQ465", date_min="2021-06-01",date_max=as.character(today()))  #DBHYDRO data for inflow to Cell 2B of STA34



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
mutate(`Cumulative ET (inches)`=-`Cumulative ET (inches)`*2843/12/24/60) %>%   #convert inches to acre-ft. Need accurate measurement of 2B size. 2843 includes PSTA cells. ET data is given as daily average
mutate(`Cumulative Rain (inches)`=cumsum(ifelse(is.na(S7_R_RAIN_Inches), 0, S7_R_RAIN_Inches)) + S7_R_RAIN_Inches*0 ) %>%
mutate(`Cumulative Rain (inches)`=`Cumulative Rain (inches)`*2843/12)  #convert inches to acre-ft. Need accurate measurement of 2B size. 2843 includes PSTA cells



# Save Data ---------------------------------------------------------------

write.csv(Water_budget,"./Data/Water Budget/Water Budget.csv",row.names = FALSE)

# Visualize ---------------------------------------------------------------

#Long DF to visualise Water budget by sources
Water_budget_long <- Water_budget %>%
gather("Source","Contribution Volume (ac-ft)",`Cumulative Inflow (acre-ft)`,`Cumulative Outflow (acre-ft)`,`Cumulative ET (inches)`,`Cumulative Rain (inches)`) %>%
mutate(Source= factor(Source,levels=c("Cumulative Inflow (acre-ft)","Cumulative Outflow (acre-ft)","Cumulative ET (inches)","Cumulative Rain (inches)")))   

#Water by source
ggplot(Water_budget_long,aes(`Date Time`,`Contribution Volume (ac-ft)`,fill=Source,color=Source))+geom_col( position = "stack")+theme_bw()+
geom_line(aes(`Date Time`,sum(`Contribution Volume (ac-ft)`)),color="red")+
scale_fill_viridis( discrete = TRUE,option="D")+scale_color_viridis( discrete = TRUE,option="D")+theme(axis.text.x=element_text(angle=90,hjust=1))+
scale_y_continuous(breaks=pretty_breaks(n=10),label=comma)+
#scale_x_date(limits =as.Date(c("2021-06-01","2022-05-01")),date_breaks = "3 months", date_labels = "%b %y")+
labs(y="Cummulative Water Budget (acre-ft)",x="Date")+theme(legend.position = "bottom")
