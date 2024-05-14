rm(list = ls())

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
library(ggthemr)
library(suntools)

# Import Data -------------------------------------------------------------



WQ_Field_with_continuous_same_rows <- read.csv("./Data/Joined Data/WQ_Field_with_continuous_same_rows.csv",check.names=FALSE)

# Theme -------------------------------------------------------------------

ggthemr("flat dark",type="outer", layout="scientific")
ggthemr("light",type="outer", layout="scientific")  #used for SFER figs
Presentation_theme <- theme(strip.text = element_text(size=20) ,legend.position="bottom",axis.text=element_text(size=16),axis.title = element_text(size = 20),legend.text = element_text(size = 24),legend.title = element_text(size = 20))
Presentation_theme2 <- theme( strip.text = element_text(size=20) ,legend.position="bottom",axis.text=element_text(size=14),axis.title = element_text(size = 16),legend.text = element_text(size = 20),legend.title = element_text(size = 20))




# Tidy Data ---------------------------------------------------------------

Correlation_Data_Tidy <- WQ_Field_with_continuous_same_rows %>% 
filter(Position=="Downstream",Ecotope!="Naiad")  

Correlation_Data_Tidy_sta34 <- WQ_Field_with_continuous_same_rows %>% 
filter(Position=="Downstream",Ecotope!="Naiad",STA=="STA-3/4 Cell 2B")  

Correlation_Data_Tidy_sta1W <- WQ_Field_with_continuous_same_rows %>% 
filter(Position=="Downstream",Ecotope!="Naiad",STA=="STA-1W Cell 5B")  

#Calculate correlation using daylength as proxy for season
Sunrise <-sunriset(matrix(c(-80.6676, 26.6845), nrow=1),seq(from=as.POSIXct("2021-05-31"), to=as.POSIXct("2023-10-01") , by="days"), direction="sunrise", POSIXct.out=TRUE)%>%
mutate(Date=as.Date(time)) %>% rename(Sunrise="day_frac",Sunrise_time="time") 

Sunset  <-sunriset(matrix(c(-80.6676, 26.6845), nrow=1),seq(from=as.POSIXct("2021-05-31"), to=as.POSIXct("2023-10-01") , by="days"), direction="sunset", POSIXct.out=TRUE) %>%
mutate(Date=as.Date(time)) %>% rename(Sunset="day_frac",Sunset_time="time") 

Season_Flow_Water_Depth <-Correlation_Data_Tidy_sta1W %>%    
mutate(Date=as.Date(Date)) %>%
left_join(Sunrise,by="Date") %>%
left_join(Sunset,by="Date") %>%
mutate(`Daylight Fraction`=Sunset-Sunrise) %>%
select(Ecotope,TPO4,`DCS (Field Data)`,`Mean outflow (cfs)`,`Daylight Fraction`) 

# Correlation ---------(All STAs)----------------------------

#Calculate DF of TPO4 and Physical Parameters
TP_Physical_Correlation <- Correlation_Data_Tidy %>%
select(-`Date`,Ecotope,-Position,-Hour,-Minute) %>%  
select(TPO4,Ecotope,28:29,64:70,86:87)  %>%  #Select Physical parameters
select(sort(current_vars())) 

#Calculate DF of TPO4 and Sonde Parameters
TP_Continuous_Correlation <- Correlation_Data_Tidy %>%
select(TPO4,Ecotope,75:89)  %>%  #Select Physical parameters
select(-`BGA-PC RFU`,-`BGA-PC ug/L`,-`Chl ug/L`) %>%  
select(sort(current_vars())) 

#Calculate DF of TPO4 and analytes
TP_Analyte_Correlation_1 <- Correlation_Data_Tidy %>%
select(Ecotope,10:20)  %>%  #Select Analyte 
select(sort(current_vars())) 

TP_Analyte_Correlation_2 <- Correlation_Data_Tidy %>%
select(TPO4,Ecotope,21:30)  %>%  #Select Analyte 
select(sort(current_vars()))

#Correlation Plot with GGALLY of physical parameters
ggpairs(TP_Physical_Correlation,ggplot2::aes(colour=Ecotope),method = "spearman", title = "Correlation: Physical Parameters and TP") #TP04 Correlation with Physico-chemical parameters

ggsave(plot = last_plot(),filename="./Figures/Correlogram- Physical Parameters.jpeg",width =13.333, height =7.5, units = "in")

#Correlation Plot with GGALLY of sonde parameters
ggpairs(TP_Continuous_Correlation,ggplot2::aes(colour=Ecotope),method = "spearman", title = "Correlation: Continuous Parameters and TP") #TP04 Correlation with Physico-chemical parameters

ggsave(plot = last_plot(),filename="./Figures/Correlogram- Continuous Parameters.jpeg",width =16, height =9, units = "in")

#Correlation Plot with GGALLY of analyte 
ggpairs(TP_Analyte_Correlation_1,ggplot2::aes(colour=Ecotope),method = "spearman", title = "Correlation: Water Quality Analytes") #TP04 Correlation with Physico-chemical parameters

ggsave(plot = last_plot(),filename="./Figures/Correlogram- WQ Analytes 1.jpeg",width =16, height =9, units = "in")

#Correlation Plot with GGALLY of analyte 
ggpairs(TP_Analyte_Correlation_2,ggplot2::aes(colour=Ecotope),method = "spearman", title = "Correlation: Water Quality Analytes") #TP04 Correlation with Physico-chemical parameters

ggsave(plot = last_plot(),filename="./Figures/Correlogram- WQ Analytes 2.jpeg",width =16, height =9, units = "in")


# Correlation (STA34 Only) --------------------------------------------------


#Calculate DF of TPO4 and Physical Parameters
TP_Physical_Correlation_sta1W <- Correlation_Data_Tidy_sta1W %>%
  select(-`Date`,Ecotope,-Position,-Hour,-Minute) %>%  
  select(TPO4,Ecotope,28:29,64:70,86:87)  %>%  #Select Physical parameters
  select(sort(current_vars())) 

#Calculate DF of TPO4 and Sonde Parameters
TP_Continuous_Correlation_sta1W <- Correlation_Data_Tidy_sta1W %>%
  select(TPO4,Ecotope,75:89)  %>%  #Select sonde parameters
  select(-`BGA-PC RFU`,-`BGA-PC ug/L`,-`Chl ug/L`) %>%  
  select(sort(current_vars())) 

#Calculate DF of TPO4 and analytes
TP_Analyte_Correlation_sta1W_1 <- Correlation_Data_Tidy_sta1W %>%
  select(Ecotope,10:20)  %>%  #Select Analytes 
  select(sort(current_vars())) 

TP_Analyte_Correlation_sta1W_2 <- Correlation_Data_Tidy_sta1W %>%
  select(TPO4,Ecotope,21:30)  %>%  #Select Analytes 
  select(sort(current_vars()))

#Correlation Plot with GGALLY of analyte 
ggpairs(TP_Analyte_Correlation_sta1W_1,ggplot2::aes(colour=Ecotope),method = "spearman", title = "Correlation: Water Quality Analytes") #TP04 Correlation with Physico-chemical parameters

ggsave(plot = last_plot(),filename="./Figures/Correlogram _sta1W- WQ Analytes 1.jpeg",width =16, height =9, units = "in")

#Correlation Plot with GGALLY of analyte 
ggpairs(TP_Analyte_Correlation_sta1W_2,ggplot2::aes(colour=Ecotope),method = "spearman", title = "Correlation: Water Quality Analytes") #TP04 Correlation with Physico-chemical parameters

ggsave(plot = last_plot(),filename="./Figures/Correlogram _sta1W- WQ Analytes 2.jpeg",width =16, height =9, units = "in")



# Correlation (STA1W only) ------------------------------------------------
#Calculate DF of TPO4 and Physical Parameters
TP_Physical_Correlation_sta34 <- Correlation_Data_Tidy_sta34 %>%
  select(-`Date`,Ecotope,-Position,-Hour,-Minute) %>%  
  select(TPO4,Ecotope,28:29,64:70,86:87)  %>%  #Select Physical parameters
  select(sort(current_vars())) 

#Calculate DF of TPO4 and Sonde Parameters
TP_Continuous_Correlation_sta34 <- Correlation_Data_Tidy_sta34 %>%
  select(TPO4,Ecotope,75:89)  %>%  #Select sonde parameters
  select(-`BGA-PC RFU`,-`BGA-PC ug/L`,-`Chl ug/L`) %>%  
  select(sort(current_vars())) 

#Calculate DF of TPO4 and analytes
TP_Analyte_Correlation_sta34_1 <- Correlation_Data_Tidy_sta34 %>%
  select(Ecotope,10:20)  %>%  #Select Analytes 
  select(sort(current_vars())) 

TP_Analyte_Correlation_sta34_2 <- Correlation_Data_Tidy_sta34 %>%
  select(TPO4,Ecotope,21:30)  %>%  #Select Analytes 
  select(sort(current_vars()))

#Correlation Plot with GGALLY of analyte 
ggpairs(TP_Analyte_Correlation_sta34_1,ggplot2::aes(colour=Ecotope),method = "spearman", title = "Correlation: Water Quality Analytes") #TP04 Correlation with Physico-chemical parameters

ggsave(plot = last_plot(),filename="./Figures/Correlogram STA34- WQ Analytes 1.jpeg",width =16, height =9, units = "in")

#Correlation Plot with GGALLY of analyte 
ggpairs(TP_Analyte_Correlation_sta34_2,ggplot2::aes(colour=Ecotope),method = "spearman", title = "Correlation: Water Quality Analytes") #TP04 Correlation with Physico-chemical parameters

ggsave(plot = last_plot(),filename="./Figures/Correlogram STA34- WQ Analytes 2.jpeg",width =16, height =9, units = "in")





# Season Flow and Water Depth Correlation ---------------------------------

#Correlation Plot with GGALLY of analyte 
ggpairs(Season_Flow_Water_Depth,ggplot2::aes(colour=Ecotope),method = "spearman", title = "Correlation: Water Quality Analytes") #TP04 Correlation with Physico-chemical parameters

ggsave(plot = last_plot(),filename="./Figures/Correlogram_STA_1W_Flow_Season_Depth.jpeg",width =16, height =9, units = "in")

# Visualize ---------------------------------------------------------------


#Correlation Plot with GGALLY 
ggpairs(select(TP_differences_vs_Analytes,-Date,-Ecotope,-PH,-DO,-COND,-TEMP), title="correlogram with ggpairs()") #removed physico-chemical parameters since that data hasn't been entered yet.

#just select parameters with significant correlation with TP 
ggpairs(select(TP_differences_vs_Analytes,COLOR,CL,TDPO4,TPO4,`Chlorophyll B`,`Chlorophyll A`,`Pheophytin A`,Hardness,CA,TOTFE), title="correlogram with ggpairs()")

#Correlation Plot with GGALLY 
ggpairs(select(WQ_Field_Data_Continuous_data,`Average DCS (Field Data)`,`DCS Levelogger`), title="correlogram with ggpairs()") #.

ggpairs(Differences_chemistry, title="correlogram with ggpairs()") #.

ggpairs(Differences_physico, title="correlogram with ggpairs()") #.


# Test code ---------------------------------------------------------------

#Calculate correlation of all ecotopes grouped together
TP_Correlation <- Correlation_Data_Tidy %>%
  select(-`Date`,-Ecotope,-Position,-Hour,-Minute) %>%  
  select(1:28,57:76)  %>%  #remove upstream downstream differences column
  select(-OPO4,-`TSS mg/L`,-`BGA-PC RFU`,-`BGA-PC ug/L`,-`Chl ug/L`) %>%  #remove parameters that are missing data or has unchanging data
  select(sort(current_vars())) %>% #sorts column alphabetically
  cor(method="spearman",use = "pairwise.complete.obs")

#Calculate correlation of all ecotopes grouped together
TP_Correlation <- Correlation_Data_Tidy %>%
  select(-`Date`,-Ecotope,-Position,-Hour,-Minute) %>%  
  select(1:28,57:76)  %>%  #remove upstream downstream differences column
  select(-OPO4,-`TSS mg/L`,-`BGA-PC RFU`,-`BGA-PC ug/L`,-`Chl ug/L`) %>%  #remove parameters that are missing data or has unchanging data
  select(sort(current_vars())) %>% #sorts column alphabetically
  cor(method="spearman",use = "pairwise.complete.obs")



All_Correlation <- WQ_Field_with_continuous_same_rows %>%
  select(-`Date Time`,-Ecotope,-Position,-Hour,-Minute)  %>% 
  select(sort(current_vars())) %>% #sorts column alphabetically
  
  cor(method="spearman",use = "pairwise.complete.obs")


Differences_chemistry <- select(select(WQ_Field_Data_Continuous_data,32:52),-`Dif TN`,-`Dif OPO4`)
Differences_physico <- select(WQ_Field_Data_Continuous_data,53:59,`Dif TPO4`)


Values <-  select(WQ_Field_Data_Continuous_data,4:34,)

#DF of differences in TP vs change in other analytes
TP_differences_vs_Analytes <- WQ_Data_Tidy %>%
  select(Date,Ecotope,Difference,TEST_NAME)  %>%
  pivot_wider(names_from = TEST_NAME,values_from=`Difference`) 


