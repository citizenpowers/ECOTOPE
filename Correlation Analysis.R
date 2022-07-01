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

# Import Data -------------------------------------------------------------



WQ_Field_with_continuous_same_rows <- read.csv("./Data/Joined Data/WQ_Field_with_continuous_same_rows.csv",check.names=FALSE)

# Theme -------------------------------------------------------------------

ggthemr("flat dark",type="outer", layout="scientific")

# Tidy Data ---------------------------------------------------------------

Correlation_Data_Tidy <- WQ_Field_with_continuous_same_rows %>% 
filter(Position=="Downstream")  
#filter(TPO4<30) #remove right handed outliers


# Correlation ---------------------------------------------


#Calculate DF of TPO4 and Physical Parameters
TP_Physical_Correlation <- Correlation_Data_Tidy %>%
select(-`Date`,Ecotope,-Position,-Hour,-Minute) %>%  
select(TPO4,Ecotope,27:28,76:79,-`fDOM QSUL`)  %>%  #Select Physical parameters
select(sort(current_vars())) 

#Calculate DF of TPO4 and Sonde Parameters
TP_Continuous_Correlation <- Correlation_Data_Tidy %>%
select(-`Date`,Ecotope,-Position,-Hour,-Minute) %>%  
select(TPO4,Ecotope,58:68)  %>%  #Select Physical parameters
select(sort(current_vars())) 

#Calculate DF of TPO4 and analytes
TP_Analyte_Correlation_1 <- Correlation_Data_Tidy %>%
select(-`Date`,Ecotope,-Position,-Hour,-Minute) %>%  
select(TPO4,Ecotope,2:12)  %>%  #Select Analyte 
select(sort(current_vars())) 

TP_Analyte_Correlation_2 <- Correlation_Data_Tidy %>%
select(-`Date`,Ecotope,-Position,-Hour,-Minute) %>%  
select(TPO4,Ecotope,13:22)  %>%  #Select Analyte 
select(sort(current_vars()))


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


# Visualize ---------------------------------------------------------------

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

#Correlation Plot with GGALLY 
ggpairs(select(TP_differences_vs_Analytes,-Date,-Ecotope,-PH,-DO,-COND,-TEMP), title="correlogram with ggpairs()") #removed physico-chemical parameters since that data hasn't been entered yet.

#just select parameters with significant correlation with TP 
ggpairs(select(TP_differences_vs_Analytes,COLOR,CL,TDPO4,TPO4,`Chlorophyll B`,`Chlorophyll A`,`Pheophytin A`,Hardness,CA,TOTFE), title="correlogram with ggpairs()")

#Correlation Plot with GGALLY 
ggpairs(select(WQ_Field_Data_Continuous_data,`Average DCS (Field Data)`,`DCS Levelogger`), title="correlogram with ggpairs()") #.

ggpairs(Differences_chemistry, title="correlogram with ggpairs()") #.

ggpairs(Differences_physico, title="correlogram with ggpairs()") #.




