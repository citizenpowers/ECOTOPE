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

# Import Data -------------------------------------------------------------



WQ_Field_with_continuous_same_rows <- read.csv("./Data/Joined Data/WQ_Field_with_continuous_same_rows.csv",check.names=FALSE)

# Tidy Data ---------------------------------------------------------------

Correlation_Data_Tidy <- WQ_Field_with_continuous_same_rows %>% 
filter(TPO4<30) #remove right handed outliers



# Correlation ---------------------------------------------


#Calculate DF of TPO4 and Physical Parameters
TP_Physical_Correlation <- Correlation_Data_Tidy %>%
select(-`Date`,Ecotope,-Position,-Hour,-Minute) %>%  
select(TPO4,Ecotope,26:28,74:77)  %>%  #Select Physical parameters
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

#Correlation Plot with GGALLY of 
ggpairs(TP_Physical_Correlation,ggplot2::aes(colour=Ecotope), title="correlogram with ggpairs()") #TP04 Correlation with Physico-chemical parameters

#Correlation Plot with GGALLY 
ggpairs(select(TP_differences_vs_Analytes,-Date,-Ecotope,-PH,-DO,-COND,-TEMP), title="correlogram with ggpairs()") #removed physico-chemical parameters since that data hasn't been entered yet.

#just select parameters with significant correlation with TP 
ggpairs(select(TP_differences_vs_Analytes,COLOR,CL,TDPO4,TPO4,`Chlorophyll B`,`Chlorophyll A`,`Pheophytin A`,Hardness,CA,TOTFE), title="correlogram with ggpairs()")

#Correlation Plot with GGALLY 
ggpairs(select(WQ_Field_Data_Continuous_data,`Average DCS (Field Data)`,`DCS Levelogger`), title="correlogram with ggpairs()") #.

ggpairs(Differences_chemistry, title="correlogram with ggpairs()") #.

ggpairs(Differences_physico, title="correlogram with ggpairs()") #.




