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


WQ_Upstream_Downstream_Tidy <- read_csv("./Data/WQ Data/WQ_Upstream_Downstream_Tidy.csv")
WQ_Data_Tidy <- read_csv("./Data/WQ Data/WQ_Data_Tidy.csv")
WQ_Data <- read_excel("Data/WQ Data/WQ Data.xlsx",sheet = "Sheet1")
WQ_Field_Data_Continuous_data <- read.csv("Data/Joined Data/WQ_Field_Data_Continuous_data.csv",check.names=FALSE)



# Correlation ----------------Need to enter physico-chemical parameters-----------------------------

#DF of differences in TP vs change in other analytes
TP_differences_vs_Analytes <- WQ_Upstream_Downstream_Tidy %>%
select(Date,Ecotope,Difference,TEST_NAME)  %>%
pivot_wider(names_from = TEST_NAME,values_from=`Difference`) 

#Calculate correlation all ecotopes grouped
TP_Correlation <- TP_differences_vs_Analytes %>%
select(-Date,-Ecotope) %>%  
select(sort(current_vars())) %>% #sorts column alphabetically
cor(method="spearman",use = "pairwise.complete.obs")

All_Correlation <- WQ_Field_Data_Continuous_data %>%
select(-`Date Time`,-Ecotope,-Position)  %>% 
select(sort(current_vars())) %>% #sorts column alphabetically
cor(method="spearman",use = "pairwise.complete.obs")


Differences_chemistry <- select(select(WQ_Field_Data_Continuous_data,32:52),-`Dif TN`,-`Dif OPO4`)
Differences_physico <- select(WQ_Field_Data_Continuous_data,53:59,`Dif TPO4`)


Values <-  select(WQ_Field_Data_Continuous_data,4:34,)

# Visualize ---------------------------------------------------------------

#Correlation plot
corrplot(TP_Correlation , type = "upper",  tl.col = "black", tl.srt = 45)
corrplot(All_Correlation , type = "upper",  tl.col = "black", tl.srt = 45)

#Correlation Plot with GGALLY 
ggpairs(select(TP_differences_vs_Analytes,-Date,-Ecotope,-PH,-DO,-COND,-TEMP), title="correlogram with ggpairs()") #removed physico-chemical parameters since that data hasn't been entered yet.

#just select parameters with significant correlation with TP 
ggpairs(select(TP_differences_vs_Analytes,COLOR,CL,TDPO4,TPO4,`Chlorophyll B`,`Chlorophyll A`,`Pheophytin A`,Hardness,CA,TOTFE), title="correlogram with ggpairs()")

#Correlation Plot with GGALLY 
ggpairs(select(WQ_Field_Data_Continuous_data,`Average DCS (Field Data)`,`DCS Levelogger`), title="correlogram with ggpairs()") #.

ggpairs(Differences_chemistry, title="correlogram with ggpairs()") #.

ggpairs(Differences_physico, title="correlogram with ggpairs()") #.

