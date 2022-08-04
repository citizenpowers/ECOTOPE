
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


# Import Data -------------------------------------------------------------


WQ_Data <- read_excel("Data/WQ Data/WQ Data.xlsx",sheet = "Sheet1")


# Tidy Data ---------------------------------------------------------------


Soils_data <- WQ_Data %>%
filter(MATRIX %in% c("FLOC","SO")) %>%
mutate(Date=as.Date(COLLECT_DATE)) %>%
mutate(Ecotope=case_when(str_detect(STATION,"STA34C2B_C")~"Chara",
                         str_detect(STATION,"STA34C2B_T")~"Typha",
                         str_detect(STATION,"STA34C2B_N")~"Naiad",
                         str_detect(STATION,"STA34C2B_M")~"Mixed",
                         str_detect(STATION,"STA34C2B_B")~"Bare")) 
# Save Data ---------------------------------------------------------------

write.csv(Soils_data , "./Data/Soils Data/Soils_Data_Tidy.csv",row.names = FALSE)



Soils_boxplot <-ggplot(Soils_data,aes(y=VALUE,color=Ecotope,fill=Ecotope))+
geom_boxplot(alpha=.5)+facet_wrap(~TEST_NAME,scale="free")

ggthemr("flat dark",type="outer", layout="scientific")
TPO4_boxplot
