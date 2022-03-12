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


WQ_Data <- read_excel("Data/WQ Data/WQ Data.xlsx",sheet = "Sheet1")


# Tidy Data ---------------------------------------------------------------

WQ_Data_Tidy <-WQ_Data  %>%
  filter(STATION!="OCS",SAMPLE_TYPE=="SAMP") %>%  
  mutate(Date=as.Date(COLLECT_DATE)) %>%
  mutate(Ecotope=case_when(str_detect(STATION,"STA34C2B_C")~"Chara",
                           str_detect(STATION,"STA34C2B_T")~"Typha",
                           str_detect(STATION,"STA34C2B_N")~"Naiad",
                           str_detect(STATION,"STA34C2B_M")~"Mixed",
                           str_detect(STATION,"STA34C2B_B")~"Bare"))  %>%
  mutate(Position=case_when(str_detect(STATION,"DWN")~"Downstream",
                            str_detect(STATION,"Up")~"Upstream"))

WQ_Upstream <- WQ_Data_Tidy  %>%
  filter(Position=="Upstream") %>%
  mutate(`Upstream Values`=VALUE)  %>%
  select(Date,Ecotope,TEST_NAME,`Upstream Values`)

WQ_DownStream <- WQ_Data_Tidy  %>%
  filter(Position=="Downstream") %>%
  mutate(`Downstream Values`=VALUE) %>%
  select(Date,Ecotope,TEST_NAME,`Downstream Values`) 

WQ_Upstream_Downstream_Tidy <- WQ_Upstream %>%
  left_join(WQ_DownStream,by=c("Date","Ecotope","TEST_NAME"))  %>%
  mutate(`Difference`=`Upstream Values`-`Downstream Values`)


# Save Data ---------------------------------------------------------------

write.csv(WQ_Data_Tidy , "./Data/WQ Data/WQ_Data_Tidy.csv",row.names = FALSE)

write.csv(WQ_Upstream_Downstream_Tidy, "./Data/WQ Data/WQ_Upstream_Downstream_Tidy.csv",row.names = FALSE)
