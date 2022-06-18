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
  filter(STATION!="OCS",SAMPLE_TYPE=="SAMP",MATRIX=="SW") %>%  
  mutate(Date=as.Date(COLLECT_DATE)) %>%
  mutate(Ecotope=case_when(str_detect(STATION,"STA34C2B_C")~"Chara",
                           str_detect(STATION,"STA34C2B_T")~"Typha",
                           str_detect(STATION,"STA34C2B_N")~"Naiad",
                           str_detect(STATION,"STA34C2B_M")~"Mixed",
                           str_detect(STATION,"STA34C2B_B")~"Bare"))  %>%
  mutate(Position=case_when(str_detect(STATION,"DWN")~"Downstream",
                            str_detect(STATION,"Up")~"Upstream")) %>%
  mutate(TEST_NAME=if_else(TEST_NAME=="COND","SpCond",TEST_NAME)) %>%
  mutate(TEST_NAME=if_else(TEST_NAME=="TEMP","Temp",TEST_NAME)) %>%
  mutate(TEST_NAME=if_else(TEST_NAME=="PH","pH",TEST_NAME)) 



# Save Data ---------------------------------------------------------------

write.csv(WQ_Data_Tidy , "./Data/WQ Data/WQ_Data_Tidy.csv",row.names = FALSE)


