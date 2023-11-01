library(dplyr)
library(tidyr)
library(stringr)
library(lubridate)
library(scales)
library(readxl)



# Import Data -------------------------------------------------------------


WQ_Data <- read_excel("Data/WQ Data/WQ Data.xlsx",sheet = "Sheet1")

Ecotope_data_LIMSP <- read_excel("Data/WQ Data/Ecotope_data_LIMSP.xlsx")   #Import data from provisional LIMSP


# Tidy Data ---------------------------------------------------------------

WQ_Data_Tidy <-WQ_Data  %>%
filter(STATION!="OCS",SAMPLE_TYPE=="SAMP",MATRIX=="SW") %>%  
mutate(Date=as.Date(COLLECT_DATE)) %>%
filter(Date %in% c("2023-08-15", "2023-07-25", "2023-07-25") ==FALSE)  %>%   # Remove spatial sampling data from time series data  
filter(Date!="2023-07-25" & Date!="2023-07-26")  %>%   #spatial sampling days
mutate(Ecotope=case_when(str_detect(STATION,"STA34C2B_C")~"Chara",
                         str_detect(STATION,"STA34C2B_T")~"Typha",
                         str_detect(STATION,"STA34C2B_N")~"Naiad",
                         str_detect(STATION,"STA34C2B_M")~"Mixed",
                         str_detect(STATION,"STA34C2B_B")~"Bare",
                         str_detect(STATION,"STA1WC5B_C")~"Chara",
                         str_detect(STATION,"STA1WC5B_T")~"Typha",
                         str_detect(STATION,"STA1WC5B_M")~"Mixed",
                         str_detect(STATION,"STA1WC5B_B")~"Bare"))  %>%
mutate(Position=case_when(str_detect(STATION,"DWN")~"Downstream",
                          str_detect(STATION,"Up")~"Upstream")) %>%
mutate(TEST_NAME=if_else(TEST_NAME=="COND","SpCond",TEST_NAME)) %>%
mutate(TEST_NAME=if_else(TEST_NAME=="TEMP","Temp",TEST_NAME)) %>%
mutate(TEST_NAME=if_else(TEST_NAME=="PH","pH",TEST_NAME))  %>%
mutate(STA=case_when(str_detect(STATION,"STA34")~"STA-3/4 Cell 2B",str_detect(STATION,"STA1W")~"STA-1W Cell 5B"))


# Tidy provisional data
WQ_Provisional_Tidy <- Ecotope_data_LIMSP %>%
rename(STATION="LOCATION",TEST_NAME="ACODE",REMARK_CODE="ALL_QUALIFIERS",VALUE="RESULT_ONLY") %>%
filter(STATION!="OCS",SAMPLE_TYPE=="SAMP",MATRIX=="SW") %>%  
mutate(Date=as.Date(COLLECT_DATE)) %>%
mutate(Ecotope=case_when(str_detect(STATION,"STA34C2B_C")~"Chara",
                         str_detect(STATION,"STA1WC5B_C")~"Chara",
                         str_detect(STATION,"STA34C2B_T")~"Typha",
                         str_detect(STATION,"STA1WC5B_T")~"Typha",
                         str_detect(STATION,"STA34C2B_N")~"Naiad",
                         str_detect(STATION,"STA34C2B_M")~"Mixed",
                         str_detect(STATION,"STA1WC5B_M")~"Mixed",
                         str_detect(STATION,"STA34C2B_B")~"Bare",
                         str_detect(STATION,"STA1WC5B_B")~"Bare"))  %>%
mutate(Position=case_when(str_detect(STATION,"DWN")~"Downstream",
                          str_detect(STATION,"Up")~"Upstream")) %>%
mutate(STA=case_when(str_detect(STATION,"STA34")~"STA-3/4 Cell 2B",
                            str_detect(STATION,"STA1W")~"STA-1W Cell 5B")) %>%
mutate(TEST_NAME=if_else(TEST_NAME=="COND","SpCond",TEST_NAME)) %>%
mutate(TEST_NAME=if_else(TEST_NAME=="TEMP","Temp",TEST_NAME)) %>%
mutate(TEST_NAME=if_else(TEST_NAME=="PH","pH",TEST_NAME))   %>%
mutate(TEST_NAME=if_else(TEST_NAME=="NA","Sodium",TEST_NAME))   

#Tidy Spatial Data
Spatial_Data_Tidy <-WQ_Data  %>%
mutate(Date=as.Date(COLLECT_DATE)) %>%
filter(Date %in% c("2023-08-15", "2023-07-26", "2023-07-25"))  #Data from spatial sampling days only



# Save Data ---------------------------------------------------------------

write.csv(WQ_Data_Tidy , "./Data/WQ Data/WQ_Data_Tidy.csv",row.names = FALSE) 

write.csv(WQ_Provisional_Tidy , "./Data/WQ Data/WQ_Provisional_Tidy.csv",row.names = FALSE) 

write.csv(Spatial_Data_Tidy , "./Data/WQ Data/Spatial_Data_Tidy.csv",row.names = FALSE) 

