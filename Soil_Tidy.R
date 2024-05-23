
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

Physical_Parameters <- read_excel("Data/Soils Data/Physical Parameters.xlsx")


# Tidy Data ---------------------------------------------------------------


#Find the moisture content of each sample. Needed to calculate the bulk density of sample.
Moisture_content <- Physical_Parameters %>%  
filter(TEST_NAME=="MOISTURE") %>%
select(SAMPLE_ID,STATION,MATRIX,VALUE) %>%
rename(`Moisture Content`="VALUE")

Soils_data <- Physical_Parameters %>%
left_join(Moisture_content,by=c("SAMPLE_ID","STATION","MATRIX")) %>%  #join moisture content
mutate(`Bulk Density`=ifelse(!is.na(`Bulk Density`),`Bulk Density`,`Wet Weight`*(1-`Moisture Content`/100)/(78.5*as.numeric(CORE_LENGTH)))) %>%
  mutate(Ecotope=case_when(str_detect(STATION,"STA34C2B_C")~"Chara",
                           str_detect(STATION,"STA34C2B_T")~"Typha",
                           str_detect(STATION,"STA34C2B_N")~"Naiad",
                           str_detect(STATION,"STA34C2B_M")~"Mixed",
                           str_detect(STATION,"STA34C2B_B")~"Bare",
                           str_detect(STATION,"STA1WC5B_C")~"Chara",
                           str_detect(STATION,"STA1WC5B_T")~"Typha",
                           str_detect(STATION,"STA1WC5B_M")~"Mixed",
                           str_detect(STATION,"STA1WC5B_B")~"Bare"))  %>%
mutate(`Storage (g/m^2)`=ifelse(UNITS=="mg/Kg",`Bulk Density`*as.numeric(CORE_LENGTH)*VALUE/100,`Bulk Density`*as.numeric(CORE_LENGTH)*VALUE*10000))



# Save Data ---------------------------------------------------------------

write.csv(Soils_data , "./Data/Soils Data/Soils_Data_Tidy.csv",row.names = FALSE)


