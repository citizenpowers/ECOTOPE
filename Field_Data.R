rm(list = ls())

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


Ecotope_Data_2021_2022 <- read_excel("Data/Field Data/Ecotope Data 2021-2022.xlsx", sheet = "Field Data", col_types = c("text",  "numeric", "date", "text", "text","numeric", "numeric", "numeric", "numeric", "numeric", "numeric","numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric",  "text"))



# Tidy Data ---------------------------------------------------------------


Field_data_wide <- Ecotope_Data_2021_2022 %>%
mutate(`Date Time`=ymd_hms(paste0(Date," ",trunc(`Collection Time`/100),":",`Collection Time`%%100,":00"))) %>% #create Date Time Column
mutate(Ecotope=case_when(str_detect(Site,"Chara")~"Chara",str_detect(Site,"Typha")~"Typha",str_detect(Site,"Naiad")~"Naiad", str_detect(Site,"Mixed")~"Mixed", str_detect(Site,"Bare")~"Bare"))  %>% #Rename stations to standard name
mutate(Position=case_when(str_detect(Site,"Downstream")~"Downstream",str_detect(Site,"Upstream")~"Upstream")) %>% #create column of upstram/downstream position
rename(`Collection Depth`="Collection Depth - inconsistant measurements") %>%
rowwise() %>% #allows for use of DPLYR verbs in Rows
mutate(`Average DCS (Field Data)`=mean(`DCS 1`,`DCS 2`,`DCS 3`,na.rm = TRUE)) %>%  #average DCS
mutate(`Average Water Column (Field Data)`=mean(`Water Depth 1`,`Water Depth 2`,`Water Depth 3`,na.rm = TRUE))  # Average Water Column

Field_data <- Field_data_wide %>%
select(`Date`,Ecotope,Position,`Average DCS (Field Data)`,`Average Water Column (Field Data)`,`Collection Depth`,Temp,pH,DO,SpCond) %>%  #Select desired paramters. Only numeric vairables. Can't mix numerica na character variables
filter(!is.na(Ecotope)) %>%  #remove FCEB rows
pivot_longer(names_to = "TEST_NAME",values_to="VALUE",4:10)   #Create long format data frame


# Save Data ---------------------------------------------------------------

write.csv(Field_data ,"./Data/Field Data/Field_data.csv",row.names = FALSE)
write.csv(Field_data_wide ,"./Data/Field Data/Field_data_wide.csv",row.names = FALSE)
