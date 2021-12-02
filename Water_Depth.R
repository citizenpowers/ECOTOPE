library(dplyr)
library(ggplot2)
library(readr)
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



# Import data -------------------------------------------------------------

#Deployment 1
Bare_Depth_20210609_Data <- mutate(read_csv("Data/Levelogger/20210609  Bare.csv",  skip = 11),Site="Bare")
Typha_Depth_20210609_Data <- mutate(read_csv("Data/Levelogger/20210609 Typha.csv",  skip = 11),Site="Typha")
Mixed_Depth_20210609_Data <- mutate(read_csv("Data/Levelogger/20210609 Mixed.csv",  skip = 11),Site="Mixed")
Chara_Depth_20210609_Data <- mutate(read_csv("Data/Levelogger/20210609 Chara.csv",  skip = 11),Site="Chara")
Naiad_Depth_20210609_Data <- mutate(read_csv("Data/Levelogger/20210609 Naiad.csv",  skip = 11),Site="Southern Naiad")




# Tidy Data ---------------------------------------------------------------

Water_Depth_Data <- bind_rows(Bare_Depth_20210609_Data,Typha_Depth_20210609_Data,Mixed_Depth_20210609_Data ,Chara_Depth_20210609_Data,Naiad_Depth_20210609_Data) %>%
mutate(`Date Time`=mdy_hms(paste(Date," ",Time," "),tz="America/New_York"))  %>%
filter(`Date Time`>"2021-06-09 11:30:00")



# Figures -----------------------------------------------------------------
ggplot(Water_Depth_Data,aes(`Date Time`,level,color=Site,fill=Site))+geom_point(shape=21)+
scale_fill_brewer(palette = "Set2",direction = -1)+scale_color_brewer(palette = "Set2",direction = -1)+theme_bw()





