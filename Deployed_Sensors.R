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


Bare_Sonde <- read_csv("Data/Sonde/EXOdata_Bare_ECOTOPE_060921.csv")  #deployment 1
Chara_Sonde <- read_csv("Data/Sonde/EXOdata_Chara_ECOTOPE_060921.csv")   #deployment 1
Cattail_Sonde <- read_csv("Data/Sonde/EXOdata_Cattail_ECOTOPE_060921.csv")   #deployment 1
Southern_N_Sonde <- read_csv("Data/Sonde/EXOdata_Southern_N_ECOTOPE_060921.csv")  #deployment 1
Mixed_Sonde <- read_csv("Data/Sonde/EXOdata_Mixed_ECOTOPE_060921.csv")   #deployment 1
Bare_Sonde_082621 <- read_csv("Data/Sonde/pdynSTA34A41_ecotopeBare - 082621 174830.csv")   #file contains data from PDYNAMICS site and ECOTOPE site


# Tidy Data from deployment 1---------------------------------------------------------------

All_Sonde_wide_1 <- bind_rows(Column_Name_fixer(Bare_Sonde),Column_Name_fixer(Chara_Sonde), Column_Name_fixer(Cattail_Sonde), Column_Name_fixer(Mixed_Sonde),Column_Name_fixer(Southern_N_Sonde)) %>%
mutate(`Date Time`=mdy_hms(paste(Date," ",Time," ",AMPM))) %>%
filter(`Date Time`<="2021-06-28 12:00:00")  #Sondes deployment ended


# Tidy data from deployment 2 ---------------------------------------------

Bare_Sonde_2 <-Column_Name_fixer(Bare_Sonde_082621) %>%
mutate(`Date Time`=mdy_hms(paste(Date," ",Time," ",AMPM))) %>%
slice(1993:2325) %>% #sonde was switched from PDYNAMICS site to ECOTOPE site at  row 1991 and collected at 2325
mutate(Site="STA3/4C2B_Ecotope_Bare")
  
PDYNAMICS_DATA <-Column_Name_fixer(Bare_Sonde_082621) %>%
mutate(`Date Time`=mdy_hms(paste(Date," ",Time," ",AMPM))) %>%
slice(1:1990) #sonde was switched from PDYNAMICS site to ECOTOPE site at  row 1991 and collected at 2325

write.csv(PDYNAMICS_DATA,"PDYNAMICS_DATA_STA34C2A41_071721_081717.csv")

# Join Event Data ---------------------------------------------------------

All_Sonde_wide <-bind_rows(All_Sonde_wide_1,Bare_Sonde_2)   

All_Sonde_long <- All_Sonde_wide %>%
pivot_longer(names_to = "Parameter",values_to="Value",6:20) 


# Figures -----------------------------------------------------------------

#all parameters
ggplot(All_Sonde_long,aes(`Date Time`,Value,color=Site,fill=Site))+geom_point(shape=21)+
facet_wrap(~Parameter,scales = "free_y")+scale_fill_brewer(palette = "Set2",direction = -1)+scale_color_brewer(palette = "Set2",direction = -1)+theme_bw()

#all parameters log10 scale
ggplot(filter(All_Sonde_long,Value>0),aes(`Date Time`,Value,color=Site,fill=Site))+geom_point(shape=21)+
facet_wrap(~Parameter,scales = "free_y")+scale_fill_brewer(palette = "Set2",direction = -1)+scale_color_brewer(palette = "Set2",direction = -1)+scale_y_log10()+theme_bw()

#Turbidity log10 scale
ggplot(filter(All_Sonde_long,`Parameter`=="FNU") ,aes(`Date Time`,Value,color=Site,fill=Site))+geom_point(shape=21)+
scale_fill_brewer(palette = "Set3",direction = -1)+scale_color_brewer(palette = "Set3",direction = -1)+scale_y_log10(breaks=c(.1,1,10,100,1000),labels=comma)+scale_x_datetime(date_breaks="12 hours",labels = date_format("%b %d %I%p"))+theme_bw()+
theme(legend.position="bottom",axis.text.x=element_text(angle=90,hjust=1,size=8))  





# Helper Functions --------------------------------------------------------

#Renames unique columns to standard column names
Column_Name_fixer <-function(df)
{  
    for (i in 1:length(df)) 
    {
      colnames(df )[i]  <-if_else(str_detect(names(df)[i],"°C"),"Temp C°",names(df)[i])
      colnames(df )[i]  <-if_else(str_detect(names(df)[i],"\\bDO %-\\b"),"DO %°",names(df)[i])
      colnames(df )[i]  <-if_else(str_detect(names(df)[i],"DO mg/L"),"DO mg/L",names(df)[i])
      colnames(df )[i]  <-if_else(str_detect(names(df)[i],"SPC-uS/cm"),"SpCond uS/cm",names(df)[i])
      colnames(df )[i]  <-if_else(str_detect(names(df)[i],"nLFC-uS/cm"),"nLFC-uS/cm",names(df)[i])
      colnames(df )[i]  <-if_else(str_detect(names(df)[i],"TDS mg/L"),"TDS mg/L",names(df)[i])
      colnames(df )[i]  <-if_else(str_detect(names(df)[i],"pH-\\b"),"pH",names(df)[i])
      colnames(df )[i]  <-if_else(str_detect(names(df)[i],"FNU-\\b"),"FNU",names(df)[i])
      colnames(df )[i]  <-if_else(str_detect(names(df)[i],"TSS mg/L"),"TSS mg/L",names(df)[i])
      colnames(df )[i]  <-if_else(str_detect(names(df)[i],"BGA-PC RFU\\b"),"BGA-PC RFU",names(df)[i])
      colnames(df )[i]  <-if_else(str_detect(names(df)[i],"BGA-PCug/L\\b"),"BGA-PC ug/L",names(df)[i])
      colnames(df )[i]  <-if_else(str_detect(names(df)[i],"Chl RFU"),"Chl RFU",names(df)[i])
      colnames(df )[i]  <-if_else(str_detect(names(df)[i],"Chl ug/L"),"Chl ug/L",names(df)[i])
      colnames(df )[i]  <-if_else(str_detect(names(df)[i],"fDOM RFU"),"fDOM RFU",names(df)[i])
      colnames(df )[i]  <-if_else(str_detect(names(df)[i],"fDOM QSU"),"fDOM QSUL",names(df)[i])
      colnames(df )[i]  <-if_else(str_detect(names(df)[i],"DEP m"),"Depth (m)",names(df)[i])  #doesn't exist for all sondes
    }
  
  
  df <- mutate(df,AMPM=" ")  #add am/pm column- remove once exodata time is fixed 
  
  for(i in 1:nrow(df)) 
  {       # for-loop over rows. Data file must start at 12:00AM to work
    df$AMPM[i] <- ifelse((i%%48)/24<=1,"AM","PM")  
  }
 
  #Create character vector of column names only if the column name exists
  Name_exists <- character()
  ifelse("Date" %in% names(df), Name_exists <-c(Name_exists,"Date"),"")
  ifelse("Time" %in% names(df), Name_exists <-c(Name_exists,"Time"),"")
  ifelse("AMPM" %in% names(df), Name_exists <-c(Name_exists,"AMPM"),"")
  ifelse("Site" %in% names(df), Name_exists <-c(Name_exists,"Site"),"")
  ifelse("Unit ID" %in% names(df), Name_exists <-c(Name_exists,"Unit ID"),"")
  ifelse("Temp C°" %in% names(df), Name_exists <-c(Name_exists,"Temp C°"),"")
  ifelse("DO %°" %in% names(df), Name_exists <-c(Name_exists,"DO %°"),"")
  ifelse("DO mg/L" %in% names(df), Name_exists <-c(Name_exists,"DO mg/L"),"")
  ifelse("SpCond uS/cm" %in% names(df), Name_exists <-c(Name_exists,"SpCond uS/cm"),"")
  ifelse("Date" %in% names(df), Name_exists <-c(Name_exists,"Date"),"")
  ifelse("nLFC-uS/cm" %in% names(df), Name_exists <-c(Name_exists,"nLFC-uS/cm"),"")
  ifelse("TDS mg/L" %in% names(df), Name_exists <-c(Name_exists,"TDS mg/L"),"")
  ifelse("pH" %in% names(df), Name_exists <-c(Name_exists,"pH"),"")
  ifelse("FNU" %in% names(df), Name_exists <-c(Name_exists,"FNU"),"")
  ifelse("TSS mg/L" %in% names(df), Name_exists <-c(Name_exists,"TSS mg/L"),"")
  ifelse("BGA-PC RFU" %in% names(df), Name_exists <-c(Name_exists,"BGA-PC RFU"),"")
  ifelse("BGA-PC ug/L" %in% names(df), Name_exists <-c(Name_exists,"BGA-PC ug/L"),"")
  ifelse("Chl RFU" %in% names(df), Name_exists <-c(Name_exists,"Chl RFU"),"")
  ifelse("Chl ug/L" %in% names(df), Name_exists <-c(Name_exists,"Chl ug/L"),"")
  ifelse("fDOM RFU" %in% names(df), Name_exists <-c(Name_exists,"fDOM RFU"),"")
  ifelse("fDOM QSUL" %in% names(df), Name_exists <-c(Name_exists,"fDOM QSUL"),"")
  
  #select existing column names
  select(df,Name_exists )    
 
}  
