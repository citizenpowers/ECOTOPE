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


Bare_Sonde <- read_csv("Data/Sonde/EXOdata_Bare_ECOTOPE_060921.csv")
Chara_Sonde <- read_csv("Data/Sonde/EXOdata_Chara_ECOTOPE_060921.csv")
Cattail_Sonde <- read_csv("Data/Sonde/EXOdata_Cattail_ECOTOPE_060921.csv")
Southern_N_Sonde <- read_csv("Data/Sonde/EXOdata_Southern_N_ECOTOPE_060921.csv")
Mixed_Sonde <- read_csv("Data/Sonde/EXOdata_Mixed_ECOTOPE_060921.csv")



# Tidy Data ---------------------------------------------------------------

All_Sonde_wide <- bind_rows(Column_Name_fixer(Bare_Sonde),Column_Name_fixer(Chara_Sonde), Column_Name_fixer(Cattail_Sonde), Column_Name_fixer(Mixed_Sonde),Column_Name_fixer(Southern_N_Sonde)) %>%
mutate(`Date Time`=mdy_hms(paste(Date," ",Time," ",AMPM))) %>%
filter(`Date Time`<="2021-06-28 12:00:00")  #Sondes deployment ended

test <-Column_Name_fixer(Bare_Sonde) %>%
mutate(`Date Time`=mdy_hms(paste(Date," ",Time," ",AMPM)))  

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
  {       # for-loop over rows
    df$AMPM[i] <- ifelse((i%%48)/24<=1,"AM","PM")  
  }
 
  df<- select(df,"Date","Time","AMPM","Site","Unit ID","Temp C°","DO %°","DO mg/L","SpCond uS/cm","nLFC-uS/cm","TDS mg/L","pH","FNU","TSS mg/L","BGA-PC RFU","BGA-PC ug/L","Chl RFU","Chl ug/L","fDOM RFU","fDOM QSUL")
  
  
}  



