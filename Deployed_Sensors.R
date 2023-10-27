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

All_Sonde_long <- read_csv("Data/Sonde/Ecotope_deployed_sonde_data_qualified.csv")   #Sonde Data joined and qualified using qualifier script


# Save Data ---------------------------------------------------------------

write.csv(All_Sonde_long,"./Data/Sonde/All_Sonde_long.csv",row.names = FALSE)
write.csv(All_Sonde_wide,"./Data/Sonde/All_Sonde_wide.csv",row.names = FALSE)

# Figures -----------------------------------------------------------------

#all parameters
ggplot(All_Sonde_long,aes(`Date Time`,Value,color=Ecotope,fill=Ecotope))+geom_point(shape=21)+
facet_wrap(~Parameter,scales = "free_y")+scale_fill_brewer(palette = "Set2",direction = -1)+scale_color_brewer(palette = "Set2",direction = -1)+theme_bw()

#all parameters log10 scale
ggplot(filter(All_Sonde_long,Value>0),aes(`Date Time`,Value,color=Ecotope,fill=Ecotope))+geom_point(shape=21)+
facet_wrap(~Parameter,scales = "free_y")+scale_fill_brewer(palette = "Set2",direction = -1)+scale_color_brewer(palette = "Set2",direction = -1)+scale_y_log10()+theme_bw()

#Turbidity log10 scale
ggplot(filter(All_Sonde_long,`Parameter`=="Turbidity FNU") ,aes(`Date Time`,Value,color=Ecotope,fill=Ecotope))+geom_point(shape=21)+
scale_fill_brewer(palette = "Set3",direction = -1)+scale_color_brewer(palette = "Set3",direction = -1)+scale_y_log10(breaks=c(.1,1,10,100,1000),labels=comma)+scale_x_datetime(date_breaks="1 week",labels = date_format("%b %d"))+theme_bw()+
theme(legend.position="bottom",axis.text.x=element_text(angle=90,hjust=1,size=8))  

#pH
ggplot(filter(All_Sonde_long,Parameter=="pH",`Remark Code` %in% c("?","J")==FALSE),aes(`Date Time`,Value,color=Ecotope,fill=Ecotope))+geom_point(shape=21)+
scale_fill_brewer(palette = "Set2",direction = -1)+scale_color_brewer(palette = "Set2",direction = -1)+theme_bw()

#Temp
ggplot(filter(All_Sonde_long,Parameter=="Temp C°"),aes(`Date Time`,Value,color=Ecotope,fill=Ecotope))+geom_point(shape=21,alpha=.5)+
scale_fill_brewer(palette = "Set2",direction = -1)+scale_color_brewer(palette = "Set2",direction = -1)+theme_bw()

#SpCond
ggplot(filter(All_Sonde_long,Parameter=="SpCond µS/cm"),aes(`Date Time`,Value,color=Ecotope,fill=Ecotope))+geom_point(shape=21,alpha=.5)+
scale_fill_brewer(palette = "Set2",direction = -1)+scale_color_brewer(palette = "Set2",direction = -1)+theme_bw()

#DO 
ggplot(filter(All_Sonde_long,Parameter=="DO (mg/L)",`Remark Code` %in% c("?","J")==FALSE),aes(`Date Time`,Value,color=Ecotope,fill=Ecotope))+geom_point(shape=21,alpha=.5)+
scale_fill_brewer(palette = "Set2",direction = -1)+scale_color_brewer(palette = "Set2",direction = -1)+theme_bw()

#DO%
ggplot(filter(All_Sonde_long,Parameter=="DO (mg/L)"),aes(`Date Time`,Value,color=Ecotope,fill=Ecotope))+geom_point(shape=21,alpha=.5)+
scale_fill_brewer(palette = "Set2",direction = -1)+scale_color_brewer(palette = "Set2",direction = -1)+theme_bw()

#Turbidity
ggplot(filter(All_Sonde_long,Parameter=="Turbidity FNU"),aes(`Date Time`,Value,color=Ecotope,fill=Ecotope))+geom_point(shape=21,alpha=.5)+
scale_fill_brewer(palette = "Set2",direction = -1)+scale_color_brewer(palette = "Set2",direction = -1)+theme_bw()+coord_cartesian(ylim = c(0,100))

#FDOM  scale
ggplot(filter(All_Sonde_long,`Parameter`=="fDOM RFU") ,aes(`Date Time`,Value,color=Ecotope,fill=Ecotope))+geom_point(shape=21)+
scale_fill_brewer(palette = "Set3",direction = -1)+scale_color_brewer(palette = "Set3",direction = -1)+scale_y_continuous(limits=c(0,300),breaks=c(0,100,200,300),labels=comma)+scale_x_datetime(date_breaks="1 week",labels = date_format("%b %d"))+theme_bw()+
theme(legend.position="bottom",axis.text.x=element_text(angle=90,hjust=1,size=8))  

# Helper Functions --------------------------------------------------------

#Renames unique columns to standard column names
Column_Name_fixer <-function(df)
{  
    for (i in 1:length(df)) 
    {
      colnames(df )[i]  <-if_else(str_detect(names(df)[i],"Site"),"Site",names(df)[i])
      colnames(df )[i]  <-if_else(str_detect(names(df)[i],"Date"),"Date",names(df)[i])
      colnames(df )[i]  <-if_else(str_detect(names(df)[i],"Time"),"Time",names(df)[i])
      colnames(df )[i]  <-if_else(str_detect(names(df)[i],"mV"),"Millivolts",names(df)[i])
      colnames(df )[i]  <-if_else(str_detect(names(df)[i],"°C"),"Temp C°",names(df)[i])
      colnames(df )[i]  <-if_else(str_detect(names(df)[i],"\\bDO %-\\b"),"DO %°",names(df)[i])
      colnames(df )[i]  <-if_else(str_detect(names(df)[i],"ODO % sat"),"DO %",names(df)[i])
      colnames(df )[i]  <-if_else(str_detect(names(df)[i],"DO %L"),"DO % Do not Collect",names(df)[i])
      colnames(df )[i]  <-if_else(str_detect(names(df)[i],"DO %"),"DO %",names(df)[i])
      colnames(df )[i]  <-if_else(str_detect(names(df)[i],"DO mg/L"),"DO mg/L",names(df)[i])
      colnames(df )[i]  <-if_else(str_detect(names(df)[i],"SPC-uS/cm"),"SpCond µS/cm",names(df)[i])
      colnames(df )[i]  <-if_else(str_detect(names(df)[i],"SpCond µS/cm"),"SpCond µS/cm",names(df)[i])
      colnames(df )[i]  <-if_else(str_detect(names(df)[i],"nLFC-uS/cm"),"Cond µS/cm",names(df)[i])
     # colnames(df )[i]  <-if_else(str_detect(names(df)[i],"Cond µS/cm"),"Cond µS/cm",names(df)[i])
      colnames(df )[i]  <-if_else(str_detect(names(df)[i],"TDS mg/L"),"TDS mg/L",names(df)[i])
      colnames(df )[i]  <-if_else(str_detect(names(df)[i],"pH"),"pH Sonde",names(df)[i])
      colnames(df )[i]  <-if_else(str_detect(names(df)[i],"pH-\\b"),"pH Sonde",names(df)[i])
      colnames(df )[i]  <-if_else(str_detect(names(df)[i],"FNU-\\b"),"Turbidity FNU",names(df)[i])
      colnames(df )[i]  <-if_else(str_detect(names(df)[i],"FNU"),"Turbidity FNU",names(df)[i])
      colnames(df )[i]  <-if_else(str_detect(names(df)[i],"TSS"),"TSS mg/L",names(df)[i])
      colnames(df )[i]  <-if_else(str_detect(names(df)[i],"BGA-PC RFU\\b"),"BGA-PC RFU",names(df)[i])
      colnames(df )[i]  <-if_else(str_detect(names(df)[i],"BGA-PCug/L\\b"),"BGA-PC ug/L",names(df)[i])
      colnames(df )[i]  <-if_else(str_detect(names(df)[i],"Chl RFU"),"Chl RFU",names(df)[i])
      colnames(df )[i]  <-if_else(str_detect(names(df)[i],"Chlorophyll RFU"),"Chl RFU",names(df)[i])
      colnames(df )[i]  <-if_else(str_detect(names(df)[i],"Chl ug/L"),"Chl ug/L",names(df)[i])
      colnames(df )[i]  <-if_else(str_detect(names(df)[i],"fDOM RFU"),"fDOM RFU",names(df)[i])
      colnames(df )[i]  <-if_else(str_detect(names(df)[i],"fDOM QSU"),"fDOM QSUL",names(df)[i])
      colnames(df )[i]  <-if_else(str_detect(names(df)[i],"DEP m"),"Depth (m)",names(df)[i])  #doesn't exist for all sondes
    }
  
  #Create character vector of column names only if the column name exists
  Name_exists <- character()
  ifelse("Date" %in% names(df), Name_exists <-c(Name_exists,"Date"),"")
  ifelse("Time" %in% names(df), Name_exists <-c(Name_exists,"Time"),"")
  ifelse("AMPM" %in% names(df), Name_exists <-c(Name_exists,"AMPM"),"")
  ifelse("Site" %in% names(df), Name_exists <-c(Name_exists,"Site"),"")
  ifelse("Unit ID" %in% names(df), Name_exists <-c(Name_exists,"Unit ID"),"")
  ifelse("Temp C°" %in% names(df), Name_exists <-c(Name_exists,"Temp C°"),"")
  ifelse("DO %" %in% names(df), Name_exists <-c(Name_exists,"DO %"),"")
  ifelse("DO mg/L" %in% names(df), Name_exists <-c(Name_exists,"DO mg/L"),"")
  ifelse("SpCond µS/cm" %in% names(df), Name_exists <-c(Name_exists,"SpCond µS/cm"),"")
  ifelse("Date" %in% names(df), Name_exists <-c(Name_exists,"Date"),"")
  ifelse("Cond µS/cm" %in% names(df), Name_exists <-c(Name_exists,"Cond µS/cm"),"")
  ifelse("TDS mg/L" %in% names(df), Name_exists <-c(Name_exists,"TDS mg/L"),"")
  ifelse("pH Sonde" %in% names(df), Name_exists <-c(Name_exists,"pH Sonde"),"")
  ifelse("Turbidity FNU" %in% names(df), Name_exists <-c(Name_exists,"Turbidity FNU"),"")
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


Date_fixer <-function(df)
{
df <- mutate(df,AMPM=" ")  #add am/pm column- remove once exodata time is fixed 

for(i in 1:nrow(df)) 
{       # for-loop over rows. Data file must start at 12:00AM to work
  df$AMPM[i] <- ifelse((i%%48)/24<=1,"AM","PM")  
}

return(df)
}

Site_fixer <- function(df) 
{
df1 <- df %>%
mutate(`Site`=case_when(str_detect(Site,"Bare")~"Bare",
                        str_detect(Site,"Naiad")~"Southern Naiad",
                        str_detect(Site,"Cattail")~"Typha",
                        str_detect(Site,"Typha")~"Typha",
                        str_detect(Site,"Chara")~"Chara",
                        str_detect(Site,"Mix")~"Mixed",
                        str_detect(Site,"South")~"Southern Naiad",
                        TRUE ~ as.character(Site)))
return(df1)  
}



