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
Bare_Sonde <- read_csv("Data/Sonde/EXOdata_Bare_ECOTOPE_060921.csv")  #deployment 1
Chara_Sonde <- read_csv("Data/Sonde/EXOdata_Chara_ECOTOPE_060921.csv")   #deployment 1
Cattail_Sonde <- read_csv("Data/Sonde/EXOdata_Cattail_ECOTOPE_060921.csv")   #deployment 1
Southern_N_Sonde <- read_csv("Data/Sonde/EXOdata_Southern_N_ECOTOPE_060921.csv")  #deployment 1
Mixed_Sonde <- read_csv("Data/Sonde/EXOdata_Mixed_ECOTOPE_060921.csv")   #deployment 1

#Deployment 2
Bare_Sonde_082621 <- read_csv("Data/Sonde/pdynSTA34A41_ecotopeBare - 082621 174830.csv")   #file contains data from PDYNAMICS site and ECOTOPE site
Cattail_Sonde_082621 <- read_csv("Data/Sonde/Cattail - 083121 133214.csv")   #file contains data from PDYNAMICS site and ECOTOPE site
Chara_Sonde_082621 <-  read_csv("Data/Sonde/Chara - 083121 133903.csv")  #file contains data from PDYNAMICS site and ECOTOPE site
Mixed_Sonde_082621 <-  read_csv("Data/Sonde/Mixed- 083121 134557.csv")   #file contains data from PDYNAMICS site and ECOTOPE site
Naiad_Sonde_082621<- read_csv("Data/Sonde/Southern Naiad- 083121 135134.csv")  #file contains data from PDYNAMICS site and ECOTOPE site

#Deployment 3
Bare_091521 <- read_csv("Data/Sonde/20210915_bare.csv")
Mixed_091521 <- read_csv("Data/Sonde/20210915_mixed.csv")
Chara_091521 <- read_csv("Data/Sonde/20210915_chara.csv")
Naiad_091521 <- read_csv("Data/Sonde/20210915_naiad.csv")
Typha_091521 <- read_csv("Data/Sonde/20210915_typha.csv")

# Tidy Data from deployment 1---------------------------------------------------------------

All_Sonde_wide_1 <- bind_rows(Column_Name_fixer(Bare_Sonde),Column_Name_fixer(Chara_Sonde), Column_Name_fixer(Cattail_Sonde), Column_Name_fixer(Mixed_Sonde),Column_Name_fixer(Southern_N_Sonde)) %>%
Date_fixer() %>%
mutate(`Date Time`=mdy_hms(paste(Date," ",Time," ",AMPM))) %>%
filter(`Date Time`<="2021-06-28 12:00:00")  #Sondes deployment ended


# Tidy data from deployment 2 ---------------------------------------------

Bare_Sonde_2 <-Column_Name_fixer(Date_fixer(Bare_Sonde_082621)) %>%
mutate(`Date Time`=mdy_hms(paste(Date," ",Time," ",AMPM))) %>%
slice(1993:2325) %>% #sonde was switched from PDYNAMICS site to ECOTOPE site at  row 1991 and collected at 2325
mutate(Site="STA3/4C2B_Ecotope_Bare")
  
Cattail_Sonde_2 <-Column_Name_fixer(Cattail_Sonde_082621)  %>%
Date_fixer()  %>%
mutate(`Site`="Typha")  %>%
mutate(`Date Time`=mdy_hms(paste(Date," ",Time," ",AMPM))) %>% 
slice(3875:4847)
  
Chara_Sonde_2 <- Column_Name_fixer(Chara_Sonde_082621) %>%
mutate(`Date Time`=mdy_hms(paste(`Date`," ",`Time`," "))) %>%
slice(1994:2328) %>% #sonde was switched from PDYNAMICS site to ECOTOPE site at  row 1994 and collected at 2328
mutate(Site="STA3/4C2B_Ecotope_Chara")

Mixed_Sonde_2 <- Column_Name_fixer(Mixed_Sonde_082621) %>%
mutate(`Date Time`=mdy_hms(paste(`Date`," ",`Time`," "))) %>%
slice(1995:2326) %>% #sonde was switched from PDYNAMICS site to ECOTOPE site at  row 1995 and collected at 2326
mutate(Site="STA3/4C2B_Ecotope_Mixed")

Naiad_Sonde_2 <- Column_Name_fixer(Naiad_Sonde_082621) %>%
mutate(`Date Time`=mdy_hms(paste(`Date`," ",`Time`," "))) %>%
mutate(across(4:11, ~ as.numeric(.))) %>%
slice(1992:2325) %>%  #sonde was switched from PDYNAMICS site to ECOTOPE site at  row 1992 and collected at 1995
mutate(Site="STA3/4C2B_Ecotope_Naiad")

All_Sonde_wide_2 <- bind_rows(Bare_Sonde_2,Naiad_Sonde_2,Mixed_Sonde_2,Chara_Sonde_2,Cattail_Sonde_2)



# PDYNAMICS Data ----------------------------------------------------------
PDYNAMICS_STA34C2B56_070721_081717_Data <-Column_Name_fixer(Cattail_Sonde_082621)  %>%
Date_fixer()  %>%
mutate(`Date Time`=mdy_hms(paste(Date," ",Time," ",AMPM))) %>% #can't find break in data when sonde was moved from PDYNAMICS to ECOTOPE
slice(1:3875)

# write.csv(PDYNAMICS_STA34C2B56_070721_081717_Data ,"PDYNAMICS_STA34C2B56_070721_081717.csv")

PDYNAMICS_STA34C2A41_DATA <-Column_Name_fixer(Bare_Sonde_082621) %>%
mutate(`Date Time`=mdy_hms(paste(Date," ",Time," ",AMPM))) %>%
slice(1:1990) #sonde was switched from PDYNAMICS site to ECOTOPE site at  row 1991 and collected at 2325

# write.csv(PDYNAMICS_STA34C2A41_DATA,"PDYNAMICS_STA34C2A41_071721_081717.csv")

PDYNAMICS_STA34C2A27_DATA <-Naiad_Sonde_082621 %>%
mutate(`Date Time`=mdy_hms(paste(`Date (MM/DD/YYYY)`," ",`Time (HH:MM:SS)`," "))) %>%
slice(1:1992) #sonde was switched from PDYNAMICS site to ECOTOPE site at  row 1991 and collected at 2325

# write.csv(PDYNAMICS_STA34C2A27_DATA,"PDYNAMICS_STA34C2A27_071721_081717.csv")

PDYNAMICS_STA34C2B33_DATA <-Mixed_Sonde_082621 %>%
mutate(`Date Time`=mdy_hms(paste(`Date (MM/DD/YYYY)`," ",`Time (HH:MM:SS)`," "))) %>%
slice(1:1992) #sonde was switched from PDYNAMICS site to ECOTOPE site at  row 1991 and collected at 2325

# write.csv(PDYNAMICS_STA34C2B33_DATA,"PDYNAMICS_STA34C2B33_070721_081717.csv")

PDYNAMICS_STA34C2A9_DATA <-Chara_Sonde_082621 %>%
mutate(`Date Time`=mdy_hms(paste(`Date (MM/DD/YYYY)`," ",`Time (HH:MM:SS)`," "))) %>%
slice(1:1990) #sonde was switched from PDYNAMICS site to ECOTOPE site at  row 1991 and collected at 2325

# write.csv(PDYNAMICS_STA34C2A9_DATA,"PDYNAMICS_STA34C2A9_070721_081717.csv")

# Deployment 3 ------------------------------------------------------------

All_Sonde_wide_3 <- bind_rows(Column_Name_fixer(Bare_091521),Column_Name_fixer(Chara_091521), Column_Name_fixer(Typha_091521), Column_Name_fixer(Naiad_091521),Column_Name_fixer(Mixed_091521)) %>%
mutate(`Date Time`=mdy_hms(paste(`Date`," ",`Time`," "),tz="America/New_York"))  %>%
filter(`Date Time`>"2021-09-15 09:30:00",`Date Time`<"2021-11-09 11:30:00")  #Time sondes were deployed
  
# Join Event Data ---------------------------------------------------------

All_Sonde_wide <-bind_rows(All_Sonde_wide_1,All_Sonde_wide_2,All_Sonde_wide_3) %>%
Site_fixer()

All_Sonde_long <- All_Sonde_wide %>%
pivot_longer(names_to = "Parameter",values_to="Value",5:18) 



# Save Data ---------------------------------------------------------------

write.csv(All_Sonde_long,"./Data/Sonde/All_Sonde_long.csv",row.names = FALSE)




# Figures -----------------------------------------------------------------

#all parameters
ggplot(All_Sonde_long,aes(`Date Time`,Value,color=Site,fill=Site))+geom_point(shape=21)+
facet_wrap(~Parameter,scales = "free_y")+scale_fill_brewer(palette = "Set2",direction = -1)+scale_color_brewer(palette = "Set2",direction = -1)+theme_bw()

#all parameters log10 scale
ggplot(filter(All_Sonde_long,Value>0),aes(`Date Time`,Value,color=Site,fill=Site))+geom_point(shape=21)+
facet_wrap(~Parameter,scales = "free_y")+scale_fill_brewer(palette = "Set2",direction = -1)+scale_color_brewer(palette = "Set2",direction = -1)+scale_y_log10()+theme_bw()

#Turbidity log10 scale
ggplot(filter(All_Sonde_long,`Parameter`=="Turbidity FNU") ,aes(`Date Time`,Value,color=Site,fill=Site))+geom_point(shape=21)+
scale_fill_brewer(palette = "Set3",direction = -1)+scale_color_brewer(palette = "Set3",direction = -1)+scale_y_log10(breaks=c(.1,1,10,100,1000),labels=comma)+scale_x_datetime(date_breaks="12 hours",labels = date_format("%b %d %I%p"))+theme_bw()+
theme(legend.position="bottom",axis.text.x=element_text(angle=90,hjust=1,size=8))  

#pH
ggplot(filter(All_Sonde_long,Parameter=="pH"),aes(`Date Time`,Value,color=Site,fill=Site))+geom_point(shape=21)+
scale_fill_brewer(palette = "Set2",direction = -1)+scale_color_brewer(palette = "Set2",direction = -1)+theme_bw()

#Temp
ggplot(filter(All_Sonde_long,Parameter=="Temp C°"),aes(`Date Time`,Value,color=Site,fill=Site))+geom_point(shape=21,alpha=.5)+
scale_fill_brewer(palette = "Set2",direction = -1)+scale_color_brewer(palette = "Set2",direction = -1)+theme_bw()

#Temp
ggplot(filter(All_Sonde_long,Parameter=="SpCond µS/cm"),aes(`Date Time`,Value,color=Site,fill=Site))+geom_point(shape=21,alpha=.5)+
scale_fill_brewer(palette = "Set2",direction = -1)+scale_color_brewer(palette = "Set2",direction = -1)+theme_bw()

#DO 
ggplot(filter(All_Sonde_long,Parameter=="DO mg/L"),aes(`Date Time`,Value,color=Site,fill=Site))+geom_point(shape=21,alpha=.5)+
scale_fill_brewer(palette = "Set2",direction = -1)+scale_color_brewer(palette = "Set2",direction = -1)+theme_bw()

#DO%
ggplot(filter(All_Sonde_long,Parameter=="DO %°"),aes(`Date Time`,Value,color=Site,fill=Site))+geom_point(shape=21,alpha=.5)+
scale_fill_brewer(palette = "Set2",direction = -1)+scale_color_brewer(palette = "Set2",direction = -1)+theme_bw()

#Turbidity
ggplot(filter(All_Sonde_long,Parameter=="Turbidity FNU"),aes(`Date Time`,Value,color=Site,fill=Site))+geom_point(shape=21,alpha=.5)+
scale_fill_brewer(palette = "Set2",direction = -1)+scale_color_brewer(palette = "Set2",direction = -1)+theme_bw()+coord_cartesian(ylim = c(0,100))


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
      colnames(df )[i]  <-if_else(str_detect(names(df)[i],"DO mg/L"),"DO mg/L",names(df)[i])
      colnames(df )[i]  <-if_else(str_detect(names(df)[i],"SPC-uS/cm"),"SpCond µS/cm",names(df)[i])
      colnames(df )[i]  <-if_else(str_detect(names(df)[i],"SpCond µS/cm"),"SpCond µS/cm",names(df)[i])
      colnames(df )[i]  <-if_else(str_detect(names(df)[i],"nLFC-uS/cm"),"Cond µS/cm",names(df)[i])
     # colnames(df )[i]  <-if_else(str_detect(names(df)[i],"Cond µS/cm"),"Cond µS/cm",names(df)[i])
      colnames(df )[i]  <-if_else(str_detect(names(df)[i],"TDS mg/L"),"TDS mg/L",names(df)[i])
      colnames(df )[i]  <-if_else(str_detect(names(df)[i],"pH-\\b"),"pH",names(df)[i])
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
  ifelse("pH" %in% names(df), Name_exists <-c(Name_exists,"pH"),"")
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


