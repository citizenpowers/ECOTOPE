#Objective of this script is to evaluate spatial differences in TPO4 accross ecotope

library(dplyr)
library(tidyr)
library(stringr)
library(lubridate)
library(scales)
library(readxl)
library(ggplot2)
library(ggthemr)



# Import Data -------------------------------------------------------------


WQ_Data <- read_excel("Data/WQ Data/WQ Data.xlsx",sheet = "Sheet1")
Spatial_Site_Surveys <- read_excel("Data/Spatial Data/Spatial Site Surveys.xlsx")
Coordinates_all_sites <- read_excel("Data/Spatial Data/Coordinates all sites.xlsx",  sheet = "Sheet1")

# Theme  ------------------------------------------------------------------

ggthemr("flat dark",type="outer", layout="scientific")   #used for presntation figs


# Tidy Data ---------------------------------------------------------------

Spatial_Tidy <-WQ_Data  %>%
filter(STATION!="OCS",SAMPLE_TYPE=="SAMP",MATRIX=="SW") %>%  
mutate(Date=as.Date(COLLECT_DATE)) %>%
filter(Date=="2023-07-25" | Date=="2023-07-26" | Date == "2023-08-15")  %>%   #spatial sampling days
mutate(Ecotope=case_when(str_detect(STATION,"STA34C2B_C")~"Chara",
                           str_detect(STATION,"STA34C2B_T")~"Typha",
                           str_detect(STATION,"STA34C2B_N")~"Naiad",
                           str_detect(STATION,"STA34C2B_M")~"Mixed",
                           str_detect(STATION,"STA34C2B_B")~"Bare",
                           str_detect(STATION,"STA1WC5B_C")~"Chara",
                           str_detect(STATION,"STA1WC5B_T")~"Typha",
                           str_detect(STATION,"STA1WC5B_M")~"Mixed",
                           str_detect(STATION,"STA1WC5B_B")~"Bare"))  %>%
mutate(STA=case_when(str_detect(STATION,"STA34")~"STA-3/4 Cell 2B",str_detect(STATION,"STA1W")~"STA-1W Cell 5B"))  %>%
select(STA,Ecotope,SAMPLE_ID,STATION,COLLECT_DATE,TEST_NAME,TEST_NUMBER,VALUE) %>%
mutate(`Site`=as.numeric(str_split_i(SAMPLE_ID,"-",2))) %>%
left_join(Spatial_Site_Surveys,by=c("Ecotope","Site")) %>%
mutate_at(c('Chara','Typha','Bare','Nuphar','Potamogeton','Utricularia','Spiny Naiad','Southern Naiad','Periphyton'), ~replace_na(.,0)) %>%
mutate(VALUE=VALUE*1000) %>%  
group_by(Ecotope) %>%
mutate(`Mean TP`=mean(VALUE,na.rm=TRUE)) %>%
mutate(`Difference from mean`=VALUE-`Mean TP`+10) %>%
mutate(`Diff from mean label`=VALUE-`Mean TP`)


Coordinates_tidy <- Coordinates_all_sites  %>%
mutate(FID=case_when(FID==0 ~25,FID==1 ~26,FID>1~FID)) %>% #coordinate sites 0 and 1 match TP values for dash #s 25 and 26 
rename(Site="FID")

WQ_and_Spatial <- Spatial_Tidy %>%
left_join(Coordinates_tidy, by=c("Ecotope", "Site"))


# Save Data ---------------------------------------------------------------

write.csv(Spatial_Tidy , "./Data/WQ Data/Spatial_Tidy.csv",row.names = FALSE) 


# Figures -----------------------------------------------------------------

#histogram
ggplot(Spatial_Tidy,aes(VALUE,color=Ecotope,fill=Ecotope))+geom_histogram(binwidth =1,alpha=.5,position = "dodge" )+theme_bw()
#boxplots
ggplot(Spatial_Tidy,aes(VALUE,color=Ecotope,fill=Ecotope))+geom_boxplot()+theme_bw()
#Chara COverage vs TP
ggplot(Spatial_Tidy,aes(Chara,`Difference from mean`,color=Ecotope,fill=Ecotope))+geom_point(size=2)+theme_bw()+facet_wrap(~Ecotope)
ggplot(Spatial_Tidy,aes(Chara,`Difference from mean`,color=Ecotope,fill=Ecotope))+geom_point(size=2)+theme_bw()
#Nuphar COverage vs TP
ggplot(Spatial_Tidy,aes(Nuphar,`Difference from mean`,color=Ecotope,fill=Ecotope))+geom_point(size=2)+theme_bw()+facet_wrap(~Ecotope)
ggplot(Spatial_Tidy,aes(Nuphar,`Difference from mean`,color=Ecotope,fill=Ecotope))+geom_point(size=2)+theme_bw()
#Typha COverage vs TP
ggplot(Spatial_Tidy,aes(Typha,`Difference from mean`,color=Ecotope,fill=Ecotope))+geom_point(size=2)+theme_bw()+facet_wrap(~Ecotope)
ggplot(Spatial_Tidy,aes(Typha,`Difference from mean`,color=Ecotope,fill=Ecotope))+geom_point(size=2)+theme_bw()
#Bare COverage vs TP
ggplot(Spatial_Tidy,aes(Bare,`Difference from mean`,color=Ecotope,fill=Ecotope))+geom_point(size=2)+theme_bw()+facet_wrap(~Ecotope)
ggplot(Spatial_Tidy,aes(Bare,`Difference from mean`,color=Ecotope,fill=Ecotope))+geom_point(size=2)+theme_bw()


# Maps --------------------------------------------------------------------

ggplot(WQ_and_Spatial ,aes(x=Long, y=Lat,size=VALUE))+geom_point()

ggplot(WQ_and_Spatial ,aes(x=Lat, y=Long,color=Ecotope,size=`Diff from mean label`))+geom_point()+scale_size(range=c(-2,8))+scale_size_continuous(breaks=breaks_pretty(6))

                                                                                                                                                  