#Vegetation Change 
#Objective is to import and join vegetation change data


# Library -----------------------------------------------------------------

library(tidyverse)



# Import Data -------------------------------------------------------------
Bare_20211117<- read.table("Data/Vegetation Change/Bare_20211117_area.txt",sep=",",header=T)  #Area in sq meters
Bare_20221219<- read.table("Data/Vegetation Change/Bare_20221219_area.txt",sep=",",header=T)  #Area in sq meters
Bare_20220322<- read.table("Data/Vegetation Change/Bare_20220322_area.txt",sep=",",header=T)  #Area in sq meters
Bare_20230525<- read.table("Data/Vegetation Change/Bare_20230525_area.txt",sep=",",header=T)  #Area in sq meters
Bare_20220630<- read.table("Data/Vegetation Change/Bare_20220630_area.txt",sep=",",header=T)  #Area in sq meters
Bare_20230723<- read.table("Data/Vegetation Change/Bare_20230723_area.txt",sep=",",header=T)  #Area in sq meters
Bare_20230726<- read.table("Data/Vegetation Change/Bare_20230726_area.txt",sep=",",header=T)   #Area in sq meters
Bare_20231012<- read.table("Data/Vegetation Change/Bare_20231012_area.txt",sep=",",header=T)  #Area in acres

Mixed_20211117<- read.table("Data/Vegetation Change/20211117_Mixed.txt",sep=",",header=T)  #Area in sq meters
Mixed_20220323<- read.table("Data/Vegetation Change/20220323_Mixed.txt",sep=",",header=T)  #Area in sq meters
Mixed_20220630<- read.table("Data/Vegetation Change/20220630_Mixed2.txt",sep=",",header=T)  #Area in sq meters
Mixed_20221219<- read.table("Data/Vegetation Change/20221219_Mixed.txt",sep=",",header=T)  #Area in sq meters
Mixed_20230525<- read.table("Data/Vegetation Change/20230525_Mixed.txt",sep=",",header=T)  #Area in sq meters
Mixed_20230723<- read.table("Data/Vegetation Change/20230723_Mixed.txt",sep=",",header=T)  #Area in sq meters
Mixed_20231012<- read.table("Data/Vegetation Change/20231012_Mixed3.txt",sep=",",header=T)  #Area in sq meters

Chara_20211117<- read.table("Data/Vegetation Change/20211117_Chara.txt",sep=",",header=T)  #Area in sq meters
Chara_20220323<- read.table("Data/Vegetation Change/20220323_Chara.txt",sep=",",header=T)  #Area in sq meters
Chara_20220630<- read.table("Data/Vegetation Change/20220630_Chara.txt",sep=",",header=T)  #Area in sq meters
Chara_20221219<- read.table("Data/Vegetation Change/20221219_Chara.txt",sep=",",header=T)  #Area in sq meters
Chara_20230525<- read.table("Data/Vegetation Change/20230525_Chara.txt",sep=",",header=T)  #Area in sq meters
Chara_20230725<- read.table("Data/Vegetation Change/20230725_Chara.txt",sep=",",header=T)  #Area in sq meters
Chara_20231012<- read.table("Data/Vegetation Change/20231012_Chara.txt",sep=",",header=T)  #Area in sq meters

Typha_20211117<- read.table("Data/Vegetation Change/20211117_Typha.txt",sep=",",header=T)  #Area in sq meters
Typha_20220630<- read.table("Data/Vegetation Change/20220630_Typha.txt",sep=",",header=T)  #Area in sq meters
Typha_20221219<- read.table("Data/Vegetation Change/20221219_Typha.txt",sep=",",header=T)  #Area in sq meters
Typha_20230725<- read.table("Data/Vegetation Change/20230723_Typha.txt",sep=",",header=T)  #Area in sq meters
Typha_20231012<- read.table("Data/Vegetation Change/20231031_Typha.txt",sep=",",header=T)  #Area in sq meters


# Tidy Data ---------------------------------------------------------------
#Bare_072323  1=Spatterdock,2=Lily,3=Cattail,4=Cattail,5=SAV,6=Bare,7=Cattail   Vegetation classes weren't exported from ArcMap. Entered manually 
#Bare_072623  1=Bare,5=SAV,13=Typha
Bare_20230726_fixed <- Bare_20230726 %>%  mutate(Vegetation=case_when(gridcode==1~"Bare",gridcode==5~"SAV",gridcode==13~"Typha"))
Bare_20230723_fixed <- Bare_20230723 %>%  mutate(Vegetation=case_when(gridcode==1~"FAV",gridcode==2~"FAV",gridcode==3~"Typha",gridcode==4~"Typha",gridcode==5~"SAV",gridcode==6~"Bare",gridcode==7~"Typha"))

#Mixed_20220323 1-bare,8-SAV, 17-Cattail
#Mixed_20220603 1-bare,2-SAV, 21-cattail
#Mixed_20221219 1-bare,10-SAV, 22-cattail
#Mixed_20230525 23-SAV, 8-cattail, 1-bare
#Mixed_20230723 1-bare,4-SAV,13-cattail
#Mixed_20231012 1-bare,5-SAV,10-Sparse SAV,16-cattail
Mixed_20220323_fixed <- Mixed_20220323 %>%  mutate(Vegetation=case_when(gridcode==1~"Bare",gridcode==8~"SAV",gridcode==17~"Typha"))
Mixed_20220630_fixed <- Mixed_20220630 %>%  mutate(Vegetation=case_when(gridcode==1~"Bare",gridcode==8~"SAV",gridcode==21~"Typha"))
Mixed_20221219_fixed <- Mixed_20221219 %>%  mutate(Vegetation=case_when(gridcode==1~"Bare",gridcode==10~"SAV",gridcode==22~"Typha"))
Mixed_20230525_fixed <- Mixed_20230525 %>%  mutate(Vegetation=case_when(gridcode==1~"Bare",gridcode==23~"SAV",gridcode==8~"Typha"))
Mixed_20230723_fixed <- Mixed_20230723 %>%  mutate(Vegetation=case_when(gridcode==1~"Bare",gridcode==4~"SAV",gridcode==13~"Typha"))
Mixed_20231012_fixed <- Mixed_20231012 %>%  mutate(Vegetation=case_when(gridcode==1~"Bare",gridcode==5~"SAV",gridcode==10~"SAV",gridcode==16~"Typha"))
Mixed_20211117_fixed <- Mixed_20211117 %>%  mutate(Vegetation=case_when(gridcode==1~"SAV",gridcode==8~"Typha",gridcode==13~"Bare"))

#20231012 1-Bare, 3-SAV,5-Typha,6-Spatterdock,12-Sparse SAV
#20230723 1-SAV, 9-Typha, 13-Spatterdock, 16-Bare, 21-Sparse SAV
#20230525 1-Bare, 11-SAV, 21-Typha, 23-FAV
#20221219 1-Bare, 9-SAV, 16-Typha, 20-FAV
#20220630 1-Bare,7-SAV,12-FAV,15-Typha
#20220323 1-Bare,9-SAV,15-Typha,18- FAV
#20211117 1-NA, 2-Bare, 4-Sparse SAV, 7-SAV, 12-Typha, 13-FAV
Chara_20231012_fixed <- Chara_20231012 %>%  mutate(Vegetation=case_when(gridcode==1~"Bare",gridcode==3~"SAV",gridcode==5~"Typha",gridcode==6~"FAV",gridcode==12~"SAV"))
Chara_20230725_fixed <- Chara_20230725 %>%  mutate(Vegetation=case_when(gridcode==1~"SAV",gridcode==9~"Typha",gridcode==13~"FAV",gridcode==16~"Bare",gridcode==21~"SAV"))
Chara_20230525_fixed <- Chara_20230525 %>%  mutate(Vegetation=case_when(gridcode==1~"Bare",gridcode==11~"SAV",gridcode==21~"Typha",gridcode==23~"FAV"))
Chara_20221219_fixed <- Chara_20221219 %>%  mutate(Vegetation=case_when(gridcode==1~"Bare",gridcode==9~"SAV",gridcode==16~"Typha",gridcode==20~"FAV"))
Chara_20220630_fixed <- Chara_20220630 %>%  mutate(Vegetation=case_when(gridcode==1~"Bare",gridcode==7~"SAV",gridcode==12~"FAV",gridcode==15~"Typha"))
Chara_20220323_fixed <- Chara_20220323 %>%  mutate(Vegetation=case_when(gridcode==1~"Bare",gridcode==9~"SAV",gridcode==15~"Typha",gridcode==18~"FAV"))
Chara_20211117_fixed <- Chara_20211117 %>%  mutate(Vegetation=case_when(gridcode==1~NA,gridcode==2~"Bare",gridcode==4~"SAV",gridcode==7~"SAV",gridcode==12~"Typha",gridcode==13~"FAV"))

#20231031 SAV-12,Typha-1,Bare-14 
#20230725 1-Typha, 6-Bare, 8-SAV,12-FAV
#20220630 1-Typha, 6-SAV,9-Bare,11-FAV 
#20221219 1-Typha, 3-SAV, 6-NA 
#20211117 1-Typha, 5-NA, 6-SAV, 10-FAV
Typha_20211117_fixed <- Typha_20211117 %>%  mutate(Vegetation=case_when(gridcode==1~"Typha",gridcode==5~NA,gridcode==6~"SAV",gridcode==10~"FAV"))
Typha_20221219_fixed <- Typha_20221219 %>%  mutate(Vegetation=case_when(gridcode==1~"Typha",gridcode==6~NA,gridcode==3~"SAV"))
Typha_20230725_fixed <- Typha_20230725 %>%  mutate(Vegetation=case_when(gridcode==1~"Typha",gridcode==6~"Bare",gridcode==12~"FAV"))
Typha_20220630_fixed <- Typha_20220630 %>%  mutate(Vegetation=case_when(gridcode==1~"Typha",gridcode==9~"Bare",gridcode==11~"FAV",gridcode==6~"SAV"))
Typha_20231012_fixed <- Typha_20231012 %>%  mutate(Vegetation=case_when(gridcode==1~"Typha",gridcode==14~"Bare",gridcode==12~"SAV"))


#join data
Bare_joined <-select(mutate(Bare_20231012,Ecotope="Bare",Date=ymd("2023-10-12"),Area=Area*4046.86),Date,Ecotope,Vegetation,Area) %>%  #convert to sq meters
bind_rows(select(rename(mutate(Bare_20220322,Ecotope="Bare",Date=ymd("2022-03-22")),Vegetation="Class_name",Area="Shape_Area"),Date,Ecotope,Vegetation,Area)) %>%
bind_rows(select(rename(mutate(Bare_20230525,Ecotope="Bare",Date=ymd("2023-05-25")),Vegetation="Class_name",Area="Shape_Area"),Date,Ecotope,Vegetation,Area)) %>% 
bind_rows(select(rename(mutate(Bare_20220630,Ecotope="Bare",Date=ymd("2022-06-30")),Vegetation="Class_name",Area="Shape_Area"),Date,Ecotope,Vegetation,Area)) %>%
bind_rows(select(rename(mutate(Bare_20221219,Ecotope="Bare",Date=ymd("2022-12-19")),Vegetation="Class_name",Area="Shape_Area"),Date,Ecotope,Vegetation,Area)) %>%
bind_rows(select(rename(mutate(Bare_20211117,Ecotope="Bare",Date=ymd("2021-11-17")),Vegetation="Class_name",Area="Shape_Area"),Date,Ecotope,Vegetation,Area)) %>% #NA is blank unmappe area
bind_rows(select(rename(mutate(Bare_20230726_fixed,Ecotope="Bare",Date=ymd("2023-07-26")),Area="Shape_Area"),Date,Ecotope,Vegetation,Area)) %>%
bind_rows(select(rename(mutate(Bare_20230723_fixed,Ecotope="Bare",Date=ymd("2023-07-23")),Area="Shape_Area"),Date,Ecotope,Vegetation,Area)) %>%
mutate(Vegetation=case_when(str_detect(Vegetation,"tail")~"Typha",str_detect(Vegetation,"Bare")~"Bare",str_detect(Vegetation,"SAV")~"SAV",str_detect(Vegetation,"ily")~"Lily",str_detect(Vegetation,"Spatterdock")~"Spatterdock",TRUE~Vegetation))  %>%
mutate(Vegetation=case_when(str_detect(Vegetation,"Spatterdock")~"FAV",str_detect(Vegetation,"Lily")~"FAV",TRUE~Vegetation))  

Mixed_joined <-select(mutate(Mixed_20231012_fixed,Ecotope="Mixed",Date=ymd("2023-10-12")),Date,Ecotope,Vegetation,Area) %>%  
bind_rows(select(mutate(Mixed_20220323_fixed,Ecotope="Mixed",Date=ymd("2022-03-22")),Date,Ecotope,Vegetation,Area)) %>%
bind_rows(select(mutate(Mixed_20230525_fixed,Ecotope="Mixed",Date=ymd("2023-05-25")),Date,Ecotope,Vegetation,Area)) %>% 
bind_rows(select(mutate(Mixed_20220630_fixed,Ecotope="Mixed",Date=ymd("2022-06-30")),Date,Ecotope,Vegetation,Area)) %>%
bind_rows(select(mutate(Mixed_20221219_fixed,Ecotope="Mixed",Date=ymd("2022-12-19")),Date,Ecotope,Vegetation,Area)) %>%
bind_rows(select(mutate(Mixed_20211117_fixed,Ecotope="Mixed",Date=ymd("2021-11-17")),Date,Ecotope,Vegetation,Area)) %>%
bind_rows(select(mutate(Mixed_20230723_fixed,Ecotope="Mixed",Date=ymd("2023-07-23")),Date,Ecotope,Vegetation,Area)) %>%
mutate(Vegetation=case_when(str_detect(Vegetation,"tail")~"Typha",str_detect(Vegetation,"Bare")~"Bare",str_detect(Vegetation,"SAV")~"SAV",str_detect(Vegetation,"ily")~"Lily",str_detect(Vegetation,"Spatterdock")~"Spatterdock",TRUE~Vegetation))  %>%
mutate(Vegetation=case_when(str_detect(Vegetation,"Spatterdock")~"FAV",str_detect(Vegetation,"Lily")~"FAV",TRUE~Vegetation))  

Chara_joined <-select(mutate(Chara_20231012_fixed,Ecotope="Chara",Date=ymd("2023-10-12")),Date,Ecotope,Vegetation,Area) %>%  
bind_rows(select(mutate(Chara_20220323_fixed,Ecotope="Chara",Date=ymd("2022-03-22")),Date,Ecotope,Vegetation,Area)) %>%
bind_rows(select(mutate(Chara_20230525_fixed,Ecotope="Chara",Date=ymd("2023-05-25")),Date,Ecotope,Vegetation,Area)) %>% 
bind_rows(select(mutate(Chara_20220630_fixed,Ecotope="Chara",Date=ymd("2022-06-30")),Date,Ecotope,Vegetation,Area)) %>%
bind_rows(select(mutate(Chara_20221219_fixed,Ecotope="Chara",Date=ymd("2022-12-19")),Date,Ecotope,Vegetation,Area)) %>%
bind_rows(select(mutate(Chara_20211117_fixed,Ecotope="Chara",Date=ymd("2021-11-17")),Date,Ecotope,Vegetation,Area)) %>%
bind_rows(select(mutate(Chara_20230725_fixed,Ecotope="Chara",Date=ymd("2023-07-25")),Date,Ecotope,Vegetation,Area)) %>%
mutate(Vegetation=case_when(str_detect(Vegetation,"tail")~"Typha",str_detect(Vegetation,"Bare")~"Bare",str_detect(Vegetation,"SAV")~"SAV",str_detect(Vegetation,"ily")~"Lily",str_detect(Vegetation,"Spatterdock")~"Spatterdock",TRUE~Vegetation))  %>%
mutate(Vegetation=case_when(str_detect(Vegetation,"Spatterdock")~"FAV",str_detect(Vegetation,"Lily")~"FAV",TRUE~Vegetation))  

Typha_joined <-select(mutate(Typha_20231012_fixed,Ecotope="Typha",Date=ymd("2023-10-12")),Date,Ecotope,Vegetation,Area) %>%  
bind_rows(select(mutate(Typha_20220630_fixed,Ecotope="Typha",Date=ymd("2022-06-30")),Date,Ecotope,Vegetation,Area)) %>%
bind_rows(select(mutate(Typha_20221219_fixed,Ecotope="Typha",Date=ymd("2022-12-19")),Date,Ecotope,Vegetation,Area)) %>%
bind_rows(select(mutate(Typha_20211117_fixed,Ecotope="Typha",Date=ymd("2021-11-17")),Date,Ecotope,Vegetation,Area)) %>%
bind_rows(select(mutate(Typha_20230725_fixed,Ecotope="Typha",Date=ymd("2023-07-25")),Date,Ecotope,Vegetation,Area)) %>%
mutate(Vegetation=case_when(str_detect(Vegetation,"tail")~"Typha",str_detect(Vegetation,"Bare")~"Bare",str_detect(Vegetation,"SAV")~"SAV",str_detect(Vegetation,"ily")~"Lily",str_detect(Vegetation,"Spatterdock")~"Spatterdock",TRUE~Vegetation))  %>%
mutate(Vegetation=case_when(str_detect(Vegetation,"Spatterdock")~"FAV",str_detect(Vegetation,"Lily")~"FAV",TRUE~Vegetation))  


#join data
Veg_joined <- bind_rows(Bare_joined,Mixed_joined,Chara_joined,Typha_joined) 

#Calculate area by veg class
Veg_areas <-Veg_joined %>%
group_by(Ecotope,Date,Vegetation) %>%
summarise(Area=sum(Area,na.rm=T)/4046.86)

#Calculate total area of bare ecotope
Veg_total_area <- Veg_joined  %>% 
group_by(Ecotope,Date) %>%
summarise(`Total Area`=sum(Area,na.rm=T)/4046.86)  

#join total area to vegetation area totals, join totals, and calculate percentages
Veg_tidy <- Veg_areas %>%
pivot_wider(names_from=Vegetation,values_from=Area,names_prefix =  "Acres_") %>%
left_join(Veg_total_area,by=c("Ecotope","Date"))  %>%
mutate(`Percentage_Bare`=Acres_Bare/`Total Area`,`Percentage_FAV`=Acres_FAV/`Total Area`,Percentage_SAV=Acres_SAV/`Total Area`,Percentage_Typha=Acres_Typha/`Total Area`)  

#Create long format dataframe
Veg_tidy_long <- Veg_tidy %>%
pivot_longer(names_to=c("Units","Vegetation"),names_sep='_',c(`Percentage_Bare`:`Percentage_Typha`,Acres_Bare:Acres_NA))

write.csv(Veg_tidy_long,"./Data/Vegetation Change/Bare_tidy_long.csv",row.names = FALSE)
write.csv(Veg_tidy,"./Data/Vegetation Change/Bare_tidy.csv",row.names = FALSE)


