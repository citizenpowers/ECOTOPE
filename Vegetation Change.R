#Vegetation Change 
#Objective is to import and join vegetaton change data


# Library -----------------------------------------------------------------





# Import Data -------------------------------------------------------------
Bare_20211117<- read.table("Data/Vegetation Change/Bare_20211117_area.txt",sep=",",header=T)  #Area in sq meters
Bare_20221219<- read.table("Data/Vegetation Change/Bare_20221219_area.txt",sep=",",header=T)  #Area in sq meters
Bare_20220322<- read.table("Data/Vegetation Change/Bare_20220322_area.txt",sep=",",header=T)  #Area in sq meters
Bare_20230525<- read.table("Data/Vegetation Change/Bare_20230525_area.txt",sep=",",header=T)  #Area in sq meters
Bare_20220630<- read.table("Data/Vegetation Change/Bare_20220630_area.txt",sep=",",header=T)  #Area in sq meters
Bare_20230723<- read.table("Data/Vegetation Change/Bare_20230723_area.txt",sep=",",header=T)  #Area in sq meters
Bare_20230726<- read.table("Data/Vegetation Change/Bare_20230726_area.txt",sep=",",header=T)   #Area in sq meters
Bare_20231012<- read.table("Data/Vegetation Change/Bare_20231012_area.txt",sep=",",header=T)  #Area in acres

#072323  1=Spatterdock,2=Lily,3=Cattail,4=Cattail,5=SAV,6=Bare,7=Cattail   Vegetation classes weren't exported from ArcMap. Entered manually 
#072623  1=Bare,5=SAV,13=Typha

# Tidy Data ---------------------------------------------------------------
#072323  1=Spatterdock,2=Lily,3=Cattail,4=Cattail,5=SAV,6=Bare,7=Cattail   Vegetation classes weren't exported from ArcMap. Entered manually 
#072623  1=Bare,5=SAV,13=Typha
Bare_20230726_fixed <- Bare_20230726 %>%  mutate(Vegetation=case_when(gridcode==1~"Bare",gridcode==5~"SAV",gridcode==13~"Typha"))
Bare_20230723_fixed <- Bare_20230723 %>%  mutate(Vegetation=case_when(gridcode==1~"Spatterdock",gridcode==2~"Lily",gridcode==3~"Typha",gridcode==4~"Typha",gridcode==5~"SAV",gridcode==6~"Bare",gridcode==7~"Typha"))

#join data
Bare_joined <-select(mutate(Bare_20231012,Ecotope="Bare",Date=ymd("2023-10-12"),Area=Area*4046.86),Date,Ecotope,Vegetation,Area) %>%  #convert to sq meters
bind_rows(select(rename(mutate(Bare_20220322,Ecotope="Bare",Date=ymd("2022-03-22")),Vegetation="Class_name",Area="Shape_Area"),Date,Ecotope,Vegetation,Area)) %>%
bind_rows(select(rename(mutate(Bare_20220525,Ecotope="Bare",Date=ymd("2023-05-25")),Vegetation="Class_name",Area="Shape_Area"),Date,Ecotope,Vegetation,Area)) %>% 
bind_rows(select(rename(mutate(Bare_20220630,Ecotope="Bare",Date=ymd("2022-06-30")),Vegetation="Class_name",Area="Shape_Area"),Date,Ecotope,Vegetation,Area)) %>%
bind_rows(select(rename(mutate(Bare_20221219,Ecotope="Bare",Date=ymd("2022-12-19")),Vegetation="Class_name",Area="Shape_Area"),Date,Ecotope,Vegetation,Area)) %>%
bind_rows(select(rename(mutate(Bare_20211117,Ecotope="Bare",Date=ymd("2021-11-17")),Vegetation="Class_name",Area="Shape_Area"),Date,Ecotope,Vegetation,Area)) %>% #NA is blank unmappe area
bind_rows(select(rename(mutate(Bare_20230726_fixed,Ecotope="Bare",Date=ymd("2023-07-26")),Area="Shape_Area"),Date,Ecotope,Vegetation,Area)) %>%
bind_rows(select(rename(mutate(Bare_20230723_fixed,Ecotope="Bare",Date=ymd("2023-07-23")),Area="Shape_Area"),Date,Ecotope,Vegetation,Area)) %>%
mutate(Vegetation=case_when(str_detect(Vegetation,"tail")~"Typha",
                            str_detect(Vegetation,"Bare")~"Bare",
                            str_detect(Vegetation,"SAV")~"SAV",
                            str_detect(Vegetation,"ily")~"Lily",
                            str_detect(Vegetation,"Spatterdock")~"Spatterdock",
                            TRUE~Vegetation)) 

#Calculate area by veg class
Bare_veg_areas <-Bare_joined %>%
group_by(Date,Vegetation) %>%
summarise(Area=sum(Area,na.rm=T)/4046.86)

#Calculate total area of bare ecotope
Bare_total_area <- Bare_joined %>% 
group_by(Date) %>%
summarise(`Total Area`=sum(Area,na.rm=T)/4046.86)  
  
#join total area to vegetation area totals, join totals, and calculate percentages
Bare_tidy <- Bare_veg_areas %>%
pivot_wider(names_from=Vegetation,values_from=Area,names_prefix =  "Acres_") %>%
left_join(Bare_total_area,by="Date")  %>%
mutate(`Percentage_Bare`=Acres_Bare/`Total Area`,`Percentage_Lily`=Acres_Lily/`Total Area`,Percentage_SAV=Acres_SAV/`Total Area`,Percentage_Spatterdock=Acres_Spatterdock/`Total Area`,Percentage_Typha=Acres_Typha/`Total Area`)  

#Create long format dataframe
Bare_tidy_long <- Bare_tidy %>%
pivot_longer(names_to=c("Units","Vegetation"),names_sep='_',c(`Percentage_Bare`:`Percentage_Typha`,Acres_Bare:Acres_Spatterdock))

write.csv(Bare_tidy_long,"./Data/Vegetation Change/Bare_tidy_long.csv",row.names = FALSE)
write.csv(Bare_tidy,"./Data/Vegetation Change/Bare_tidy.csv",row.names = FALSE)


