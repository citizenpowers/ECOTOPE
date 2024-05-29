
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
library(multcompView)
library(broom)

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
mutate(`Storage (g/m^2)`=ifelse(UNITS=="mg/Kg",`Bulk Density`*as.numeric(CORE_LENGTH)*VALUE/100,`Bulk Density`*as.numeric(CORE_LENGTH)*VALUE*10000)) %>%
mutate(TEST_NAME=case_when(TEST_NAME=="TMG-SOL"~"Magnesium",
                           TEST_NAME=="TAL-SOL"~"Aluminum",
                           TEST_NAME=="TFE-SOL"~"Iron",
                           TEST_NAME=="TCA-SOL"~"Calcium",
                           TEST_NAME=="TC-SOL"~"Carbon",
                           TEST_NAME=="TN-SOL"~"Nitrogen",
                           TEST_NAME=="TP-SOL"~"Phosphorus",
                           TEST_NAME=="MOISTURE"~"MOISTURE",
                           TEST_NAME=="ASH"~"Ash"))  %>%
mutate(MATRIX=case_when(MATRIX=="FLOC"~"Floc",MATRIX=="SO"~"RAS"))  

#Storage for units in mg/kg calculated differently than those as a percentage. 

# Run pairwise comparison to find stat sig between ecotopes for ea --------

#Soils Concentration table
Soils_Concentration_Summary <- Soils_data %>%
filter(Ecotope!="Naiad",TEST_NAME %in% c("Magnesium","Aluminum","Iron","Calcium","Carbon","Nitrogen","Phosphorus")) %>%
group_by(Ecotope,MATRIX,TEST_NAME) %>%
summarise(Samples=n(),`Mean`=mean(VALUE,na.rm=T),`SD`=sd(VALUE,na.rm=T),SE=sd(VALUE,na.rm=T)/n()^.5) %>%
mutate(`Units`="(mg/kg)") %>%
mutate(across(where(is.numeric) , ~round(.x,digits=1)))

#Soils Storage Table
Soils_Storage_Summary <- Soils_data %>%
filter(Ecotope!="Naiad",TEST_NAME %in% c("Magnesium","Aluminum","Iron","Calcium","Carbon","Nitrogen","Phosphorus")) %>%
group_by(Ecotope,MATRIX,TEST_NAME) %>%
summarise(Samples=n(),`Mean`=mean(`Storage (g/m^2)`,na.rm=T),`SD`=sd(`Storage (g/m^2)`,na.rm=T),SE=sd(`Storage (g/m^2)`,na.rm=T)/n()^.5) %>%
mutate(`Units`="(g/m^2)") %>%
mutate(across(where(is.numeric) , ~round(.x,digits=1)))  

#Physical Parameters Table
Soils_Physical_Summary <- Soils_data %>%
filter(Ecotope!="Naiad",TEST_NAME %in% c("Phosphorus")) %>%
select(Ecotope,MATRIX,`Bulk Density`,CORE_LENGTH)  %>%
mutate(CORE_LENGTH=as.numeric(CORE_LENGTH)) %>%  
pivot_longer(names_to="TEST_NAME",values_to="VALUE",3:4)  %>%
group_by(Ecotope,MATRIX,TEST_NAME) %>%
summarise(`Samples`=sum(is.finite(`VALUE`)),`Mean`=mean(`VALUE`,na.rm=T),`SD`=sd(`VALUE`,na.rm=T),SE=sd(`VALUE`,na.rm=T)/`Samples`^.5) %>%
mutate(Units=case_when(TEST_NAME=="Bulk Density" ~ "(g/cm^3)",TEST_NAME=="CORE_LENGTH" ~"(cm)"))  %>%
mutate(TEST_NAME=case_when(TEST_NAME=="CORE_LENGTH" ~ "Depth",TEST_NAME=="Bulk Density" ~ "Bulk Density")) %>%
mutate(across(where(is.numeric) , ~round(.x,digits=2)))

#All parameter table
All_Soils_Table_long <- Soils_Physical_Summary %>%
bind_rows(Soils_Storage_Summary) %>%
bind_rows(Soils_Concentration_Summary)  

#Join all soil measurements into a single table
All_Soils_Table_wide <- All_Soils_Table_long %>%
pivot_wider(values_from=c("Mean","SE","SD","Samples"),names_from="Ecotope") %>%
mutate(`Chara Mean ±SE (n)`=paste(`Mean_Chara`,"±",`SE_Chara`," (",`Samples_Chara`,")"),`Mixed Mean ±SE (n)`=paste(`Mean_Mixed`,"±",`SE_Mixed`," (",`Samples_Mixed`,")"),
         `Bare Mean ±SE (n)`=paste(`Mean_Bare`,"±",`SE_Bare`," (",`Samples_Bare`,")"),`Typha Mean ±SE (n)`=paste(`Mean_Typha`,"±",`SE_Typha`," (",`Samples_Typha`,")")) 

#Run ANOVA and Tukey's HSD on concentrations
Concentration_Anova_models <- Soils_data %>%
filter(Ecotope!="Naiad", TEST_NAME %in% c("Magnesium","Aluminum","Iron","Calcium","Carbon","Nitrogen","Phosphorus" )) %>%  
nest_by(MATRIX,TEST_NAME) %>% 
mutate(Model = list(lm(VALUE ~ Ecotope, data = data))) %>%   #create model
mutate(Tukey_HSD=list(TukeyHSD(aov(Model)))) %>%             #Tukey's pairwise comparison
mutate(Letters=list(multcompLetters4(Model, Tukey_HSD)$Ecotope$Letters)) %>% #Label ecotopes that are not sig different with same letter
mutate(Letters2=list(as.list(Letters)))  %>%                                 #create list that can be unnested. Multicomplist will not unnest.  
unnest_wider(Letters2) %>%
pivot_longer(names_to="Ecotope",values_to="Letter",7:10) %>%
select("Ecotope","MATRIX","TEST_NAME","Letter")  %>% 	mutate(Units="(mg/kg)")


#Run ANOVA and Tukey's HSD on Storages
Storage_Anova_models <- Soils_data %>%
filter(Ecotope!="Naiad", TEST_NAME %in% c("Magnesium","Aluminum","Iron","Calcium","Carbon","Nitrogen","Phosphorus")) %>%
mutate(Storage=`Storage (g/m^2)`) %>%
nest_by(MATRIX,TEST_NAME) %>% 
mutate(Model = list(lm(`Storage` ~ Ecotope, data = data))) %>%   #create model
mutate(Tukey_HSD=list(TukeyHSD(aov(Model)))) %>%             #Tukey's pairwise comparison
mutate(Letters=list(multcompLetters4(Model, Tukey_HSD)$Ecotope$Letters)) %>% #Label ecotopes that are not sig different with same letter
mutate(Letters2=list(as.list(Letters)))  %>%                                 #create list that can be unnested. Multicomplist will not unnest.  
unnest_wider(Letters2) %>%
pivot_longer(names_to="Ecotope",values_to="Letter",7:10) %>%
select("Ecotope","MATRIX","TEST_NAME","Letter")  %>% mutate(Units="(g/m^2)")

#Run ANOVA and Tukey's HSD on physical parameters
Physical_Anova_models <- Soils_data %>%
filter(Ecotope!="Naiad",TEST_NAME %in% c("Phosphorus")) %>%
select(Ecotope,MATRIX,`Bulk Density`,CORE_LENGTH)  %>%
mutate(CORE_LENGTH=as.numeric(CORE_LENGTH)) %>%  
pivot_longer(names_to="TEST_NAME",values_to="VALUE",3:4)  %>%
mutate(TEST_NAME=case_when(TEST_NAME=="CORE_LENGTH" ~ "Depth",TEST_NAME=="Bulk Density" ~ "Bulk Density")) %>%
nest_by(MATRIX,TEST_NAME) %>% 
mutate(Model = list(lm(`VALUE` ~ Ecotope, data = data))) %>%   #create model
mutate(Tukey_HSD=list(TukeyHSD(aov(Model)))) %>%             #Tukey's pairwise comparison
mutate(Letters=list(multcompLetters4(Model, Tukey_HSD)$Ecotope$Letters)) %>% #Label ecotopes that are not sig different with same letter
mutate(Letters2=list(as.list(Letters)))  %>%                                 #create list that can be unnested. Multicomplist will not unnest.  
unnest_wider(Letters2) %>%
pivot_longer(names_to="Ecotope",values_to="Letter",7:10) %>%
select("Ecotope","MATRIX","TEST_NAME","Letter")  %>% mutate(Units=case_when(TEST_NAME=="Bulk Density" ~ "(g/cm^3)",TEST_NAME=="Depth" ~"(cm)"))  


#join stat sig labels with summary table
Soils_Summary_Stat_Sig <- All_Soils_Table_long %>%
left_join(bind_rows(Concentration_Anova_models,Storage_Anova_models,Physical_Anova_models),by=c("Ecotope","MATRIX","TEST_NAME","Units"))  %>%
group_by(TEST_NAME,Units) %>% mutate(y=max(Mean)+max(SE))   %>% #add y coordinate to display labels
mutate(`Facet Label`=paste(TEST_NAME,Units))

# Save Data ---------------------------------------------------------------

write.csv(Soils_data , "./Data/Soils Data/Soils_Data_Tidy.csv",row.names = FALSE)
write.csv(Soils_Summary_Stat_Sig , "./Data/Soils Data/Soils_Summary_Stat_Sig.csv",row.names = FALSE)


