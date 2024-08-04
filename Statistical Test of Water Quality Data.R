#The objective of this script is to run statistical tests on water quality data



library(tidyverse)
library(ggpubr)
library(rstatix)
library(PMCMRplus)
library(rcompanion)
library(DescTools)

# Import Data -------------------------------------------------------------

WQ_Field_Data <- read_csv("./Data/Joined Data/WQ_Field_Data.csv")


# Statistical tests -------------------------------------------------------
Stat_test_data <-WQ_Field_Data %>%
mutate(Month=month(Date),Season=if_else(between(Month,6,11),"Wet Season","Dry Season")) %>%
filter(Position=="Downstream",Ecotope!="Naiad") %>%
select(Date,STA,Ecotope,Position,Season,TPO4) %>%
mutate(Date_fct=as.factor(Date),Ecotope_fct=as.factor(Ecotope)) 

# #All STA3/4 data for period of record -No stat sig -----------------------------------

Stat_test_STA34_data <-Stat_test_data  %>% filter(STA=="STA-3/4 Cell 2B") %>%
mutate(Date_fct=as.factor(Date),Ecotope_fct=as.factor(Ecotope)) 

#Friedman test for STA3/4 -No stat sig
friedman_test(TPO4 ~ Ecotope_fct|Date_fct,data= Stat_test_STA34_data)

#post-hoc test
Stat_test_STA34_data_post_hoc = frdAllPairsConoverTest(y      = Stat_test_STA34_data$TPO4,
                                                     groups = Stat_test_STA34_data$Ecotope_fct,
                                                     blocks = Stat_test_STA34_data$Date_fct,
                                                     p.adjust.method="single-step")
#post-hoc test
Stat_test_STA34_data_post_hoc_table =PMCMRTable(Stat_test_STA34_data_post_hoc)

#get monoletter for stat sig
Stat_test_STA34_data_post_hoc_table_letters <-cldList(p.value ~ Comparison, data = Stat_test_STA34_data_post_hoc_table) %>%
rename(Ecotope="Group")

# STA3/4 data  year 1 -No stat sig ------------------------------------------
Stat_test_STA34_yr1_data <-WQ_Field_Data %>%
filter(Position=="Downstream",STA=="STA-3/4 Cell 2B",Ecotope!="Naiad",Date<"2022-08-27") %>%
select(Date,STA,Ecotope,Position,TPO4) %>%
mutate(Date_fct=as.factor(Date),Ecotope_fct=as.factor(Ecotope))  

#Friedman test for STA3/4 wet season -Stat sig
friedman_test(TPO4 ~ Ecotope_fct|Date_fct,data= Stat_test_STA34_yr1_data )
# STA3/4 data  year 2 -Stat sig ---------------------------------------------------
Stat_test_STA34_yr2_data <-WQ_Field_Data %>%
  filter(Position=="Downstream",STA=="STA-3/4 Cell 2B",Ecotope!="Naiad",Date>"2022-08-27") %>%
  select(Date,STA,Ecotope,Position,TPO4) %>%
  mutate(Date_fct=as.factor(Date),Ecotope_fct=as.factor(Ecotope))  

#Friedman test for STA3/4 wet season -Stat sig
friedman_test(TPO4 ~ Ecotope_fct|Date_fct,data= Stat_test_STA34_yr2_data )

#post-hoc test
STA34_yr2_data_post_hoc = frdAllPairsConoverTest(y      = Stat_test_STA34_yr2_data$TPO4,
                                                     groups = Stat_test_STA34_yr2_data$Ecotope_fct,
                                                     blocks = Stat_test_STA34_yr2_data$Date_fct,
                                                     p.adjust.method="single-step")
#post-hoc test
STA34_yr2_data_post_hoc_table =PMCMRTable(STA34_yr2_data_post_hoc)

#get monoletter for stat sig
STA34_yr2_data_post_hoc_table_letters <-cldList(p.value ~ Comparison, data = STA34_yr2_data_post_hoc_table)

#calculate effect size using KendallW
STA34_yr2_effect_size <- Stat_test_STA34_yr2_data %>%
select(Date_fct,Ecotope_fct,TPO4) %>%  
pivot_wider(values_from="TPO4",names_from="Ecotope_fct")

KendallW(STA34_yr2_effect_size,correct=TRUE,test=TRUE)


# STA3/4 data wet season year 1 -Stat sig ------------------------------------------
Stat_test_STA34_wet_yr1_data <-WQ_Field_Data %>%
mutate(Month=month(Date),Season=if_else(between(Month,6,11),"Wet Season","Dry Season")) %>%
filter(Position=="Downstream",STA=="STA-3/4 Cell 2B",Ecotope!="Naiad",Season=="Wet Season",Date<"2022-08-27") %>%
select(Date,STA,Ecotope,Position,TPO4) %>%
mutate(Date_fct=as.factor(Date),Ecotope_fct=as.factor(Ecotope))  

#Friedman test for STA3/4 wet season -Stat sig
friedman_test(TPO4 ~ Ecotope_fct|Date_fct,data= Stat_test_STA34_wet_yr1_data )

#post-hoc test
STA34_wet_yr1_data_post_hoc = frdAllPairsConoverTest(y      = Stat_test_STA34_wet_yr1_data$TPO4,
                                                       groups = Stat_test_STA34_wet_yr1_data$Ecotope_fct,
                                                       blocks = Stat_test_STA34_wet_yr1_data$Date_fct,
                                                       p.adjust.method="single-step")
#post-hoc test
STA34_wet_yr1_data_post_hoc_table =PMCMRTable(STA34_wet_yr1_data_post_hoc)

#get monoletter for stat sig
STA34_wet_yr1_data_post_hoc_table_letters <-cldList(p.value ~ Comparison, data = STA34_wet_yr1_data_post_hoc_table) %>%
rename(Ecotope="Group")

#calculate effect size using KendallW
STA34_wet_yr1_data_effect_size <- Stat_test_STA34_wet_yr1_data %>%
select(Date_fct,Ecotope_fct,TPO4) %>%  
pivot_wider(values_from="TPO4",names_from="Ecotope_fct")

KendallW(STA34_wet_yr1_data_effect_size,correct=TRUE,test=TRUE)
# STA3/4 data wet season year 2 -stat sig -------------------------------------------
Stat_test_STA34_wet_yr2_data <-WQ_Field_Data %>%
  mutate(Month=month(Date),Season=if_else(between(Month,6,11),"Wet Season","Dry Season")) %>%
  filter(Position=="Downstream",STA=="STA-3/4 Cell 2B",Ecotope!="Naiad",Season=="Wet Season",Date>"2022-08-27") %>%
  select(Date,STA,Ecotope,Position,TPO4) %>%
  mutate(Date_fct=as.factor(Date),Ecotope_fct=as.factor(Ecotope))  

#Friedman test for STA3/4 wet season -Stat sig
friedman_test(TPO4 ~ Ecotope_fct|Date_fct,data= Stat_test_STA34_wet_yr2_data )

#post-hoc test
STA34_wet_yr2_data_post_hoc = frdAllPairsConoverTest(y      = Stat_test_STA34_wet_yr2_data$TPO4,
                                                     groups = Stat_test_STA34_wet_yr2_data$Ecotope_fct,
                                                     blocks = Stat_test_STA34_wet_yr2_data$Date_fct,
                                                     p.adjust.method="single-step")
#post-hoc test
STA34_wet_yr2_data_post_hoc_table =PMCMRTable(STA34_wet_yr2_data_post_hoc)

#get monoletter for stat sig
STA34_wet_yr2_data_post_hoc_table_letters <-cldList(p.value ~ Comparison, data = STA34_wet_yr2_data_post_hoc_table) %>%
rename(Ecotope="Group")



# STA3/4 data dry season year 1 -------------------------------------------
Stat_test_STA34_dry_yr1_data <-WQ_Field_Data %>%
mutate(Month=month(Date),Season=if_else(between(Month,6,11),"Wet Season","Dry Season")) %>%
filter(Position=="Downstream",STA=="STA-3/4 Cell 2B",Ecotope!="Naiad",Season=="Dry Season",Date<"2022-08-27") %>%
select(Date,STA,Ecotope,Position,TPO4) %>%
mutate(Date_fct=as.factor(Date),Ecotope_fct=as.factor(Ecotope)) 

#Friedman test for STA3/4 dry season year 1- stat sig
friedman_test(TPO4 ~ Ecotope_fct|Date_fct,data= Stat_test_STA34_dry_yr1_data )

#post-hoc test
STA34_dry_yr1_data_post_hoc = frdAllPairsConoverTest(y      = Stat_test_STA34_dry_yr1_data$TPO4,
                                                     groups = Stat_test_STA34_dry_yr1_data$Ecotope_fct,
                                                     blocks = Stat_test_STA34_dry_yr1_data$Date_fct,
                                                     p.adjust.method="single-step")
#post-hoc test
STA34_dry_yr1_data_post_hoc_table =PMCMRTable(STA34_dry_yr1_data_post_hoc)

#get monoletter for stat sig
STA34_dry_yr1_data_post_hoc_table_letters <-cldList(p.value ~ Comparison, data = STA34_dry_yr1_data_post_hoc_table) %>%
rename(Ecotope="Group")

# STA3/4 data dry season year 2 ------------------------------------------
Stat_test_STA34_dry_yr2_data <-WQ_Field_Data %>%
mutate(Month=month(Date),Season=if_else(between(Month,6,11),"Wet Season","Dry Season")) %>%
filter(Position=="Downstream",STA=="STA-3/4 Cell 2B",Ecotope!="Naiad",Season=="Dry Season",Date>"2022-08-27") %>%
select(Date,STA,Ecotope,Position,TPO4) %>%
mutate(Date_fct=as.factor(Date),Ecotope_fct=as.factor(Ecotope)) 

#Friedman test for STA3/4 dry season year 1- stat sig
friedman_test(TPO4 ~ Ecotope_fct|Date_fct,data= Stat_test_STA34_dry_yr2_data )

#post-hoc test
STA34_dry_yr2_data_post_hoc = frdAllPairsConoverTest(y      = Stat_test_STA34_dry_yr2_data$TPO4,
                                                     groups = Stat_test_STA34_dry_yr2_data$Ecotope_fct,
                                                     blocks = Stat_test_STA34_dry_yr2_data$Date_fct,
                                                     p.adjust.method="single-step")
#post-hoc test
STA34_dry_yr2_data_post_hoc_table =PMCMRTable(STA34_dry_yr2_data_post_hoc) 

#get monoletter for stat sig
STA34_dry_yr2_data_post_hoc_table_letters <-cldList(p.value ~ Comparison, data = STA34_dry_yr2_data_post_hoc_table) %>%
rename(Ecotope="Group")


#STA3/4 data wet season
Stat_test_STA34_dry_data <-WQ_Field_Data %>%
mutate(Month=month(Date),Season=if_else(between(Month,6,11),"Wet Season","Dry Season")) %>%
filter(Position=="Downstream",STA=="STA-3/4 Cell 2B",Ecotope!="Naiad",Season=="Dry Season") %>%
select(Date,STA,Ecotope,Position,TPO4) %>%
mutate(Date_fct=as.factor(Date),Ecotope_fct=as.factor(Ecotope)) 

#Friedman test for STA3/4 dry season -Not stat sig
friedman_test(TPO4 ~ Ecotope_fct|Date_fct,data= Stat_test_STA34_dry_data )


# #All STA-1W data for period of record- Stat sig ------------------------------------

Stat_test_STA1W_data <-WQ_Field_Data %>%
  filter(Position=="Downstream",STA=="STA-1W Cell 5B",Ecotope!="Naiad") %>%
  mutate(Date_fct=as.factor(Date),Ecotope_fct=as.factor(Ecotope)) 

#Friedman test for STA1W- Stat sig
friedman_test(TPO4 ~ Ecotope_fct|Date_fct,data= Stat_test_STA1W_data )

#post-hoc test
Stat_test_STA1W_data_post_hoc = frdAllPairsConoverTest(y      = Stat_test_STA1W_data$TPO4,
                                                       groups = Stat_test_STA1W_data$Ecotope_fct,
                                                       blocks = Stat_test_STA1W_data$Date_fct,
                                                       p.adjust.method="single-step")
#create table
STA1W_data_post_hoc_table =PMCMRTable(Stat_test_STA1W_data_post_hoc)

#get monoletter for stat sig
STA1W_data_post_hoc_table_letters <-cldList(p.value ~ Comparison, data = STA1W_data_post_hoc_table)


# STA-1W wet season  -Stat sig------------------------------------------------------
Stat_test_STA1W_wet_data <-WQ_Field_Data %>%
  mutate(Month=month(Date),Season=if_else(between(Month,6,11),"Wet Season","Dry Season")) %>%
  filter(Position=="Downstream",STA=="STA-1W Cell 5B",Ecotope!="Naiad",Season=="Wet Season") %>%
  select(Date,STA,Ecotope,Position,TPO4) %>%
  mutate(Date_fct=as.factor(Date),Ecotope_fct=as.factor(Ecotope))  

#Friedman test for STA3/4 wet season -Stat sig
friedman_test(TPO4 ~ Ecotope_fct|Date_fct,data= Stat_test_STA1W_wet_data )

#post-hoc test
Stat_test_STA1W_wet_data_post_hoc = frdAllPairsConoverTest(y      = Stat_test_STA1W_wet_data$TPO4,
                                                     groups = Stat_test_STA1W_wet_data$Ecotope_fct,
                                                     blocks = Stat_test_STA1W_wet_data$Date_fct,
                                                     p.adjust.method="single-step")
Stat_test_STA1W_wet_data_post_hoc_table =PMCMRTable(Stat_test_STA1W_wet_data_post_hoc)

#get monoletter for stat sig
Stat_test_STA1W_wet_data_post_hoc_table<-cldList(p.value ~ Comparison, data = Stat_test_STA1W_wet_data_post_hoc_table)

# STA-1W dry season  -Stat sig------------------------------------------------------
Stat_test_STA1W_dry_data <-WQ_Field_Data %>%
mutate(Month=month(Date),Season=if_else(between(Month,6,11),"Wet Season","Dry Season")) %>%
filter(Position=="Downstream",STA=="STA-1W Cell 5B",Ecotope!="Naiad",Season=="Dry Season") %>%
select(Date,STA,Ecotope,Position,TPO4) %>%
mutate(Date_fct=as.factor(Date),Ecotope_fct=as.factor(Ecotope))  

#Friedman test for STA3/4 wet season -Stat sig
friedman_test(TPO4 ~ Ecotope_fct|Date_fct,data= Stat_test_STA1W_dry_data )

#post-hoc test
Stat_test_STA1W_dry_data_post_hoc = frdAllPairsConoverTest(y      = Stat_test_STA1W_dry_data$TPO4,
                                                           groups = Stat_test_STA1W_dry_data$Ecotope_fct,
                                                           blocks = Stat_test_STA1W_dry_data$Date_fct,
                                                           p.adjust.method="single-step")

Stat_test_STA1W_dry_data_post_hoc_table =PMCMRTable(Stat_test_STA1W_dry_data_post_hoc)

#get monoletter for stat sig
Stat_test_STA1W_dry_data_post_hoc_table<-cldList(p.value ~ Comparison, data = Stat_test_STA1W_dry_data_post_hoc_table)



# Upstream data -----------------------------------------------------------


Upstream_data <-WQ_Field_Data %>%
mutate(Month=month(Date),Season=if_else(between(Month,6,11),"Wet Season","Dry Season")) %>%
filter(Position=="Upstream",Ecotope!="Naiad") %>%
select(Date,STA,Ecotope,Position,Season,TPO4) %>%
mutate(Date_fct=as.factor(Date),Ecotope_fct=as.factor(Ecotope)) 

# #All Upstream STA3/4 data for period of record -No stat sig -----------------------------------
Upstream_data_STA34_data <-Upstream_data %>% filter(STA=="STA-3/4 Cell 2B") %>%
mutate(Date_fct=as.factor(Date),Ecotope_fct=as.factor(Ecotope)) 

#Friedman test for STA3/4 -No stat sig
friedman_test(TPO4 ~ Ecotope_fct|Date_fct,data= Upstream_data_STA34_data )


# #All Upstream STA3/4 data for period of record -No stat sig -----------------------------------
Upstream_data_STA1W_data <-Upstream_data %>% filter(STA=="STA-1W Cell 5B") %>%
mutate(Date_fct=as.factor(Date),Ecotope_fct=as.factor(Ecotope)) 

#Friedman test for STA3/4 -No stat sig
friedman_test(TPO4 ~ Ecotope_fct|Date_fct,data= Upstream_data_STA1W_data )



# Combine Statistical Data into combined table ----------------------------

WQ_Stat_Sig <- Stat_test_STA34_wet_yr2_data %>% group_by(Ecotope) %>% summarise(Median=Median(TPO4,na.rm=T),IQR_25=quantile(TPO4,.25),IQR_75=quantile(TPO4,.75)) %>% mutate(Treatment="Wet Season Year 2") %>%
left_join(STA34_wet_yr2_data_post_hoc_table_letters,by="Ecotope")  %>% 
bind_rows(Stat_test_STA34_wet_yr1_data %>% group_by(Ecotope) %>% summarise(Median=Median(TPO4,na.rm=T),IQR_25=quantile(TPO4,.25),IQR_75=quantile(TPO4,.75)) %>% mutate(Treatment="Wet Season Year 1") %>%
left_join(STA34_wet_yr1_data_post_hoc_table_letters,by="Ecotope") ) %>%
bind_rows(Stat_test_STA34_dry_yr1_data %>% group_by(Ecotope) %>% summarise(Median=Median(TPO4,na.rm=T),IQR_25=quantile(TPO4,.25),IQR_75=quantile(TPO4,.75)) %>% mutate(Treatment="Dry Season Year 1") %>%
left_join(STA34_dry_yr1_data_post_hoc_table_letters,by="Ecotope")  ) %>%
bind_rows(Stat_test_STA34_dry_yr2_data %>% group_by(Ecotope) %>% summarise(Median=Median(TPO4,na.rm=T),IQR_25=quantile(TPO4,.25),IQR_75=quantile(TPO4,.75)) %>% mutate(Treatment="Dry Season Year 2") %>%
left_join(STA34_dry_yr2_data_post_hoc_table_letters,by="Ecotope")  ) %>%
bind_rows(Stat_test_STA34_data %>% group_by(Ecotope) %>% summarise(Median=Median(TPO4,na.rm=T),IQR_25=quantile(TPO4,.25),IQR_75=quantile(TPO4,.75)) %>% mutate(Treatment="Period of Record") %>%
left_join(Stat_test_STA34_data_post_hoc_table_letters,by="Ecotope")  )


WQ_Stat_Sig_Data <- Stat_test_STA34_wet_yr2_data %>% mutate(Treatment="Wet Season Year 2") %>%
bind_rows(Stat_test_STA34_wet_yr1_data %>% mutate(Treatment="Wet Season Year 1"))  %>%
bind_rows(Stat_test_STA34_dry_yr1_data %>% mutate(Treatment="Dry Season Year 1"))  %>%
bind_rows(Stat_test_STA34_dry_yr2_data %>% mutate(Treatment="Dry Season Year 2"))  %>%
bind_rows(Stat_test_STA34_data %>% mutate(Treatment="Period of Record"))

write.csv(WQ_Stat_Sig,"./Data/WQ Data/Water Quality Stats.csv",row.names = FALSE)  
write.csv(WQ_Stat_Sig_Data,"./Data/WQ Data/Water Quality Stats Data.csv",row.names = FALSE)  

