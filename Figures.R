rm(list = ls())


#remotes::install_github("cttobin/ggthemr",force = TRUE)
#library(dplyr)
#library(ggplot2)
#library(stringr)
#library(lubridate)
library(scales)
library(RColorBrewer)
library(viridis)
library(Hmisc)
library(ggpmisc)
library(ggrepel)
library(zoo)
library(readxl)
#library(readr)
library(corrplot)
library(GGally)
library(devtools)
library(ggthemr)
library(interp)
library(tidyverse)
library(stringr)
library(ggrepel)
library(cowplot)
library(hues)

# Import Data -------------------------------------------------------------


WQ_Upstream_Downstream_Tidy <- read_csv("./Data/WQ Data/WQ_Upstream_Downstream_Tidy.csv")
WQ_Data_Tidy <- read_csv("./Data/WQ Data/WQ_Data_Tidy.csv")
#WQ_Data_Tidy <- read_csv("./Data/WQ Data/WQ_Provisional_Tidy.csv")   #provisional data
WQ_Data <- read_excel("Data/WQ Data/WQ Data.xlsx",sheet = "Sheet1")
#WQ_Field_Data_Continuous_data <- read.csv("Data/Joined Data/WQ_Field_Data_Continuous_data.csv",check.names=FALSE)
WQ_Field_with_continuous_same_rows <- read.csv("./Data/Joined Data/WQ_Field_with_continuous_same_rows.csv",check.names=FALSE)
WQ_Field_Data <- read_csv("./Data/Joined Data/WQ_Field_Data.csv")
TP_Budget <- read_csv("./Data/P Budget/TP_Budget.csv")
TP_Budget_Daily_Combined <- read_csv("./Data/P Budget/TP_Budget_Daily_Combined.csv")
FWM_Weekly<- read_csv("./Data/P Budget/FWM_Weekly.csv")  #needs update
WQ_and_Spatial <- read_csv("./Data/Spatial Data/WQ_and_Spatial.csv") #needed for veg maps
WQ_and_Spatial_long <-read_csv("./Data/Spatial Data/WQ_and_Spatial_long.csv") #needed for veg maps
Soils_data <- read_csv("./Data/Soils Data/Soils_Data_Tidy.csv") #needed for soils figures
Soils_Summary_Stat_Sig <- read_csv( "./Data/Soils Data/Soils_Summary_Stat_Sig.csv")
All_light_data <- read_csv("Data/HOBO/All_light_data.csv")
All_Sonde_long <- read_csv("Data/Sonde/All_Sonde_long.csv")
All_Sonde_wide <- read_csv("Data/Sonde/All_Sonde_wide.csv")
Veg_tidy_long <- read_csv("Data/Vegetation Change/Bare_tidy_long.csv")

# Theme -------------------------------------------------------------------

#ggthemr("flat dark",type="outer", layout="scientific")   #used for presentation figs
ggthemr("light",type="outer", layout="scientific")  #used for SFER figs
Presentation_theme <- theme(strip.text = element_text(size=20) ,legend.position="bottom",axis.text=element_text(size=16),axis.title = element_text(size = 20),legend.text = element_text(size = 24),legend.title = element_text(size = 20))
Presentation_theme2 <- theme( strip.text = element_text(size=20) ,legend.position="bottom",axis.text=element_text(size=14),axis.title = element_text(size = 16),legend.text = element_text(size = 20),legend.title = element_text(size = 20))

# QC Blank Evaluation -----------------------------------------------------

QC_Blanks_Tidy <-WQ_Data  %>%
filter(SAMPLE_TYPE=="FCEB") %>%
group_by(TEST_NAME,REMARK_CODE) %>%
summarise(n=n())



# Evaluation of Samples with high TP  -------------------------------------
#look for possible causes of TP samples from field notes

High_TP_causes <-WQ_Field_with_continuous_same_rows  %>%
select(1:35,64:87) %>%
mutate(Date=as.Date(Date)) %>%  
mutate(`Possible Cause`=case_when(str_detect(tolower(Notes),"alg")==TRUE ~"Algae",
                                  str_detect(tolower(Notes),"bird")==TRUE ~"Bird",
                                  str_detect(tolower(Notes),"particle")==TRUE ~"Particles",
                                  str_detect(tolower(Notes),"ash")==TRUE ~"Ash",
                                  TRUE~"")) 

Summary_possible_causes <- High_TP_causes %>%
group_by(`Possible Cause`)  %>%
summarise(n(),`Mean TP`=mean(TPO4,na.rm=TRUE),SD=sd(TPO4,na.rm=TRUE))


ggplot(filter(High_TP_causes ,Ecotope!="Naiad",STA=="STA-3/4 Cell 2B"),aes(Date,TPO4,color=Ecotope,fill=Ecotope,label=`Possible Cause`))+geom_point()+
geom_label_repel(aes(Date,TPO4, label=`Possible Cause`),color="black",fill="white")+
Presentation_theme+scale_shape_manual(values = c(21:24)) + #scale_y_continuous(breaks=seq(0,100,10),limits=c(0,100))+
scale_x_date(date_breaks="1 month",labels = date_format("%b %y"))+guides(x =  guide_axis(angle = 40))+labs(y=expression(TP~(mu~g~L^-1)))

ggsave(plot = last_plot(),filename="./Figures/Potential sources of Variance.jpeg",width =13.333, height =7.5, units = "in")


# Contamination Evaluation ------------------------------------------------

Sample_Remarks_Code_Tidy <-WQ_Data  %>%
filter(SAMPLE_TYPE=="SAMP") %>%
group_by(TEST_NAME,REMARK_CODE) %>%
summarise(n=n())

Sample_counts <-WQ_Data  %>%
filter(SAMPLE_TYPE=="SAMP") %>%
group_by(TEST_NAME) %>%
summarise(n=n())

# TP over time -------------------------------------------------------------

#TPO4 Concentration over time points and smooth (STA1W and STA34) SFER 2023 Fig 1
WQ_Fig_data <- WQ_Field_Data %>%
mutate(`Figure Label Date`=case_when(STA=="STA-3/4 Cell 2B" & Date <"2021-09-15"~make_date(year=year(Date)+2,month=month(Date),day=day(Date)),
                                STA=="STA-3/4 Cell 2B" & Date >="2021-09-15" & Date <"2022-07-01"~make_date(year=year(Date)+1,month=month(Date),day=day(Date)),
                                STA=="STA-3/4 Cell 2B" & Date >"2022-07-01"~make_date(year=year(Date),month=month(Date),day=day(Date)),
                                STA=="STA-1W Cell 5B"~make_date(year=year(Date),month=month(Date),day=day(Date)))) %>%
mutate(`Study Period`=case_when(STA=="STA-3/4 Cell 2B" & Date <"2022-07-01"~"Year 1: STA-3/4",
                                STA=="STA-3/4 Cell 2B" & Date >"2022-07-01"~"Year 2: STA-3/4",
                                STA=="STA-1W Cell 5B"~"Year 2: STA-1W"))  

#TP time series STA34
TPO4_over_tim_STA34 <-ggplot(filter(WQ_Fig_data,Position=="Downstream",Ecotope!="Naiad",STA=="STA-3/4 Cell 2B"),aes(`Figure Label Date`,TPO4*1000,color=Ecotope,fill=Ecotope,linetype=Ecotope))+
geom_point(aes(`Figure Label Date`,TPO4*1000,color=Ecotope,fill=Ecotope),size=3)+
geom_smooth(se=FALSE)+facet_wrap(~factor(`Study Period`,levels = c("Year 1: STA-3/4","Year 2: STA-3/4","Year 2: STA-1W")),nrow=3,scales = "free_y")+
geom_hline(aes(yintercept = 13),color="#785d37",linetype="longdash")+ 
scale_shape_manual(values = c(21:24)) +#coord_cartesian(ylim=c(0,80))+#scale_y_continuous(breaks=seq(0,100,10),limits=c(0,100))+
scale_x_date(date_breaks="1 month",labels = date_format("%b"))+ 
scale_color_discrete("Ecotope", breaks = c("Bare","Chara","Mixed","Typha"),labels = c("Bare", expression(italic("Chara")),"Mixed",expression(italic("Typha"))))+  
Presentation_theme+  guides(x =  guide_axis(angle = 40),linetype="none",fill="none",color="none")+labs(y=NULL,x=NULL)##labs(y=expression(TP~(mu~g~L^-1)),x="")

ggsave(plot = last_plot(),filename="./Figures/TPO4 over time- STA34 -SFER 2024.jpeg",width =8, height =6, units = "in")

#TP time series STA1W
TPO4_over_tim_STA1W <- ggplot(filter(WQ_Fig_data,Position=="Downstream",Ecotope!="Naiad",STA=="STA-1W Cell 5B"),aes(`Figure Label Date`,TPO4*1000,color=Ecotope,fill=Ecotope,linetype=Ecotope))+
geom_point(aes(`Figure Label Date`,TPO4*1000,color=Ecotope,fill=Ecotope),size=3)+
geom_smooth(se=FALSE,alpha=.3)+facet_wrap(~factor(`Study Period`,levels = c("Year 1: STA-3/4","Year 2: STA-3/4","Year 2: STA-1W")),nrow=3,scales = "free_y")+
geom_hline(aes(yintercept = 13),color="#785d37",linetype="longdash")+
scale_shape_manual(values = c(21:24))+scale_y_continuous(breaks=seq(0,100,10),limits=c(0,100))+#coord_cartesian(ylim=c(10,100))
scale_x_date(date_breaks="1 month",labels = date_format("%b"))+ 
scale_color_discrete("Ecotope", breaks = c("Bare","Chara","Mixed","Typha"),labels = c("Bare", expression(italic("Chara")),"Mixed",expression(italic("Typha"))))+  
Presentation_theme+  guides(x =  guide_axis(angle = 40),linetype="none",fill="none")+labs(y=NULL,x=NULL)##+labs(y=expression(TP~(mu~g~L^-1)),x="")

ggsave(plot = last_plot(),filename="./Figures/TPO4 over time- STA1W -SFER 2024.jpeg",width =8, height =8, units = "in")

plot_grid(TPO4_over_tim_STA34, TPO4_over_tim_STA1W, ncol=1,rel_heights=c(4.5,3),axis=c("l"),label_y="tst")

ggsave(plot = last_plot(),filename="./Figures/TPO4 over time-SFER 2024.jpeg",width =8, height =8, units = "in")


#TPO4 Concentration over time points and smooth ( STA34)
ggplot(filter(WQ_Data_Tidy,Position=="Downstream",TEST_NAME=="TPO4",Ecotope!="Naiad",STA=="STA-3/4 Cell 2B"),aes(Date,VALUE*1000,color=Ecotope,fill=Ecotope,linetype=Ecotope))+
geom_point(aes(Date,VALUE*1000,color=Ecotope,fill=Ecotope),size=4)+
#geom_smooth(se=FALSE)+
facet_wrap(~STA,nrow=2)+geom_hline(aes(yintercept = 13),color="white",linetype="dashed")+
scale_shape_manual(values = c(21:24)) + #scale_y_continuous(breaks=seq(0,100,10),limits=c(0,100))+Presentation_theme+
scale_x_date(date_breaks="1 month",labels = date_format("%b %y"))+guides(x =  guide_axis(angle = 40))+labs(y=expression(TP~(mu~g~L^-1)))

ggsave(plot = last_plot(),filename="./Figures/TPO4 over time-flat dark- STA34 only WY22.jpeg",width =13.333, height =7.5, units = "in")



# Is TP data normally distributed? -----------------------------------------

#all downstream data density distribution
ggplot(filter(WQ_Field_Data,Position=="Downstream"),aes(TPO4))+geom_density()+
guides(x =  guide_axis(angle = 40))+labs(y=expression(TP~(mu~g~L^-1)))

#all downstream data QQplots+geom_qq()+stat_qq_line()
ggplot(filter(WQ_Field_Data,Position=="Downstream"),aes(sample=TPO4))+geom_qq()+stat_qq_line()+
guides(x =  guide_axis(angle = 40))+labs(y=expression(TP~(mu~g~L^-1)))

#test for normality
shapiro.test(filter(WQ_Field_Data,Position=="Downstream")$TPO4)

#Density plot of distributions by ecotope 
ggplot(filter(WQ_Field_Data,Position=="Downstream"),aes(TPO4,color=Ecotope,fill=Ecotope))+geom_density()+facet_wrap(STA~Ecotope,scale="free")+
guides(x =  guide_axis(angle = 40))+labs(y=expression(TP~(mu~g~L^-1)))

#qq plots of Raw data
ggplot(filter(WQ_Field_Data,Position=="Downstream"),aes(sample=TPO4,color=Ecotope,fill=Ecotope))+geom_qq()+stat_qq_line()+facet_wrap(STA~Ecotope,scale="free")+
guides(x =  guide_axis(angle = 40))+labs(y=expression(TP~(mu~g~L^-1)))


#Histogram of distributions  -- Distributions approximately log normal
ggplot(filter(WQ_Field_Data,Position=="Downstream"),aes(log(TPO4*1000),color=Ecotope,fill=Ecotope))+geom_density()+facet_wrap(STA~Ecotope,scale="free")+
guides(x =  guide_axis(angle = 40))+labs(y=expression(TP~(mu~g~L^-1)))

#qq plots of log transformed data
ggplot(filter(WQ_Field_Data,Position=="Downstream"),aes(sample=log(TPO4),color=Ecotope,fill=Ecotope))+geom_qq()+stat_qq_line()+facet_wrap(STA~Ecotope,scale="free")+
guides(x =  guide_axis(angle = 40))+labs(y=expression(TP~(mu~g~L^-1)))

#Data has log normal distribution

# Analytes other than P ---------------------------------------------------

#Potassium time series
ggplot(filter(WQ_Fig_data,Position=="Downstream",Ecotope!="Naiad",STA=="STA-3/4 Cell 2B"),aes(`Figure Label Date`,K*1000,color=Ecotope,fill=Ecotope,linetype=Ecotope))+
geom_point(aes(`Figure Label Date`,K*1000,color=Ecotope,fill=Ecotope),size=3)+
geom_smooth(se=FALSE)+facet_wrap(~factor(`Study Period`,levels = c("Year 1: STA-3/4","Year 2: STA-3/4","Year 2: STA-1W")),nrow=3,scales = "free_y")+
scale_shape_manual(values = c(21:24)) +#coord_cartesian(ylim=c(0,80))+#scale_y_continuous(breaks=seq(0,100,10),limits=c(0,100))+
scale_x_date(date_breaks="1 month",labels = date_format("%b"))+ 
scale_color_discrete("Ecotope", breaks = c("Bare","Chara","Mixed","Typha"),labels = c("Bare", expression(italic("Chara")),"Mixed",expression(italic("Typha"))))+  
Presentation_theme+  guides(x =  guide_axis(angle = 40),linetype="none",fill="none",color="none")+labs(y=expression(K~(mu~g~L^-1)),x="")

#Alkalinity time series
ggplot(filter(WQ_Fig_data,Position=="Downstream",Ecotope!="Naiad",STA=="STA-3/4 Cell 2B"),aes(`Figure Label Date`,ALKA*1000,color=Ecotope,fill=Ecotope,linetype=Ecotope))+
geom_point(aes(`Figure Label Date`,K*1000,color=Ecotope,fill=Ecotope),size=3)+
geom_smooth(se=FALSE)+facet_wrap(~factor(`Study Period`,levels = c("Year 1: STA-3/4","Year 2: STA-3/4","Year 2: STA-1W")),nrow=3,scales = "free_y")+
scale_shape_manual(values = c(21:24)) +#coord_cartesian(ylim=c(0,80))+#scale_y_continuous(breaks=seq(0,100,10),limits=c(0,100))+
scale_x_date(date_breaks="1 month",labels = date_format("%b"))+ 
scale_color_discrete("Ecotope", breaks = c("Bare","Chara","Mixed","Typha"),labels = c("Bare", expression(italic("Chara")),"Mixed",expression(italic("Typha"))))+  
Presentation_theme+  guides(x =  guide_axis(angle = 40),linetype="none",fill="none")+labs(y=expression(K~(mu~g~L^-1)),x="")

#Potassium vs TP scatterplot
ggplot(filter(WQ_Fig_data,Position=="Downstream",Ecotope!="Naiad",STA=="STA-3/4 Cell 2B"),aes(TPO4*1000,K*1000,color=Ecotope,fill=Ecotope,linetype=Ecotope))+
geom_point(aes(TPO4*1000,K*1000,color=Ecotope,fill=Ecotope),size=3)+
geom_smooth(method="lm",se=FALSE)+#facet_wrap(~factor(`Study Period`,levels = c("Year 1: STA-3/4","Year 2: STA-3/4","Year 2: STA-1W")),nrow=3,scales = "free_y")+
scale_shape_manual(values = c(21:24)) +#coord_cartesian(ylim=c(0,80))+#scale_y_continuous(breaks=seq(0,100,10),limits=c(0,100))+
scale_color_discrete("Ecotope", breaks = c("Bare","Chara","Mixed","Typha"),labels = c("Bare", expression(italic("Chara")),"Mixed",expression(italic("Typha"))))+  
Presentation_theme+  guides(x =  guide_axis(angle = 40),linetype="none",fill="none",color="none")+labs(y=NULL,x=NULL)##labs(y=expression(TP~(mu~g~L^-1)),x="")

#DOC time series
ggplot(filter(WQ_Fig_data,Position=="Downstream",Ecotope!="Naiad",STA=="STA-3/4 Cell 2B"),aes(`Figure Label Date`,DOC,color=Ecotope,fill=Ecotope,linetype=Ecotope))+
geom_point(aes(`Figure Label Date`,DOC,color=Ecotope,fill=Ecotope),size=3)+
geom_smooth(se=FALSE)+facet_wrap(~factor(`Study Period`,levels = c("Year 1: STA-3/4","Year 2: STA-3/4","Year 2: STA-1W")),nrow=3,scales = "free_y")+
scale_shape_manual(values = c(21:24)) +#coord_cartesian(ylim=c(0,80))+#scale_y_continuous(breaks=seq(0,100,10),limits=c(0,100))+
scale_x_date(date_breaks="1 month",labels = date_format("%b"))+ 
scale_color_discrete("Ecotope", breaks = c("Bare","Chara","Mixed","Typha"),labels = c("Bare", expression(italic("Chara")),"Mixed",expression(italic("Typha"))))+  
Presentation_theme+  guides(x =  guide_axis(angle = 40),linetype="none",fill="none",color="none")+labs(y=expression(DOC~(m~g~L^-1)),x="")

#TN and NH4 time series
ggplot(filter(WQ_Fig_data,Position=="Downstream",Ecotope!="Naiad",STA=="STA-3/4 Cell 2B"),aes(`Figure Label Date`,TN,color=Ecotope,fill=Ecotope,linetype=Ecotope))+
geom_point(size=3)+geom_point(aes(`Figure Label Date`,NH4,color=Ecotope,fill=Ecotope),size=3,shape=24)+
geom_smooth(se=FALSE)+geom_smooth(aes(`Figure Label Date`,NH4,color=Ecotope,fill=Ecotope),se=F)+
facet_wrap(~factor(`Study Period`,levels = c("Year 1: STA-3/4","Year 2: STA-3/4","Year 2: STA-1W")),nrow=3,scales = "free_y")+
scale_shape_manual(values = c(21:24)) +#coord_cartesian(ylim=c(0,80))+#scale_y_continuous(breaks=seq(0,100,10),limits=c(0,100))+
scale_x_date(date_breaks="1 month",labels = date_format("%b"))+ 
scale_color_discrete("Ecotope", breaks = c("Bare","Chara","Mixed","Typha"),labels = c("Bare", expression(italic("Chara")),"Mixed",expression(italic("Typha"))))+  
Presentation_theme+  guides(x =  guide_axis(angle = 40),linetype="none",fill="none",color="none")+labs(y=expression(TN~(m~g~L^-1)),x="")

#TN to NH4 ratio
filter(WQ_Fig_data,Position=="Downstream",Ecotope!="Naiad",STA=="STA-3/4 Cell 2B") %>%
mutate(`TN:NH4`=TN/NH4,`TN:NH4+NOX`=TN/(NH4+NOX)) %>%
summarise(n(),obs=sum(is.finite(`TN:NH4`)),`Mean TN`=mean(TN,na.rm=T),`SD TN`=sd(TN,na.rm=T),`Mean NH4`=mean(NH4,na.rm=T),`SD NH4`=sd(NH4,na.rm=T),
`Mean TN:NH4`=mean(`TN:NH4`,na.rm=T),`SD TN:NH4`=sd(`TN:NH4`,na.rm=T),`Mean TN:NH4+NOX`=mean(`TN:NH4+NOX`,na.rm=T),`SD TN:NH4+NOX`=sd(`TN:NH4+NOX`,na.rm=T))

#TN vs TP scatterplot
ggplot(filter(WQ_Fig_data,Position=="Downstream",Ecotope!="Naiad",STA=="STA-3/4 Cell 2B"),aes(TPO4*1000,TN*1000,color=Ecotope,fill=Ecotope,linetype=Ecotope))+
geom_point(aes(TPO4*1000,TN*1000,color=Ecotope,fill=Ecotope),size=3)+
geom_smooth(method="lm",se=T)+#facet_wrap(~factor(`Study Period`,levels = c("Year 1: STA-3/4","Year 2: STA-3/4","Year 2: STA-1W")),nrow=3,scales = "free_y")+
scale_shape_manual(values = c(21:24)) +#coord_cartesian(ylim=c(0,80))+#scale_y_continuous(breaks=seq(0,100,10),limits=c(0,100))+
scale_color_discrete("Ecotope", breaks = c("Bare","Chara","Mixed","Typha"),labels = c("Bare", expression(italic("Chara")),"Mixed",expression(italic("Typha"))))+  
Presentation_theme+  guides(x =  guide_axis(angle = 40),linetype="none",fill="none")+labs(y=expression(TN~(mu~g~L^-1)),x=expression(TP~(mu~g~L^-1)))

# WQ SFER Tables -------------------------------------------------------------

#SFER table summary
Annual_TP <- WQ_Fig_data %>%
filter(Ecotope!="Naiad") %>%  
group_by(STA,`Study Period`,Ecotope) %>% #remove study period for POR averages
#filter(Date>"2022-06-01")  %>%
filter(Position=="Downstream") %>%
summarise(Samples=sum(!is.na(TPO4)),`Annual Mean TP`=mean(TPO4,na.rm=T)*1000,`SD`=sd(TPO4,na.rm=T)*1000,`SE`=(sd(TPO4,na.rm=T)/Samples^.5)*1000)

#Dry vs wets season table 
Seasonal_TP_Summary <- WQ_Fig_data %>%
filter(Position=="Downstream",Ecotope!="Naiad") %>%
mutate(Month=month(Date),Season=if_else(between(Month,6,11),"Wet Season","Dry Season")) %>%
group_by(STA,`Study Period`,Ecotope,Season) %>% #remove study period for POR averages
summarise(Samples=sum(!is.na(TPO4)),`Mean TP`=mean(TPO4,na.rm=T)*1000,`SD`=sd(TPO4,na.rm=T)*1000,`SE`=(sd(TPO4,na.rm=T)/Samples^.5)*1000) %>%
pivot_wider(names_from="Season",values_from=c("Samples","Mean TP","SD","SE")) %>%
left_join(Annual_TP, by=c("STA","Ecotope","Study Period")) %>%
mutate(across(where(is.numeric) , ~round(.x,digits=1)))  %>%
mutate(`Dry Season TP ± SD (n)`=paste(`Mean TP_Dry Season`,"±",`SD_Dry Season`," (",`Samples_Dry Season`,")"),`Wet Season TP ± SD (n)`=paste(`Mean TP_Wet Season`,"±",`SD_Wet Season`," (",`Samples_Wet Season`,")"),`Annual TP ± SD (n)`=paste(`Annual Mean TP`,"±",`SD`," (",`Samples`,")")) %>%
mutate(`Dry Season TP ± SE (n)`=paste(`Mean TP_Dry Season`,"±",`SE_Dry Season`," (",`Samples_Dry Season`,")"),`Wet Season TP ± SE (n)`=paste(`Mean TP_Wet Season`,"±",`SE_Wet Season`," (",`Samples_Wet Season`,")"),`Annual TP ± SE (n)`=paste(`Annual Mean TP`,"±",`SE`," (",`Samples`,")")) %>%
arrange(Ecotope)  

write.csv(Seasonal_TP_Summary ,"./Data/Publish Tables/Seasonal_TP_Summary.csv",row.names = FALSE) #SFER table

write.csv(Seasonal_TP_Summary ,"./Data/Publish Tables/Seasonal_TP_Summary_by_year.csv",row.names = FALSE) #SFER table


# Water depth vs TP -----------------------------------------------------

#Depth over time with TP differences
ggplot(select(WQ_Field_Data_Continuous_data,`Average DCS (Field Data)`,`DCS Levelogger`,`Date Time`,Position,Ecotope,`Dif TPO4`),aes(ymd_hms(`Date Time`),`Average DCS (Field Data)`,fill=Position))+geom_point(shape=21,size=2)+
geom_line(aes(ymd_hms(`Date Time`),`DCS Levelogger`*100))+
geom_line(aes(ymd_hms(`Date Time`),`Dif TPO4`*10000),color="red")+ 
facet_wrap(~Ecotope)+
scale_x_datetime(date_breaks="1 month",labels = date_format("%b %y"),limits = as.POSIXct(c("2021-06-01 00:00:00","2022-05-31 00:00:00")))+
scale_fill_brewer(palette = "Set2",direction = -1)+scale_color_brewer(palette = "Set2",direction = -1)+theme_bw()

#DCS Depth vs TP 
ggplot(filter(WQ_Field_Data ,Position=="Downstream",Ecotope!="Naiad"),aes(`DCS (Field Data)`,`TPO4`*1000,fill=Ecotope,color=Ecotope))+
facet_grid(STA~Ecotope,scales="free")+scale_y_continuous(breaks=seq(0,80,10))+Presentation_theme2+coord_cartesian(ylim=c(0,80))+
geom_rect(aes(xmin = 45.72, ymin = -Inf, xmax = 76.2, ymax = Inf),alpha=.5,fill="#e0ecf4",color="#e0ecf4")+geom_point(shape=21,size=2.5,color="grey70")+geom_smooth()+
ylab(expression(TP~(mu~g~L^-1)))+xlab("Depth to Consolidated Substrate (cm)")+guides(fill="none",color="none")

ggsave(plot = last_plot(),filename="./Figures/TPO4 vs Water Depth 2024 SFER.jpeg",width =8, height =6, units = "in")

#Flow vs TP wet seaon and dry season
Wet_vs_dry <- WQ_Field_with_continuous_same_rows %>%
mutate(Season=if_else(between(month(Date),6,11)==TRUE,"Wet Season","Dry Season"))  %>%
mutate(Ecotope=factor(Ecotope,labels = c("Bare","italic(Chara)","Mixed","Naiad","italic(Typha)")))

#DCS Depth vs TP wet season vs dry season STA34
ggplot(filter(Wet_vs_dry,Position=="Downstream",Ecotope!="Naiad",STA=="STA-3/4 Cell 2B"),aes(`DCS (Field Data)`,`TPO4`*1000,fill=Ecotope,color=Ecotope))+
facet_grid(Season~Ecotope,labeller = labeller(.rows = label_value, .cols = label_parsed))+scale_y_continuous(breaks=seq(0,80,10))+Presentation_theme2+coord_cartesian(ylim=c(0,50))+
geom_rect(aes(xmin = 45.72, ymin = -Inf, xmax = 76.2, ymax = 80),alpha=.5,fill="#e0ecf4",color="#e0ecf4")+
geom_point(shape=21,size=2.5,color="grey70")+geom_smooth(span=10)+
geom_hline(yintercept = 13,linetype="longdash",color="#785d37")+
ylab(expression(TP~(mu~g~L^-1)))+xlab("Depth to Consolidated Substrate (cm)")+guides(fill="none",color="none")

ggsave(plot = last_plot(),filename="./Figures/TPO4 vs Water Depth wet vs dry season STA34- 2024 SFER.jpeg",width =8, height =6, units = "in")

#DCS Depth vs TP wet season vs dry season STA-1W
ggplot(filter(Wet_vs_dry,Position=="Downstream",Ecotope!="Naiad",STA=="STA-1W Cell 5B"),aes(`DCS (Field Data)`,`TPO4`*1000,fill=Ecotope,color=Ecotope))+
facet_grid(Season~Ecotope,labeller = labeller(.rows = label_value, .cols = label_parsed))+scale_y_continuous(breaks=seq(0,80,10))+Presentation_theme2+coord_cartesian(ylim=c(0,80))+
geom_rect(aes(xmin = 45.72, ymin = -Inf, xmax = 76.2, ymax = Inf),alpha=.5,fill="#e0ecf4",color="#e0ecf4")+
geom_point(shape=21,size=2.5,color="grey70")+geom_smooth(span=10)+
geom_hline(yintercept = 13,linetype="longdash",color="#785d37")+
ylab(expression(TP~(mu~g~L^-1)))+xlab("Depth to Consolidated Substrate (cm)")+guides(fill="none",color="none")

ggsave(plot = last_plot(),filename="./Figures/TPO4 vs Water Depth wet vs dry season STA1W- 2024 SFER.jpeg",width =8, height =6, units = "in")


#DCS Depth vs Temp
ggplot(WQ_Field_Data_Continuous_data,aes(`Average DCS (Field Data)`,`Temp`))+geom_point(shape=21,size=2)+geom_smooth()+theme_bw()+
facet_wrap(~Ecotope)+scale_y_continuous(breaks=seq(10,50,5),limits=c(10,50))+
scale_fill_brewer(palette = "Set2",direction = -1)+scale_color_brewer(palette = "Set2",direction = -1)

Temp_DF <-  mutate(WQ_Field_with_continuous_same_rows,`Temp Category`=case_when(is.na(`Temp`)~"No data",
                                                                                `Temp`<20~"Less than 20C",
                                                                                between(`Temp`,20,30)~"20-30C",
                                                                                `Temp`>30~"Greater than 30C"))
#DCS depth vs TP for various temp categories
ggplot(Temp_DF,aes(`DCS (Field Data)`,`TPO4`,fill=Ecotope))+
facet_wrap(~`Temp Category`)+scale_y_continuous(breaks=seq(0,.03,0.005),limits=c(0,.03))+geom_rect(aes(xmin = 45.72, ymin = 0, xmax = 76.2, ymax = .03),alpha=.5,fill="#99d8c9")+ 
geom_point(shape=21,size=2)+geom_smooth(fill="grey30",method="lm")+
scale_fill_brewer(palette = "Set2",direction = -1)+scale_color_brewer(palette = "Set2",direction = -1)+theme_bw()


#Depth vs TP for 
Flowing_DF <-  mutate(WQ_Field_with_continuous_same_rows,Flow=case_when(is.na(`Mean inflow (cfs)`)~"No data",
                                                                   `Mean inflow (cfs)`<10~"Less than 10 cfs",
                                                                   `Mean inflow (cfs)`>10~"10+ cfs"))

ggplot(Flowing_DF,aes(`DCS (Field Data)`,`TPO4`,fill=Ecotope))+
facet_wrap(~Flow)+scale_y_continuous(breaks=seq(0,.03,0.005),limits=c(0,.03))+geom_rect(aes(xmin = 45.72, ymin = 0, xmax = 76.2, ymax = .03),alpha=.5,fill="#99d8c9")+ 
geom_point(shape=21,size=2)+geom_smooth(fill="grey30",method="lm")+
scale_fill_brewer(palette = "Set2",direction = -1)+scale_color_brewer(palette = "Set2",direction = -1)+theme_bw()



# Wind plots --------------------------------------------------------------

#wind direction and strength on sampling days
ggplot(WQ_Field_with_continuous_same_rows,aes(`BELLE GL_WNVD_DEGREES CLOCKWI`,`BELLE GL_WNVS_MPH`,color=`BELLE GL_WNVS_MPH`))+geom_col()+
coord_polar()+scale_color_viridis(option = "D")+
scale_x_continuous(breaks=seq(0,315,45),limits=c(0,360))+theme_bw()

#wind direction and strength on all days
ggplot(WQ_Field_Data_Continuous_data,aes(`BELLE GL_WNVD_DEGREES CLOCKWI`,`BELLE GL_WNVS_MPH`,color=`BELLE GL_WNVS_MPH`))+geom_col()+
coord_polar()+scale_color_viridis(option = "D")+
scale_x_continuous(breaks=seq(0,315,45),limits=c(0,360))+theme_bw()

#Wind speed vs TP 
ggplot(WQ_Field_with_continuous_same_rows,aes(`BELLE GL_WNVS_MPH`,`TPO4`))+geom_point(shape=21,size=2)+geom_smooth()+
facet_wrap(~Ecotope)+#scale_y_continuous(breaks=seq(0,.03,0.005))+
scale_color_brewer(palette = "Set2",direction = -1)+theme_bw()

#Wind speed vs difference in TP 
ggplot(WQ_Field_with_continuous_same_rows,aes(`BELLE GL_WNVS_MPH`,`Dif TPO4`))+geom_point(shape=21,size=2)+geom_smooth()+
facet_wrap(~Ecotope)+#scale_y_continuous(breaks=seq(0,.03,0.005))+
scale_color_brewer(palette = "Set2",direction = -1)+theme_bw()

#Wind direction vs TP 
ggplot(WQ_Field_with_continuous_same_rows,aes(`BELLE GL_WNVD_DEGREES CLOCKWI`,`TPO4`,color=TPO4))+geom_col()+#geom_smooth()+
facet_wrap(~Ecotope)+coord_polar()+
scale_color_viridis(option = "D",direction = -1)+
scale_x_continuous(breaks=seq(0,315,45),limits=c(0,360))+theme_bw()

#Wind direction vs difference in TP  
ggplot(WQ_Field_with_continuous_same_rows,aes(`BELLE GL_WNVD_DEGREES CLOCKWI`,`Dif TPO4`,color=abs(`Dif TPO4`)))+geom_col()+#geom_smooth()+
facet_wrap(~Ecotope)+coord_polar()+
scale_color_viridis(option = "D",direction = -1)+
scale_x_continuous(breaks=seq(0,315,45),limits=c(0,360))+theme_bw()


# Flow vs TP  ------------------------------------------------------------
#inflow
ggplot(WQ_Field_Data_Continuous_data,aes(ymd_hms(`Date Time`),TPO4*100000,fill=Ecotope))+
geom_line(aes(ymd_hms(`Date Time`),`Mean inflow (cfs)`),color="blue")+
geom_line(aes(ymd_hms(`Date Time`),`Mean outflow (cfs)`),color="orange")+ 
geom_point(shape=21,size=2)+ 
geom_path()+
facet_wrap(~Ecotope)+
scale_x_datetime(date_breaks="1 month",labels = date_format("%b %y"),limits = as.POSIXct(c("2021-06-01 00:00:00","2022-03-01 00:00:00")))+coord_cartesian(ylim=c(-800,2000))+
scale_fill_brewer(palette = "Set2",direction = -1)+scale_color_brewer(palette = "Set2",direction = -1)+theme_bw()

#inflow vs TPO4
ggplot(filter(WQ_Field_with_continuous_same_rows,Position=="Downstream",Ecotope!="Naiad",Date<"2023-07-02"),aes(`Mean inflow (cfs)`,`TPO4`*1000,fill=Ecotope,color=Ecotope))+geom_point(shape=21,size=2,color="grey70")+
facet_grid(STA~Ecotope,scales="free_y")+Presentation_theme2+#coord_cartesian(ylim=c(0,80))+
geom_ribbon(stat='smooth', method = "loess", se=TRUE, alpha=0.3,span=3) +
geom_line(stat='smooth', method = "loess")  + 
ylab(expression(TP~(mu~g~L^-1)))+xlab("Outflow (cfs)")+guides(fill="none",color="none")

#outflow vs TPO4 in STA3/4
WQ_Field_with_continuous_same_rows_new_labels <- WQ_Field_with_continuous_same_rows %>%
mutate(Ecotope=factor(Ecotope,labels = c("Bare","italic(Chara)","Mixed","Naiad","italic(Typha)")))
  
ggplot(filter(WQ_Field_with_continuous_same_rows_new_labels,Position=="Downstream",STA=="STA-3/4 Cell 2B",Ecotope!="Naiad",Date<"2023-07-02"),aes(`Mean outflow (cfs)`,`TPO4`*1000,fill=Ecotope,color=Ecotope))+geom_point(shape=21,size=2.5,color="grey70")+
facet_wrap(~Ecotope,scales="free_y",labeller=label_parsed,nrow=1)+scale_y_continuous(breaks=seq(0,100,10))+Presentation_theme2+scale_x_continuous(breaks=seq(0,1200,200))+
geom_ribbon(stat='smooth', method = "loess", se=TRUE, alpha=0.3,span=1) +coord_cartesian(ylim=c(0,50),xlim=c(50,1300))+
geom_line(stat='smooth', method = "loess",span=1)  + geom_hline(yintercept = 13,linetype="longdash",color="#785d37")+
ylab(expression(TP~(mu~g~L^-1)))+xlab("Flow (cfs)")+guides(x =  guide_axis(angle = 40),fill="none",color="none")

ggsave(plot = last_plot(),filename="./Figures/TPO4 vs Outflow 2024 SFER.jpeg",width =8, height =5, units = "in")

#Flow vs TP wet seaon and dry season
Wet_vs_dry <- WQ_Field_with_continuous_same_rows %>%
mutate(Season=if_else(between(month(Date),6,11)==TRUE,"Wet Season","Dry Season"))  %>%
mutate(Ecotope=factor(Ecotope,labels = c("Bare","italic(Chara)","Mixed","Naiad","italic(Typha)")))

ggplot(filter(Wet_vs_dry,Position=="Downstream",Ecotope!="Naiad",STA=="STA-3/4 Cell 2B",Date<"2023-07-02"),aes(`Mean outflow (cfs)`,`TPO4`*1000,fill=Ecotope,color=Ecotope))+geom_point(shape=21,size=2.5,color="grey70")+
facet_grid(Season~Ecotope,labeller = labeller(.rows = label_value, .cols = label_parsed))+scale_y_continuous(breaks=seq(0,100,10))+Presentation_theme2+scale_x_continuous(breaks=seq(0,1200,200))+
geom_ribbon(stat='smooth', method = "loess", se=TRUE, alpha=0.3,span=1) +coord_cartesian(ylim=c(0,50),xlim=c(50,1300))+
geom_line(stat='smooth', method = "loess",span=1)  + geom_hline(yintercept = 13,linetype="longdash",color="#785d37")+
ylab(expression(TP~(mu~g~L^-1)))+xlab("Flow (cfs)")+guides(x =  guide_axis(angle = 40),fill="none",color="none")

ggsave(plot = last_plot(),filename="./Figures/TPO4 vs Outflow 2024 wet vs dry SFER.jpeg",width =8, height =7, units = "in")

#outflow vs TPO4 in STA1W (not enough data to use yet)
ggplot(filter(WQ_Field_with_continuous_same_rows,Position=="Downstream",Ecotope!="Naiad",STA=="STA-1W Cell 5B",Date<"2023-07-02"),aes(`Mean outflow (cfs)`,`TPO4`*1000,fill=Ecotope,color=Ecotope))+geom_point(shape=21,size=2,color="grey70")+
facet_wrap(~Ecotope)+scale_y_continuous(breaks=seq(0,100,10))+Presentation_theme2+scale_x_continuous(breaks=seq(0,1200,200))+
geom_ribbon(stat='smooth', method = "loess", se=TRUE, alpha=0.3) +coord_cartesian(ylim=c(0,80),xlim=c(50,1300))+
geom_line(stat='smooth', method = "loess")  + geom_hline(yintercept = 13,linetype="longdash",color="#785d37")+
ylab(expression(TP~(mu~g~L^-1)))+xlab("Outflow (cfs)")+guides(fill="none",color="none")

ggsave(plot = last_plot(),filename="./Figures/TPO4 vs Outflow 2023 SFER.jpeg",width =8, height =6, units = "in")


# Water Depth vs Flow and TP ----------------------------------------------
#Remove NAs
contour_data <- filter(WQ_Field_with_continuous_same_rows,Ecotope!="Naiad",STA=="STA-3/4 Cell 2B",Position=="Downstream",between(month(Date),5,11)) %>%
drop_na(TPO4) %>%
drop_na(`DCS (Field Data)`) %>%
drop_na(`Mean outflow (cfs)`) 

#Interpolate to grid from STA-3/4
contour_grid <- with(contour_data, interp::interp(`DCS (Field Data)`, `Mean outflow (cfs)`, TPO4*1000,duplicate="mean",method="linear"))
griddf <- subset(data.frame(`Depth` = rep(contour_grid$x, nrow(contour_grid$z)),`CFS`= rep(contour_grid$y, each = ncol(contour_grid$z)), z = as.numeric(contour_grid$z)),!is.na(z)) %>%
rename(`DCS (Field Data)`="Depth",`Mean outflow (cfs)`="CFS",TPO4="z")

ggplot(griddf, aes(`DCS (Field Data)`,`Mean outflow (cfs)` , z = TPO4)) +
geom_contour_filled(binwidth =3) +  xlab("Depth to Consolidated Substrate (cm)")+ylab("Flow (cfs)")+Presentation_theme+theme(legend.position="right")+
geom_point(data = contour_data, aes(`DCS (Field Data)`,`Mean outflow (cfs)`))+guides(fill=guide_legend(title=expression(TP~(mu~g~L^-1))))

ggsave(plot = last_plot(),filename="./Figures/TPO4 vs Flow vs Depth-SFER 2024.jpeg",width =8.5, height =11, units = "in")

ggsave(plot = last_plot(),filename="./Figures/TPO4 vs Flow vs Depth.jpeg",width =13.333, height =7.5, units = "in")

#figs for individual ecotopes and all STA3/4 during wetseason
contour_data_chara <- filter(WQ_Field_with_continuous_same_rows,Ecotope=="Chara",Position=="Downstream",STA=="STA-3/4 Cell 2B",between(month(Date),5,11),TPO4<.030) %>% drop_na(TPO4) %>% drop_na(`DCS (Field Data)`) %>% drop_na(`Mean outflow (cfs)`) 
contour_data_typha <- filter(WQ_Field_with_continuous_same_rows,Ecotope=="Typha",Position=="Downstream",STA=="STA-3/4 Cell 2B",between(month(Date),5,11),TPO4<.030) %>% drop_na(TPO4) %>% drop_na(`DCS (Field Data)`) %>% drop_na(`Mean outflow (cfs)`) 
contour_data_mixed <- filter(WQ_Field_with_continuous_same_rows,Ecotope=="Mixed",Position=="Downstream",STA=="STA-3/4 Cell 2B",between(month(Date),5,11),TPO4<.030) %>% drop_na(TPO4) %>% drop_na(`DCS (Field Data)`) %>% drop_na(`Mean outflow (cfs)`) 
contour_data_bare <- filter(WQ_Field_with_continuous_same_rows,Ecotope=="Bare",Position=="Downstream",STA=="STA-3/4 Cell 2B",between(month(Date),5,11),TPO4<.030) %>% drop_na(TPO4) %>% drop_na(`DCS (Field Data)`) %>% drop_na(`Mean outflow (cfs)`) 
contour_grid_chara <- with(contour_data_chara, interp::interp(`DCS (Field Data)`, `Mean outflow (cfs)`, TPO4*1000,duplicate="mean"))
contour_grid_typha <- with(contour_data_typha, interp::interp(`DCS (Field Data)`, `Mean outflow (cfs)`, TPO4*1000,duplicate="mean"))
contour_grid_mixed <- with(contour_data_mixed, interp::interp(`DCS (Field Data)`, `Mean outflow (cfs)`, TPO4*1000,duplicate="mean"))
contour_grid_bare <- with(contour_data_bare, interp::interp(`DCS (Field Data)`, `Mean outflow (cfs)`, TPO4*1000,duplicate="mean"))
griddf_chara <- subset(data.frame(`Depth` = rep(contour_grid_chara$x, nrow(contour_grid_chara$z)), `CFS`= rep(contour_grid_chara$y, each = ncol(contour_grid_chara$z)), z = as.numeric(contour_grid_chara$z)),!is.na(z)) %>%rename(`DCS (Field Data)`="Depth",`Mean outflow (cfs)`="CFS",TPO4="z")
griddf_typha <- subset(data.frame(`Depth` = rep(contour_grid_typha$x, nrow(contour_grid_typha$z)), `CFS`= rep(contour_grid_typha$y, each = ncol(contour_grid_typha$z)), z = as.numeric(contour_grid_typha$z)),!is.na(z)) %>%rename(`DCS (Field Data)`="Depth",`Mean outflow (cfs)`="CFS",TPO4="z")
griddf_mixed <- subset(data.frame(`Depth` = rep(contour_grid_mixed$x, nrow(contour_grid_mixed$z)), `CFS`= rep(contour_grid_mixed$y, each = ncol(contour_grid_mixed$z)), z = as.numeric(contour_grid_mixed$z)),!is.na(z)) %>%rename(`DCS (Field Data)`="Depth",`Mean outflow (cfs)`="CFS",TPO4="z")
griddf_bare <- subset(data.frame(`Depth` = rep(contour_grid_bare$x, nrow(contour_grid_bare$z)), `CFS`= rep(contour_grid_bare$y, each = ncol(contour_grid_bare$z)), z = as.numeric(contour_grid_bare$z)),!is.na(z)) %>%rename(`DCS (Field Data)`="Depth",`Mean outflow (cfs)`="CFS",TPO4="z")

all_grids <- mutate(griddf_chara,Ecotope="Chara") %>%
bind_rows(mutate(griddf_typha,Ecotope="Typha")) %>%
bind_rows(mutate(griddf_mixed,Ecotope="Mixed")) %>%
bind_rows(mutate(griddf_bare,Ecotope="Bare")) 

all_points <- filter(WQ_Field_with_continuous_same_rows,Position=="Downstream",STA=="STA-3/4 Cell 2B",Ecotope!="Naiad",between(month(Date),5,11),TPO4<.30)
  
ggplot(all_grids, aes(`DCS (Field Data)`,`Mean outflow (cfs)` , z = TPO4)) +
facet_wrap(~Ecotope)+
geom_contour_filled(binwidth =2) +  xlab("")+ylab("")+Presentation_theme+theme(legend.position="right")+
xlab("Depth to Consolidated Substrate (cm)")+ylab("Flow (cfs)")+
geom_point(data = all_points, aes(`DCS (Field Data)`,`Mean outflow (cfs)`))+guides(fill=guide_legend(title=expression(TP~(mu~g~L^-1))))

ggsave(plot = last_plot(),filename="./Figures/TPO4 vs Flow vs Depth by Ecotope-SFER 2024.jpeg",width =8.5, height =8, units = "in")

#Figs wet season vs dry season 
contour_data_wet <- filter(WQ_Field_with_continuous_same_rows,Ecotope!="Naiad",Position=="Downstream",between(month(Date),5,11),STA=="STA-3/4 Cell 2B",TPO4<.040) %>% drop_na(TPO4) %>% drop_na(`DCS (Field Data)`) %>% drop_na(`Mean outflow (cfs)`)
contour_data_dry <- filter(WQ_Field_with_continuous_same_rows,Ecotope!="Naiad",Position=="Downstream",!between(month(Date),5,11),STA=="STA-3/4 Cell 2B",TPO4<.040) %>% drop_na(TPO4) %>% drop_na(`DCS (Field Data)`) %>% drop_na(`Mean outflow (cfs)`) 
contour_grid_dry <- with(contour_data_dry, interp::interp(`DCS (Field Data)`, `Mean outflow (cfs)`, TPO4*1000,duplicate="mean"))
contour_grid_wet <- with(contour_data_wet, interp::interp(`DCS (Field Data)`, `Mean outflow (cfs)`, TPO4*1000,duplicate="mean"))
griddf_dry <- subset(data.frame(`Depth` = rep(contour_grid_dry$x, nrow(contour_grid_dry$z)), `CFS`= rep(contour_grid_dry$y, each = ncol(contour_grid_dry$z)), z = as.numeric(contour_grid_dry$z)),!is.na(z)) %>%rename(`DCS (Field Data)`="Depth",`Mean outflow (cfs)`="CFS",TPO4="z")
griddf_wet <- subset(data.frame(`Depth` = rep(contour_grid_wet$x, nrow(contour_grid_wet$z)), `CFS`= rep(contour_grid_wet$y, each = ncol(contour_grid_wet$z)), z = as.numeric(contour_grid_wet$z)),!is.na(z)) %>%rename(`DCS (Field Data)`="Depth",`Mean outflow (cfs)`="CFS",TPO4="z")

ggplot(griddf_dry, aes(`DCS (Field Data)`,`Mean outflow (cfs)` , z = TPO4)) +
geom_contour_filled(binwidth =2,na.fill = TRUE) +  xlab("Depth to Consolidated Substrate (cm)")+ylab("Outflow (cfs)")+Presentation_theme+theme(legend.position="right")+
geom_point(data = contour_data_dry, aes(`DCS (Field Data)`,`Mean outflow (cfs)`))+guides(fill=guide_legend(title=expression(TP~(mu~g~L^-1))))

ggplot(griddf_wet, aes(`DCS (Field Data)`,`Mean outflow (cfs)` , z = TPO4)) +
geom_contour_filled(binwidth =2,na.fill = TRUE) +  xlab("Depth to Consolidated Substrate (cm)")+ylab("Outflow (cfs)")+Presentation_theme+theme(legend.position="right")+
geom_point(data = contour_data_wet, aes(`DCS (Field Data)`,`Mean outflow (cfs)`))+guides(fill=guide_legend(title=expression(TP~(mu~g~L^-1))))


# Flow vs Depth  and TP (STA-1W)  -----------------------------------------

contour_data_1W <- filter(WQ_Field_with_continuous_same_rows,Ecotope!="Naiad",STA=="STA-1W Cell 5B",Position=="Downstream") %>%
  drop_na(TPO4) %>%
  drop_na(`DCS (Field Data)`) %>%
  drop_na(`Mean outflow (cfs)`) 

#Interpolate to grid from STA-1W
contour_grid_1W <- with(contour_data_1W, interp::interp(`DCS (Field Data)`, `Mean outflow (cfs)`, TPO4*1000,duplicate="mean"))
griddf_1W <- subset(data.frame(`Depth` = rep(contour_grid_1W$x, nrow(contour_grid_1W$z)),
                               `CFS`= rep(contour_grid_1W$y, each = ncol(contour_grid_1W$z)),
                               z = as.numeric(contour_grid_1W$z)),!is.na(z)) %>%
  rename(`DCS (Field Data)`="Depth",`Mean outflow (cfs)`="CFS",TPO4="z")

ggplot(griddf_1W, aes(`DCS (Field Data)`,`Mean outflow (cfs)` , z = TPO4)) +
  geom_contour_filled(binwidth =3) +  xlab("Depth to Consolidated Substrate (cm)")+ylab("Outflow (cfs)")+Presentation_theme+theme(legend.position="bottom")+
  geom_point(data = contour_data_1W, aes(`DCS (Field Data)`,`Mean outflow (cfs)`))+guides(fill=guide_legend(title=expression(TP~(mu~g~L^-1))))

ggsave(plot = last_plot(),filename="./Figures/TPO4 vs Flow vs Depth-SFER.jpeg",width =8, height =5.5, units = "in")


# TP vs physico-chemical parameters ----------------------------------------
ggplot(filter(WQ_Field_with_continuous_same_rows,Position=="Downstream",Ecotope!="Naiad",STA=="STA-3/4 Cell 2B",Date<"2022-06-02"),aes(`Temp`,`TPO4`*1000,color=Ecotope,fill=Ecotope))+geom_smooth()+geom_point(shape=21,size=2,color="grey70")+
facet_wrap(~Ecotope)+scale_y_continuous(breaks=seq(0,80,10))+Presentation_theme2+coord_cartesian(ylim=c(0,40))+
ylab(expression(TP~(mu~g~L^-1)))+xlab("Temp (C)")+guides(fill="none",color="none")

ggsave(plot = last_plot(),filename="./Figures/TPO4 vs Temp.jpeg",width =5, height =6, units = "in")


ggplot(WQ_Field_with_continuous_same_rows,aes(SpCond,`TPO4`))+geom_point(shape=21,size=2)+geom_smooth()+
facet_wrap(~Ecotope)+scale_y_continuous(breaks=seq(0,.03,0.005))+
scale_fill_brewer(palette = "Set2",direction = -1)+scale_color_brewer(palette = "Set2",direction = -1)+theme_bw()

ggplot(WQ_Field_Data_Continuous_data,aes(pH,`TPO4`))+geom_point(shape=21,size=2)+geom_smooth()+
facet_wrap(~Ecotope)+scale_y_continuous(breaks=seq(0,.03,0.005))+
scale_fill_brewer(palette = "Set2",direction = -1)+scale_color_brewer(palette = "Set2",direction = -1)+theme_bw()

ggplot(WQ_Field_Data_Continuous_data,aes(DO,`TPO4`))+geom_point(shape=21,size=2)+geom_smooth()+
facet_wrap(~Ecotope)+scale_y_continuous(breaks=seq(0,.03,0.005))+scale_x_continuous(limits=c(0,25))+
scale_fill_brewer(palette = "Set2",direction = -1)+scale_color_brewer(palette = "Set2",direction = -1)+theme_bw()

# TP Differences vs physico-chemical parameters ----------------------------------------
ggplot(WQ_Field_Data_Continuous_data,aes(`Temp`,`Dif TPO4`))+geom_point(shape=21,size=2)+geom_smooth()+
facet_wrap(~Ecotope)+scale_y_continuous(breaks=seq(0,.03,0.005))+
scale_fill_brewer(palette = "Set2",direction = -1)+scale_color_brewer(palette = "Set2",direction = -1)+theme_bw()

ggplot(WQ_Field_Data_Continuous_data,aes(SpCond,`Dif TPO4`))+geom_point(shape=21,size=2)+geom_smooth()+
facet_wrap(~Ecotope)+scale_y_continuous(breaks=seq(0,.03,0.005))+
scale_fill_brewer(palette = "Set2",direction = -1)+scale_color_brewer(palette = "Set2",direction = -1)+theme_bw()

ggplot(WQ_Field_Data_Continuous_data,aes(pH,`Dif TPO4`))+geom_point(shape=21,size=2)+geom_smooth()+
facet_wrap(~Ecotope)+scale_y_continuous(breaks=seq(0,.03,0.005))+
scale_fill_brewer(palette = "Set2",direction = -1)+scale_color_brewer(palette = "Set2",direction = -1)+theme_bw()

ggplot(WQ_Field_Data_Continuous_data,aes(DO,`Dif TPO4`))+geom_point(shape=21,size=2)+geom_smooth()+
facet_wrap(~Ecotope)+scale_y_continuous(breaks=seq(0,.03,0.005))+scale_x_continuous(limits=c(0,25))+
scale_fill_brewer(palette = "Set2",direction = -1)+scale_color_brewer(palette = "Set2",direction = -1)+theme_bw()




# Ash Figures -------------------------------------------------------------

Ash_DF <- WQ_Field_Data %>%
mutate(Ash=if_else(`Date`=="2022-03-01","Yes","No")) %>%
pivot_longer(names_to="TEST_NAME",values_to="VALUE",6:33) 

ggplot(Ash_DF,aes(Ash,`VALUE`,fill=Ash))+geom_boxplot(color="black")+
facet_wrap(~TEST_NAME,scales = "free_y")+scale_fill_brewer(palette = "Set2",direction = -1)+scale_color_brewer(palette = "Set2",direction = -1)+theme_bw()




# TP Differences ----------------------------------------------------------
#Table with summary stats
TP_Diff_Summary_Stats <- filter(WQ_Upstream_Downstream_Tidy,TEST_NAME=="TPO4") %>%
mutate(Month=month(Date),Season=if_else(between(Month,6,11),"Wet Season","Dry Season")) %>%
group_by(STA,Ecotope,Season) %>%  
summarise(n()/1000,`Median diff`=median(Difference,na.rm=TRUE),`mean diff`=mean(Difference,na.rm=TRUE),`mean upstream`=mean(`Upstream Values`,na.rm=TRUE),`mean downstream`=mean(`Downstream Values`,na.rm=TRUE))%>%
mutate(across(where(is.numeric) ,~.x*1000, ~round(.x,digits=1)))    

write.csv(TP_Diff_Summary_Stats,"./Data/Publish Tables/Upstream Downstream differences by season.csv",row.names = FALSE) #SFER table


#TPO4 Differences
ggplot(filter(WQ_Upstream_Downstream_Tidy,TEST_NAME=="TPO4",Ecotope!="Naiad"),aes(Ecotope,`Difference`*1000,fill=Ecotope))+facet_wrap(~STA,ncol=1)+
scale_y_continuous(breaks=seq(-50,50,5),limits=c(-10,10))+ geom_hline(aes(yintercept = 0),linetype="dashed",color="white")+geom_boxplot()+Presentation_theme2+
ylab(expression(Upstream-Downstream~TP~(mu~g~L^-1)))+xlab("Ecotope")+guides(fill="none",color="none")

ggsave(plot = last_plot(),filename="./Figures/TPO4 Differences Upstream-Downstream.jpeg",width =5, height =5, units = "in")

# TP Differences over time
ggplot(filter(WQ_Upstream_Downstream_Tidy,TEST_NAME=="TPO4"),aes(Date,`Difference`*1000,fill=Ecotope,color=Ecotope))+
geom_point(aes(Date,Difference*1000,color=Ecotope,fill=Ecotope),size=2,shape=21)+facet_wrap(~STA,ncol=1)+
geom_smooth(se=FALSE)+scale_x_date(date_breaks="1 month",labels = date_format("%b %y"))+guides(x =  guide_axis(angle = 40))+labs(y=expression(TP~(mu~g~L^-1)))


#TPO4 Concentration over time points and smooth (STA1W and STA34) SFER 2023 Fig 1
WQ_Fig_Up_Down <- WQ_Upstream_Downstream_Tidy %>%
  mutate(`Figure Label Date`=case_when(STA=="STA-3/4 Cell 2B" & Date <"2021-09-15"~make_date(year=year(Date)+2,month=month(Date),day=day(Date)),
                                       STA=="STA-3/4 Cell 2B" & Date >="2021-09-15" & Date <"2022-07-01"~make_date(year=year(Date)+1,month=month(Date),day=day(Date)),
                                       STA=="STA-3/4 Cell 2B" & Date >"2022-07-01"~make_date(year=year(Date),month=month(Date),day=day(Date)),
                                       STA=="STA-1W Cell 5B"~make_date(year=year(Date),month=month(Date),day=day(Date)))) %>%
  mutate(`Study Period`=case_when(STA=="STA-3/4 Cell 2B" & Date <"2022-07-01"~"Year 1: STA-3/4",
                                  STA=="STA-3/4 Cell 2B" & Date >"2022-07-01"~"Year 2: STA-3/4",
                                  STA=="STA-1W Cell 5B"~"Year 2: STA-1W"))  

#TP differnce over time 
ggplot(filter(WQ_Fig_Up_Down,Ecotope!="Naiad",TEST_NAME=="TPO4"),aes(`Figure Label Date`,Difference*1000,color=Ecotope,fill=Ecotope,linetype=Ecotope))+
geom_point(aes(`Figure Label Date`,Difference*1000,color=Ecotope,fill=Ecotope),size=3)+
geom_smooth(se=FALSE)+facet_wrap(~factor(`Study Period`,levels = c("Year 1: STA-3/4","Year 2: STA-3/4","Year 2: STA-1W")),nrow=3,scales = "free_y")+
geom_hline(aes(yintercept = 0),color="#785d37",linetype="longdash")+ 
scale_shape_manual(values = c(21:24)) +#coord_cartesian(ylim=c(-15,20))+#scale_y_continuous(breaks=seq(0,100,10),limits=c(0,100))+
scale_x_date(date_breaks="1 month",labels = date_format("%b"))+ 
scale_color_discrete("Ecotope", breaks = c("Bare","Chara","Mixed","Typha"),labels = c("Bare", expression(italic("Chara")),"Mixed",expression(italic("Typha"))))+  
Presentation_theme+  guides(x =  guide_axis(angle = 40),linetype="none",fill="none")+labs(y=expression(TP~(mu~g~L^-1)),x="Date")

ggsave(plot = last_plot(),filename="./Figures/TPO4 Differences Upstream-Downstream time series.jpeg",width =8, height =8, units = "in")


# TP forms over time -----------------------------------------------------
#create DF of P forms
TP_forms <- WQ_Data_Tidy %>%
filter(TEST_NAME %in% c("TPO4","OPO4","TDPO4"),Position=="Downstream") %>%
mutate(VALUE=ifelse(REMARK_CODE %in% "U",0,VALUE)) %>%   #substitute
select(Date,TEST_NAME,VALUE,STA,Ecotope,Position)  %>%
pivot_wider(names_from="TEST_NAME",values_from="VALUE") %>%
mutate(`Particulate P`=TPO4-TDPO4,`Dissolved Organic P`=TDPO4-OPO4,`Soluble Reactive P`=OPO4) %>%
select(1:4,8:10) %>%
pivot_longer(5:7,names_to="P Form",values_to="Value") %>%
mutate(`P Form`=factor(`P Form`,levels=c("Soluble Reactive P","Dissolved Organic P","Particulate P")))  

TP_forms_monthly <- TP_forms %>%
mutate(Month=month(Date,abbr=TRUE,label=TRUE)) %>%
group_by(STA,Ecotope,Month,`P Form`) %>%
summarise(n(),`Monthly Mean`=mean(Value*1000,na.rm=TRUE))

#TP forms over time
ggplot(filter(TP_forms_monthly,Ecotope!="Naiad"),aes(Month,`Monthly Mean`))+geom_col(aes(fill=`P Form`))+
facet_grid((STA)~Ecotope)+scale_fill_discrete(limits = rev)+
Presentation_theme   +theme(axis.text.x=element_text(size=rel(0.75)))+
guides(x =guide_axis(angle = 45))+labs(y=expression(P~(mu~g~L^-1)))

ggsave(plot = last_plot(),filename="./Figures/P Forms over time- Monthly.jpeg",width =8, height =8, units = "in")

#P over time
ggplot(filter(TP_forms,Ecotope!="Naiad"),aes(month(Date)+day(Date)/31,Value*1000,color=`P Form`,fill=`P Form`))+geom_point()+
geom_ribbon(stat='smooth', method = "loess", se=TRUE, alpha=0.3) +
geom_line(stat='smooth', method = "loess")  + theme(axis.text.x=element_text(size=rel(0.75)))+
facet_grid(STA~Ecotope)+scale_fill_discrete()+Presentation_theme + scale_x_discrete(limits = 1:12, labels = month.abb)+
guides(x =  guide_axis(angle = 40))+labs(y=expression(P~(mu~g~L^-1)),x="Month")+coord_cartesian(ylim = c(0, 30))

ggsave(plot = last_plot(),filename="./Figures/P Forms over time- Points and smooth.jpeg",width =8, height =8, units = "in")


WQ_Data_Tidy %>%
filter(TEST_NAME=="OPO4",STA=="STA-3/4 Cell 2B") %>%
#group_by(Ecotope) %>%
mutate(VALUE=VALUE*1000) %>%  
summarise(n(),mean=mean(VALUE, na.rm=TRUE),max=max(VALUE,na.rm=TRUE),min=min(VALUE,na.rm=TRUE),`non-detects`=sum(if_else(`REMARK_CODE`=="U",1,0),na.rm=TRUE),`Nondetect %`=percent(`non-detects`/n()))  

opo4_data <-WQ_Data_Tidy %>%
filter(TEST_NAME=="OPO4",STA=="STA-3/4 Cell 2B") 

ggplot(opo4_data,aes(VALUE*1000))+geom_histogram()

# TP Budget Figures -------------------------------------------------------

Cumulative_TP <- TP_Budget %>%
  pivot_longer(23:28,names_to="Ecotope",values_to="Value") %>%
  #filter(row_number() %% 4==1) %>% #Select every 4th row 
  slice_sample(n=1000) %>%
  mutate(`y axis`=case_when(Ecotope=="Cumulative TP Chara"~1595,
                            Ecotope=="Cumulative TP Bare"~1935,
                            Ecotope=="Cumulative TP Typha"~1860,
                            Ecotope=="Cumulative TP Naiad"~1790,
                            Ecotope=="Cumulative TP Mixed"~1740,
                            Ecotope=="Cumulative TP G379D"~1790,
                            Ecotope=="Cumulative TP G379B"~2350)) %>%
  mutate(`x axis`=case_when(Ecotope=="Cumulative TP Chara"~"2022-04-01 00:00:00",
                            Ecotope=="Cumulative TP Bare"~"2022-04-01 00:00:00",
                            Ecotope=="Cumulative TP Typha"~"2022-04-01 00:00:00",
                            Ecotope=="Cumulative TP Naiad"~"2022-06-01 00:00:00",
                            Ecotope=="Cumulative TP Mixed"~"2022-04-01 00:00:00",
                            Ecotope=="Cumulative TP G379D"~"2022-04-01 00:00:00",
                            Ecotope=="Cumulative TP G379B"~"2022-04-01 00:00:00")) %>%
  mutate(Ecotope=case_when(Ecotope=="Cumulative TP Chara"~"Chara",
                           Ecotope=="Cumulative TP Bare"~"Bare",
                           Ecotope=="Cumulative TP Typha"~"Typha",
                           Ecotope=="Cumulative TP Naiad"~"Southern Naiad",
                           Ecotope=="Cumulative TP Mixed"~"Mix",
                           Ecotope=="Cumulative TP G379D"~"G379D",
                           Ecotope=="Cumulative TP G379B"~"G379B")) %>%  
  mutate(`x axis`=ymd_hms(`x axis`))  

#All Analyses Concentration over time points and smooth
ggplot(filter(Cumulative_TP,Ecotope %in% c("Chara","Bare","Typha","Mix")),aes(`Date Time`,`Value`,color=Ecotope))+geom_text(aes(`x axis`,`y axis`,color=Ecotope,label=Ecotope)) +
  scale_y_continuous(sec.axis = sec_axis(~., name = "Outflow (cfs)"))+geom_line()+geom_line(aes(`Date Time`,`Outflow (cfs)`),color="black",linetype="dashed")+
  theme_bw()+scale_x_datetime(date_breaks="3 month",labels = date_format("%b %y"))+ ylab("P (kg)")+guides(x =  guide_axis(angle = 40),position="bottom")+Presentation_theme

ggsave(plot = last_plot(),filename="./Figures/Cumulative TP.jpeg",width =13.333, height =7.5, units = "in")

ggsave(plot = last_plot(),filename="./Figures/Cumulative TP-SFER.jpeg",width =8, height =5.5, units = "in")





# FWM Weight over time  ----------------------------------------------------

#P Load over time
ggplot(TP_Budget_Daily_Combined,aes(`Figure Label Date`,`Load`,fill=`Study Period`,color=`Study Period`))+geom_area(alpha=.7,position="dodge")+facet_wrap(~Ecotope,nrow=2,labeller = label_parsed)+
scale_x_date(date_breaks="1 month",labels = date_format("%b"))+labs(x="")+coord_cartesian(xlim=as.Date(c("2000-01-01","2000-12-14")))+
guides(x =  guide_axis(angle = 40),position="bottom")+ylab("Daily TP Load (Kg)")+theme(legend.position="bottom")

#P FWM over time STA3/4
ggplot(filter(TP_Budget_Daily_Combined,Ecotope %in% c("italic(Chara)","Bare","italic(Typha)","Mixed"),STA=="STA-3/4"),aes(`Date`,`Int`*1000,fill=`Study Period`,color=`Study Period`))+
geom_col(aes(`Date`,y=`Outflow (cfs)`/40),alpha=.3,fill="azure3",color="azure3")+ 
geom_line(aes(`Date`,`Running_FWM`*1000),size=1,linetype="dashed",color="#3a6589")+ 
geom_line(size=1.25)+scale_color_manual(values=c("#ffb84d","#62bba5"))+  
facet_wrap(~Ecotope,labeller = label_parsed)+
scale_y_continuous( sec.axis = sec_axis(~ . * 40, name = "Discharge (CFS)"))+
geom_hline(aes(yintercept = 13),color="#785d37",linetype="longdash")+ geom_hline(aes(yintercept = 0),color="#785d37")+ 
scale_x_date(date_breaks="3 months",labels = date_format("%b %Y"))+labs(x="")+
guides(x =  guide_axis(angle = 40),position="bottom")+ylab(expression(FWM~TP~(mu~g~L^-1)))+theme(legend.position="bottom")

ggsave(plot = last_plot(),filename="./Figures/FWM TP Daily and cumulative-34- SFER24.jpeg",width =12, height =8, units = "in")

#P FWM over time 1W
ggplot(filter(TP_Budget_Daily_Combined,Ecotope %in% c("italic(Chara)","Bare","italic(Typha)","Mixed"),STA=="STA-1W"),aes(`Date`,`Int`*1000,fill=`Study Period`,color=`Study Period`))+
geom_col(aes(`Date`,y=`Outflow (cfs)`/10),alpha=.3,fill="azure3",color="azure3")+ 
geom_line(aes(`Date`,`Running_FWM`*1000),linewidth=1,linetype="dashed",color="#3a6589")+ 
geom_line(size=1.25)+scale_color_manual(values=c("#ffb84d","#62bba5"))+  
facet_wrap(~Ecotope,labeller = label_parsed,nrow=1)+
scale_y_continuous( sec.axis = sec_axis(~ . * 10, name = "Discharge (CFS)"))+
geom_hline(aes(yintercept = 13),color="#785d37",linetype="longdash")+ geom_hline(aes(yintercept = 0),color="#785d37")+ 
scale_x_date(date_breaks="3 months",labels = date_format("%b %Y"))+labs(x="")+
guides(x =  guide_axis(angle = 40),position="bottom")+ylab(expression(FWM~TP~(mu~g~L^-1)))+theme(legend.position="bottom")

ggsave(plot = last_plot(),filename="./Figures/FWM TP Daily and cumulative-1W- SFER24.jpeg",width =12, height =6, units = "in")

swatch(c("#785d37","#62bba5","#ffb84d","#aaa488","#b2432f","#3a6589","#9b5672","#908150","#373634"))

#P FWM over time
ggplot(TP_Budget_Daily_Combined,aes(`Figure Label Date`,`Running_FWM`*1000,fill=`Study Period`,color=`Study Period`))+geom_line(size=1.25,linetype="dashed")+facet_wrap(~Ecotope,nrow=2,labeller = label_parsed)+
geom_line(aes(`Figure Label Date`,`Daily_FWM`*1000,fill=`Study Period`,color=`Study Period`),size=1)+ geom_hline(aes(yintercept = 13),color="#785d37",linetype="longdash")+ 
scale_x_date(date_breaks="3 months",labels = date_format("%b"))+labs(x="")+
guides(x =  guide_axis(angle = 40),position="bottom")+ylab(expression(TP~(mu~g~L^-1)))+theme(legend.position="bottom")



#FWM-TP Weekly
ggplot(FWM_Weekly,aes(Week,`FWM-TP`*1000,fill=`Study Period`,color=`Study Period`))+geom_area(alpha=.7,position="dodge")+facet_wrap(~Ecotope,nrow=2,labeller = label_parsed)+
labs(x="")+
guides(x =  guide_axis(angle = 40),position="bottom")+ylab(expression(TP~(mu~g~L^-1)))+theme(legend.position="bottom")
ggsave(plot = last_plot(),filename="./Figures/Weekly FWM TP.jpeg",width =13.333, height =7.5, units = "in")


#FWM-TP Daily
ggplot(FWM_Daily,aes(`Figure Label Date`,`FWM-TP`*1000,fill=`Study Period`,color=`Study Period`))+geom_area(alpha=.7,position="dodge")+facet_wrap(~Ecotope,nrow=2,labeller = label_parsed)+
labs(x="")+scale_x_date(date_breaks="1 month",labels = date_format("%b"))+geom_hline(aes(yintercept = 13),color="#785d37",linetype="longdash")+ 
geom_point()  
guides(x =  guide_axis(angle = 40),position="bottom")+ylab(expression(TP~(mu~g~L^-1)))+theme(legend.position="bottom")

ggsave(plot = last_plot(),filename="./Figures/Daily FWM TP.jpeg",width =13.333, height =7.5, units = "in")






# Proportion of flow in wet and dry season.  ------------------------------

Study_total <- TP_Budget_Daily_Combined %>%
filter(Ecotope=="Bare") %>%  
group_by(STA) %>%
summarise(`Total Flow (acre ft)`=sum(`Outflow (cfs)`,na.rm=TRUE)*1.98)  

Seasonal_total <- TP_Budget_Daily_Combined %>%
filter(Ecotope=="Bare") %>%  
mutate(Season=if_else(between(month(Date),6,11)==TRUE,"Wet Season","Dry Season"))  %>%
group_by(STA,Season) %>%
summarise(n(),`Seasonal Flow (acre ft)`=sum(`Outflow (cfs)`,na.rm=TRUE)*1.98)  %>%
left_join(Study_total,by=c("STA"))  %>%
mutate(`Percent of flow`=percent(`Seasonal Flow (acre ft)`/`Total Flow (acre ft)`))  

Monthly_flow <- TP_Budget_Daily_Combined %>%
filter(Ecotope=="Bare") %>%  
mutate(Month=month(Date,abb=T,label=T))  %>%
group_by(STA,Month) %>%
summarise(`Month Flow (acre ft)`=sum(`Outflow (cfs)`,na.rm=TRUE)*1.98)   %>%  
left_join(Study_total,by=c("STA"))  %>%
mutate(`Percent of flow`=percent(as.numeric(`Month Flow (acre ft)`)/`Total Flow (acre ft)`))    



# Ion balance and nutrient Ratios  ---------------------------------------------------------

Nutrient_Ratios_tidy <- WQ_Field_with_continuous_same_rows %>%
select(1:30) %>%
mutate(`N to P`=if_else(is.finite(TN),(TN/14)/(TPO4/30.97),NULL)) %>%
mutate(`Anions`=if_else(is.finite(CL),CL/35.453+SO4/96.06*2+ALKA/100.08*2,NULL)) %>%
mutate(`Cations`=if_else(is.finite(CA),CA/40.07*2+K/39.09+MG/24.3*2+`NA`/22.99,NULL)) %>%
mutate(`Ion Ratio`=if_else(is.finite(Anions),Anions/Cations,NULL))

#Ion balance by ecotope and STA 
ggplot(Nutrient_Ratios_tidy,aes(Ecotope,`Ion Ratio`,fill=Ecotope))+facet_wrap(~STA,ncol=1)+
geom_boxplot()+Presentation_theme2+scale_y_continuous(labels=scales::percent_format(accuracy=1))+
xlab("Ecotope")+guides(fill="none",color="none")

#N to P ratio by ecotope and STA 
ggplot(filter(Nutrient_Ratios_tidy,Ecotope!="Naiad"),aes(Ecotope,`N to P`,fill=Ecotope))+facet_wrap(~STA,ncol=1)+
geom_boxplot()+Presentation_theme2+
xlab("Ecotope")+guides(fill="none",color="none")

#N to P ratio by ecotope and STA  over time
ggplot(filter(Nutrient_Ratios_tidy,Ecotope!="Naiad"),aes(ymd(Date),`N to P`,fill=Ecotope,color=Ecotope))+facet_wrap(~STA,ncol=1)+
geom_point(shape=21)+Presentation_theme2+scale_x_date(date_breaks="3 months",labels = date_format("%Y %b"))+ geom_smooth(se=FALSE)+
xlab("Ecotope")+guides(x =  guide_axis(angle = 40))

#N to P ratio vs TP by ecotope and STA  
ggplot(filter(Nutrient_Ratios_tidy,Ecotope!="Naiad"),aes(`N to P`,TPO4*1000,fill=Ecotope,color=Ecotope))+facet_wrap(~STA,ncol=1)+
geom_point(shape=21)+Presentation_theme2+labs(y=expression(TP~(mu~g~L^-1)))+scale_y_continuous(breaks=seq(0,60,10),limits=c(0,60))+
guides(x =  guide_axis(angle = 40))+geom_smooth(aes(`N to P`,TPO4*1000),se=F,color="grey",fill="grey")

ggsave(plot = last_plot(),filename="./Figures/N to P ratio vs TP by ecotope and STA.jpeg",width =12, height =8, units = "in")

#N to P ratio vs TP by ecotope and STA and ecotope (STA3/4 only)
ggplot(filter(Nutrient_Ratios_tidy,Ecotope!="Naiad",STA=="STA-3/4 Cell 2B"),aes(`N to P`,TPO4*1000,fill=Ecotope,color=Ecotope))+facet_wrap(~STA,ncol=1)+
geom_point(shape=21)+Presentation_theme2+labs(y=expression(TP~(mu~g~L^-1)))+scale_y_continuous(breaks=seq(0,60,10),limits=c(0,60))+
guides(x =  guide_axis(angle = 40))+geom_smooth()

ggsave(plot = last_plot(),filename="./Figures/N to P ratio vs TP by ecotope and STA.jpeg",width =12, height =8, units = "in")

#N to P ratio vs TP by ecotope and STA and ecotope (STA3/4 only)
ggplot(filter(Nutrient_Ratios_tidy,Ecotope!="Naiad",STA=="STA-3/4 Cell 2B"),aes(`N to P`,TPO4*1000,fill=Ecotope,color=Ecotope))+facet_wrap(~STA,ncol=1)+
  geom_point(shape=21)+Presentation_theme2+labs(y=expression(TP~(mu~g~L^-1)))+scale_y_continuous(breaks=seq(0,60,10),limits=c(0,60))+
  guides(x =  guide_axis(angle = 40))+geom_smooth()



# Spatial Sampling Maps ---------------------------------------------------

#Map of TP- using 
ggplot(WQ_and_Spatial_long,aes(Long,Lat,fill=`Diff from mean label`,color=`Diff from mean label`))+geom_point(size=3,shape=21)+
coord_quickmap()+guides(legend="bottom")+
facet_wrap(~Ecotope)

#Map of TP 
ggplot(WQ_and_Spatial_long,aes(Long,Lat,fill=`Diff from mean label`,color=`Diff from mean label`,size=`Difference from mean`))+
geom_point(shape=21)+
facet_wrap(~Ecotope,scales="free")

#Interpolate to grid for each ecotope
contour_chara <- with(filter(WQ_and_Spatial_long,Ecotope=="Chara"), interp::interp(Lat,Long,`Difference from mean`,duplicate="mean",method="linear"))
contour_Bare <- with(filter(WQ_and_Spatial_long,Ecotope=="Bare"), interp::interp(Lat,Long, `Difference from mean`,duplicate="mean",method="linear"))
contour_Mixed <- with(filter(WQ_and_Spatial_long,Ecotope=="Mixed"), interp::interp(Lat,Long,`Difference from mean`,duplicate="mean",method="linear"))
contour_Typha <- with(filter(WQ_and_Spatial_long,Ecotope=="Typha"), interp::interp(Lat,Long, `Difference from mean`,duplicate="mean",method="linear"))

gridd_map_chara <- subset(data.frame(`Long` = rep(contour_chara$x, nrow(contour_chara$z)),`Lat`= rep(contour_chara$y, each = ncol(contour_chara$z)), TPO4 = as.numeric(contour_chara$z))) 
gridd_map_bare <- subset(data.frame(`Long` = rep(contour_Bare$x, nrow(contour_Bare$z)),`Lat`= rep(contour_Bare$y, each = ncol(contour_Bare$z)), TPO4 = as.numeric(contour_Bare$z))) 
gridd_map_Mixed <- subset(data.frame(`Long` = rep(contour_Mixed$x, nrow(contour_Mixed$z)),`Lat`= rep(contour_Mixed$y, each = ncol(contour_Mixed$z)), TPO4 = as.numeric(contour_Mixed$z))) 
gridd_map_Typha <- subset(data.frame(`Long` = rep(contour_Typha$x, nrow(contour_Typha$z)),`Lat`= rep(contour_Typha$y, each = ncol(contour_Typha$z)), TPO4 = as.numeric(contour_Typha$z))) 

all_maps <- mutate(gridd_map_chara,Ecotope="Chara") %>%
bind_rows(mutate(gridd_map_Typha,Ecotope="Typha")) %>%
bind_rows(mutate(gridd_map_Mixed,Ecotope="Mixed")) %>%
bind_rows(mutate(gridd_map_bare,Ecotope="Bare")) 

#Difference from mean TP 
ggplot(all_maps, aes(Long,Lat , z = TPO4)) +
facet_wrap(~Ecotope,scales="free")+
geom_contour_filled(binwidth =.1)+Presentation_theme+
geom_point(data=WQ_and_Spatial_long,aes(`Lat`,Long),inherit.aes=FALSE,color="grey20",size=.5)+
guides(fill="none",x =  guide_axis(angle = 40))#+guides(fill=guide_legend(title=expression(TP~(mu~g~L^-1))),position="botttom")

ggsave(plot = last_plot(),filename="./Figures/TPO4 Difference from mean.jpeg",width =8.5, height =11, units = "in")

#Difference from mean TP Chara only 
ggplot(gridd_map_chara, aes(Long,Lat , z = TPO4)) +
geom_contour_filled(binwidth =.1)+theme_void()+guides(fill="none")+
geom_point(data=filter(WQ_and_Spatial_long,Ecotope=="Chara"),aes(`Lat`,Long),inherit.aes=FALSE,color="grey20",size=.5)

ggsave(plot = last_plot(),filename="./Figures/TPO4 Difference from mean Chara Void.jpeg",width =8.5, height =11, units = "in")

#Difference from mean TP Typha only 
ggplot(gridd_map_Typha, aes(Long,Lat , z = TPO4)) +
geom_contour_filled(binwidth =.1)+theme_void()+guides(fill="none")+
geom_point(data=filter(WQ_and_Spatial_long,Ecotope=="Typha"),aes(`Lat`,Long),inherit.aes=FALSE,color="grey20",size=.5)

ggsave(plot = last_plot(),filename="./Figures/TPO4 Difference from mean Typha Void.jpeg",width =8.5, height =11, units = "in")

#Difference from mean TP Mixed only 
ggplot(gridd_map_Mixed, aes(Long,Lat , z = TPO4)) +
geom_contour_filled(binwidth =.1)+theme_void()+guides(fill="none")+
geom_point(data=filter(WQ_and_Spatial_long,Ecotope=="Mixed"),aes(`Lat`,Long),inherit.aes=FALSE,color="grey20",size=.5)

ggsave(plot = last_plot(),filename="./Figures/TPO4 Difference from mean Mixed Void.jpeg",width =8.5, height =11, units = "in")

#Difference from mean TP Bare only 
ggplot(gridd_map_bare, aes(Long,Lat , z = TPO4)) +
geom_contour_filled(binwidth =.1)+theme_void()+guides(fill="none")+
geom_point(data=filter(WQ_and_Spatial_long,Ecotope=="Bare"),aes(`Lat`,Long),inherit.aes=FALSE,color="grey20",size=.5)

ggsave(plot = last_plot(),filename="./Figures/TPO4 Difference from mean Bare Void.jpeg",width =8.5, height =11, units = "in")

#Map of vegetation and TP
ggplot(WQ_and_Spatial_long,aes(Long,Lat,fill=`Vegetation`,color=`Vegetation`,size=`Diff from mean label`))+
geom_point(shape=21)+scale_color_viridis(discrete=TRUE)+scale_fill_viridis(discrete=TRUE)+
facet_wrap(~Ecotope,scales="free")

#number of species present at a point vs difference from the mean
ggplot(WQ_and_Spatial_long,aes(`Number of Species`,`Diff from mean label`))+
geom_jitter(width = 0.15,height = 0.15)+geom_smooth(color="grey",fill="grey")+
facet_wrap(~Ecotope,scales="free")

#Average difference from the mean by vegetation
ggplot(WQ_and_Spatial_long,aes(Vegetation,`Diff from mean label`,color=Vegetation))+
geom_boxplot()+scale_color_viridis(discrete=TRUE)+scale_fill_viridis(discrete=TRUE)+
facet_wrap(~Ecotope,scales="free")

#Average difference from the mean by vegetation dominant veg only
ggplot(filter(WQ_and_Spatial_long,`Percent Vegetation`>=10 ),aes(Vegetation,`Diff from mean label`,fill=Vegetation))+
geom_boxplot(color="#785d37")+
facet_wrap(~Ecotope,scales="free")

#histogram of scaled differences
ggplot(WQ_and_Spatial_long,aes(`Scaled Difference`))+geom_histogram(binwidth=.5)+
facet_wrap(~Ecotope,scales="free")

Spatial_Tidy %>%
group_by(Ecotope) %>%
summarise(n(),`mean TP`=mean(VALUE))

 
#Paretto chart

ggplot(WQ_and_Spatial_long,aes(reorder(`Diff from mean label`,Rank),`Value`,fill=as.factor(Vegetation)))+geom_col(position=position_dodge2(width = 0.9, preserve = "single"),color="black")+
scale_y_continuous(breaks= pretty_breaks(n=10),labels = percent)+scale_fill_brewer(palette = "YlOrRd")+facet_wrap(~Vegetation)+
labs(x="Project",y="Blanks above or equal to MDL (%)",title = "Ammonia Blank Hits by Project and Year",fill = "Year",caption = "Top 25 projects with highest blank hits since 2015. Excludes projects ACRA, RAIN, S356FT, C111SC, EVER, and projects with less than 50 total blanks")+theme_bw()+theme(legend.position = "bottom",axis.text.x = element_text(angle = 90))



# Soil Figures and  SFER Table -------------------------------------------------------------------------------------------------

#Boxplots all analytes concentration
ggplot(filter(Soils_data,Ecotope!="Naiad"),aes(MATRIX,y=VALUE,fill=Ecotope,))+
geom_boxplot(alpha=.5)+facet_wrap(~`TEST_NAME`,scale="free")+
labs(y=NULL,x=NULL)+Presentation_theme+scale_y_continuous(labels = label_comma())+theme(legend.position="bottom")

ggsave(plot = last_plot(),filename="./Figures/Soils all analytes.jpeg",width =12, height =15, units = "in")

#Boxplots all analytes
ggplot(filter(Soils_data,Ecotope!="Naiad",!TEST_NAME %in% c("ASH","MOISTURE") ),aes(MATRIX,y=`Storage (g/m^2)`,fill=Ecotope))+
geom_boxplot(alpha=.5)+facet_wrap(~`TEST_NAME`,scale="free")+
labs(y=expression(P~(g~m^-2)),x=NULL)+Presentation_theme+scale_y_continuous(labels = label_comma())+theme(legend.position="bottom")

ggsave(plot = last_plot(),filename="./Figures/Soils all analytes.jpeg",width =12, height =15, units = "in")

#Soils Concentration figure
ggplot(filter(Soils_Summary_Stat_Sig,Units %in% c("(mg/kg)","%")),aes(MATRIX,y=Mean,fill=Ecotope,color=as.factor(MATRIX)))+scale_color_manual(values=c("grey30","grey60"))+
geom_text(aes(MATRIX,y*1.1,label=Letter,group=Ecotope,color=as.factor(MATRIX)),position=position_dodge(.8))+
geom_col(alpha=.5,color="grey50",position = position_dodge(width=.8),width=.8)+
geom_errorbar(aes(MATRIX,ymax=Mean+SE,ymin=Mean-SE),position=position_dodge(.8),width=.8,color="grey50")+
facet_wrap(~`Facet Label`,scale="free",nrow=2)+
labs(y=NULL,x=NULL)+Presentation_theme+scale_y_continuous(labels = label_comma())+theme(legend.position="bottom")+guides(color="none")

ggsave(plot = last_plot(),filename="./Figures/Soils Concentration.jpeg",width =16, height =7, units = "in")

#Soils Storage figure
ggplot(filter(Soils_Summary_Stat_Sig,Units=="(g/m^2)"),aes(MATRIX,y=Mean,fill=Ecotope,color=as.factor(MATRIX)))+scale_color_manual(values=c("grey30","grey60"))+
geom_text(aes(MATRIX,y*1.1,label=Letter,group=Ecotope,color=as.factor(MATRIX)),position=position_dodge(.8))+
geom_col(alpha=.5,color="grey50",position = position_dodge(width=.8),width=.8)+
geom_errorbar(aes(MATRIX,ymax=Mean+SE,ymin=Mean-SE),position=position_dodge(.8),width=.8,color="grey50")+
facet_wrap(~`Facet Label`,scale="free",nrow=2)+
labs(y=NULL,x=NULL)+Presentation_theme+scale_y_continuous(labels = label_comma())+theme(legend.position="bottom")+guides(color="none")
    
ggsave(plot = last_plot(),filename="./Figures/Soils Content.jpeg",width =16, height =7, units = "in")

#Soils physical parameters figure
ggplot(filter(Soils_Summary_Stat_Sig,Units %in% c("(g/cm^3)","(cm)")),aes(MATRIX,y=Mean,fill=Ecotope,color=as.factor(MATRIX)))+scale_color_manual(values=c("grey30","grey60"))+
geom_text(aes(MATRIX,y*1.1,label=Letter,group=Ecotope,color=as.factor(MATRIX)),position=position_dodge(.8))+
geom_col(alpha=.5,color="grey50",position = position_dodge(width=.8),width=.8)+
geom_errorbar(aes(MATRIX,ymax=Mean+SE,ymin=Mean-SE),position=position_dodge(.8),width=.8,color="grey50")+
facet_wrap(~`Facet Label`,scale="free",nrow=1)+
labs(y=NULL,x=NULL)+Presentation_theme+scale_y_continuous(labels = label_comma())+theme(legend.position="bottom")+guides(color="none")

ggsave(plot = last_plot(),filename="./Figures/Soils Physical Parameters.jpeg",width =16, height =5, units = "in")

#Soils all parameters combined
ggplot(Soils_Summary_Stat_Sig,aes(MATRIX,y=Mean,fill=Ecotope,color=as.factor(MATRIX)))+scale_color_manual(values=c("grey30","grey60"))+
geom_text(aes(MATRIX,y*1.1,label=Letter,group=Ecotope,color=as.factor(MATRIX)),position=position_dodge(.8))+
geom_col(alpha=.5,color="grey50",position = position_dodge(width=.8),width=.8)+
geom_errorbar(aes(MATRIX,ymax=Mean+SE,ymin=Mean-SE),position=position_dodge(.8),width=.8,color="grey50")+
facet_wrap(~`Facet Label`,scale="free",nrow=6)+
labs(y=NULL,x=NULL)+Presentation_theme+scale_y_continuous(labels = label_comma())+theme(legend.position="bottom")+guides(color="none")

ggsave(plot = last_plot(),filename="./Figures/Soils all analytes.jpeg",width =11.9, height =15.4, units = "in")


Soils_Summary_Stat_Sig %>% 
mutate(`Veg_type`=ifelse(Ecotope=="Typha","EAV","SAV")) %>%
filter(TEST_NAME=="Magnesium",Units=="(g/m^2)")  %>%
group_by(`Veg_type`,MATRIX) %>%
summarise(n(),mean=mean(Mean))



# Light Sensor Figures ----------------------------------------------------

All_light_data_season <- All_light_data %>%
mutate(Month=month(`Date Time`),Season=if_else(between(Month,6,11),"Wet Season","Dry Season"))


#Seasonal Light intensity by ecotope  (Issue with Bare dry season- investigate)
ggplot(filter(All_light_data_season,Ecotope!="Naiad"),aes(ymd_hms(format(`Date Time`, "2000-01-01 %H:%M:%S")),`Light Intensity Lux`,color=Position,fill=Position))+
geom_jitter(alpha=0.04)+  
geom_smooth(aes(ymd_hms(format(`Date Time`, "2000-01-01 %H:%M:%S")),`Light Intensity Lux`,linetype=Position,color=Position),size=1)+
facet_grid(~Ecotope)+
scale_x_datetime(labels = date_format("%I:%M:%p"),breaks=as.POSIXct(c("2000-01-01 00:00:00","2000-01-01 04:00:00","2000-01-01 08:00:00","2000-01-01 12:00:00","2000-01-01 16:00:00","2000-01-01 20:00:00","2000-01-01 24:00:00"),tz = "GMT"))+ 
scale_y_continuous(label=comma)+
Presentation_theme+  guides(x =  guide_axis(angle = 40))+labs(y=expression(Light~(Lumen~m^-2)),x="Time")

ggsave(plot = last_plot(),filename="./Figures/Light Intenstity.jpeg",width =12, height =8, units = "in")



#Seasonal Light intensity by ecotope  (Issue with Bare dry season- investigate)
ggplot(filter(All_light_data_season,Ecotope!="Naiad"),aes(ymd_hms(format(`Date Time`, "2000-01-01 %H:%M:%S")),`Light Intensity Lux`,color=Position,fill=Position))+
geom_jitter(alpha=0.04)+  
geom_smooth(aes(ymd_hms(format(`Date Time`, "2000-01-01 %H:%M:%S")),`Light Intensity Lux`,linetype=Position,color=Position),size=1)+
facet_grid(Season~Ecotope)+
scale_x_datetime(labels = date_format("%I:%M:%p"),breaks=as.POSIXct(c("2000-01-01 00:00:00","2000-01-01 04:00:00","2000-01-01 08:00:00","2000-01-01 12:00:00","2000-01-01 16:00:00","2000-01-01 20:00:00","2000-01-01 24:00:00"),tz = "GMT"))+ 
scale_y_continuous(label=comma)+
Presentation_theme+  guides(x =  guide_axis(angle = 40))+labs(y=expression(Light~(Lumen~m^-2)),x="Time")

ggsave(plot = last_plot(),filename="./Figures/Light Intenstity Seasonal.jpeg",width =12, height =8, units = "in")




# Physico-chemical parameters ---------------------------------------------

Physico_parameters_long <- WQ_Field_Data %>%
select(Date, STA,Ecotope,Position,Hour,Minute,Temp,pH,DO,SpCond) %>%
pivot_longer(values_to="Value",names_to="Parameter",7:10) 

# #trying to create a gap in hte dataframe where do dat was collected
bind_rows(data.frame(Date=as.Date(c("2022-06-01","2022-06-01","2022-06-01","2022-06-01","2022-09-01","2022-09-01","2022-09-01","2022-09-01")),STA="STA-3/4 Cell 2B",Ecotope="Chara",Parameter=c("DO","pH","Temp","SpCond"),Value=NA)) %>%
bind_rows(data.frame(Date=as.Date(c("2022-06-01","2022-06-01","2022-06-01","2022-06-01","2022-09-01","2022-09-01","2022-09-01","2022-09-01")),STA="STA-3/4 Cell 2B",Ecotope="Bare",Parameter=c("DO","pH","Temp","SpCond"),Value=NA)) %>%
bind_rows(data.frame(Date=as.Date(c("2022-06-01","2022-06-01","2022-06-01","2022-06-01","2022-09-01","2022-09-01","2022-09-01","2022-09-01")),STA="STA-3/4 Cell 2B",Ecotope="Typha",Parameter=c("DO","pH","Temp","SpCond"),Value=NA)) %>%
bind_rows(data.frame(Date=as.Date(c("2022-06-01","2022-06-01","2022-06-01","2022-06-01","2022-09-01","2022-09-01","2022-09-01","2022-09-01")),STA="STA-3/4 Cell 2B",Ecotope="Mixed",Parameter=c("DO","pH","Temp","SpCond"),Value=NA)) 

DO_mod <-  mutate(filter(Physico_parameters_long,Parameter=="DO"),Date=as.numeric(Date)) %>%
                group_by(Ecotope) %>% 
                arrange(Ecotope, Date) %>% 
                nest() %>%
                mutate(pred.response = purrr::map(data, function(x)stats::loess(Value~Date, span= 0.6, data = x))) %>%
                                                      #stats::predict(data.frame(Date = seq(min(x$Date), max(x$Date), 1))))) %>%
                unnest(cols = c(data, pred.response))
                
          
                
#DO by ecotope
DO_plot <- ggplot(filter(Physico_parameters_long,Ecotope!="Naiad",Parameter=="DO"),aes(month(Date)+day(Date)/31,Value,color=Ecotope,fill=Ecotope))+
geom_point()+  geom_smooth(se=T,span = .6)+
facet_wrap(~STA)+scale_x_discrete(limits = 1:12, labels = month.abb)+
labs(y=expression(DO~(mg~L^-1)),x=NULL)+
Presentation_theme+  guides(x =  guide_axis(angle = 40))+theme(legend.position="none", axis.text.x=element_blank())

#SpCond by ecotope
SpCond_plot <-ggplot(filter(Physico_parameters_long,Ecotope!="Naiad",Parameter=="SpCond"),aes(month(Date)+day(Date)/31,Value,color=Ecotope,fill=Ecotope))+
geom_point()+  geom_smooth(method="loess",se=T)+
facet_wrap(~STA)+scale_x_discrete(limits = 1:12, labels = month.abb)+
labs(y=expression(SpCond~(us~cm^-1)),x=NULL)+scale_y_continuous(label=comma)+coord_cartesian(ylim=c(500,1500))+
Presentation_theme+ guides(x =  guide_axis(angle = 40))+theme(legend.position="none", axis.text.x=element_blank(),strip.background = element_blank(),strip.text.x = element_blank())

#pH by ecotope
pH_plot <-ggplot(filter(Physico_parameters_long,Ecotope!="Naiad",Parameter=="pH"),aes(month(Date)+day(Date)/31,Value,color=Ecotope,fill=Ecotope))+
geom_point()+  geom_smooth(method="loess",se=T)+
facet_wrap(~STA)+scale_x_discrete(limits = 1:12, labels = month.abb)+
labs(y=expression(pH),x=NULL)+scale_y_continuous(label=comma)+coord_cartesian(ylim=c(6,10))+
Presentation_theme+  guides(x =  guide_axis(angle = 40))+theme(legend.position="none", axis.text.x=element_blank(),strip.background = element_blank(),strip.text.x = element_blank())

#Temp by ecotope
Temp_plot <-ggplot(filter(Physico_parameters_long,Ecotope!="Naiad",Parameter=="Temp"),aes(month(Date)+day(Date)/31,Value,color=Ecotope,fill=Ecotope))+
geom_point()+  geom_smooth(method="loess",se=T)+
facet_wrap(~STA)+scale_x_discrete(limits = 1:12, labels = month.abb)+
labs(y=expression(Temp~("°C")),x=NULL)+scale_y_continuous(label=comma)+#coord_cartesian(ylim=c(6,10))+
Presentation_theme+  guides(x =  guide_axis(angle = 40))+theme(strip.background = element_blank(),strip.text.x = element_blank())

plot_grid(DO_plot,SpCond_plot,pH_plot,Temp_plot,nrow=4,align="v",rel_heights = c(1, .85, .85, 1.2))

ggsave(plot = last_plot(),filename="./Figures/Physico-Chemical.jpeg",width =8.5, height =11, units = "in")


# Vegetation Change -------------------------------------------------------

#Bare site was switched after 2023-06-20 sampling to new site
ggplot(filter(Veg_tidy_long,Units=="Percentage"),aes(Date,value,color=Vegetation))+
geom_line(size=2)+#geom_vline(aes(xintercept=as.Date("2023-06-20")),color="grey80",size=2,linetype="dashed") + 
facet_wrap(~Ecotope,nrow=2)+
scale_x_date(date_breaks="2 month",labels = date_format("%b %Y"))+ 
scale_y_continuous(label=percent,limits=c(0,1))+
Presentation_theme+  guides(x =  guide_axis(angle = 40))+labs(y="Vegetation Coverage (%)")

ggsave(plot = last_plot(),filename="./Figures/Vegetation Change.jpeg",width =12, height =8, units = "in")

#Bare site was switched after 2023-06-20 sampling to new site
ggplot(filter(Veg_tidy_long,Units=="Percentage"),aes(month(Date),value,color=Vegetation))+
geom_smooth(size=2,se=FALSE)+ 
facet_wrap(~Ecotope,nrow=2)+coord_cartesian(xlim=c(1,12))+
scale_x_discrete(limits = 1:12,labels = month.abb,breaks=seq(1,12,1))+scale_y_continuous(label=percent,limits=c(0,1))+
Presentation_theme+ guides(x =  guide_axis(angle = 40))+labs(y="Vegetation Coverage (%)")

ggsave(plot = last_plot(),filename="./Figures/Vegetation Change-annual pattern.jpeg",width =12, height =8, units = "in")

