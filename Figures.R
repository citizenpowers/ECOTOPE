rm(list = ls())

library(dplyr)
library(ggplot2)
library(tidyr)
library(stringr)
library(lubridate)
library(scales)
library(RColorBrewer)
library(viridis)
library(Hmisc)
library(ggpmisc)
library(ggrepel)
library(zoo)
library(readxl)
library(readr)
library(corrplot)
library(GGally)
library(devtools)
library(ggthemr)
library(interp)
library(tidyverse)
library(stringr)
library(ggrepel)
install.packages("remotes")
remotes::install_github("cttobin/ggthemr")
# Import Data -------------------------------------------------------------


WQ_Upstream_Downstream_Tidy <- read_csv("./Data/WQ Data/WQ_Upstream_Downstream_Tidy.csv")
WQ_Data_Tidy <- read_csv("./Data/WQ Data/WQ_Data_Tidy.csv")
WQ_Data_Tidy <- read_csv("./Data/WQ Data/WQ_Provisional_Tidy.csv")   #provisional data
WQ_Data <- read_excel("Data/WQ Data/WQ Data.xlsx",sheet = "Sheet1")
WQ_Field_Data_Continuous_data <- read.csv("Data/Joined Data/WQ_Field_Data_Continuous_data.csv",check.names=FALSE)
WQ_Field_with_continuous_same_rows <- read.csv("./Data/Joined Data/WQ_Field_with_continuous_same_rows.csv",check.names=FALSE)
WQ_Field_Data <- read_csv("./Data/Joined Data/WQ_Field_Data.csv")
TP_Budget <- read_csv("./Data/P Budget/TP_Budget.csv")

# Theme -------------------------------------------------------------------

ggthemr("flat dark",type="outer", layout="scientific")   #used for presntation figs
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

#Histogram of distributions  -- Distributions approximately log normal
ggplot(WQ_Field_Data,aes(log(TPO4*1000),color=Ecotope,fill=Ecotope))+geom_histogram()+facet_wrap(~Ecotope,scale="free")+
guides(x =  guide_axis(angle = 40))+labs(y=expression(TP~(mu~g~L^-1)))

#All Analyses Concentration over time points and smooth
all_analytes_plot <-ggplot(pivot_longer(WQ_Field_Data,names_to="TEST_NAME",values_to="VALUE",6:33),aes(Date,`VALUE`,color=Ecotope,fill=Ecotope))+geom_point()+geom_smooth(se=FALSE)+
facet_wrap(~TEST_NAME,scales = "free_y")+
#scale_fill_brewer(palette = "Set2",direction = -1)+scale_color_brewer(palette = "Set2",direction = -1)+
#scale_color_pomological()+ scale_fill_pomological()  +theme_pomological_fancy()+  #pomological theme
#scale_fill_manual(values = pal("ft"))+scale_color_manual(values = pal("ft"))+ theme_ft()+ #sparkly stones theme
scale_x_date(date_breaks="3 month",labels = date_format("%b %y"))+ ylab(" ")+guides(x =  guide_axis(angle = 40))

ggsave(plot = last_plot(),filename="./Figures/All WQ Analytes over time.jpeg",width =13.333, height =7.5, units = "in")

#TPO4 Concentration over time points and smooth (STA1W and STA34) SFER 2023 Fig 1
WQ_Fig_data <- WQ_Field_Data %>%
mutate(`Figure Label Date`=case_when(STA=="STA-3/4 Cell 2B" & Date <"2021-09-15"~make_date(year=year(Date)+2,month=month(Date),day=day(Date)),
                                STA=="STA-3/4 Cell 2B" & Date >="2021-09-15" & Date <"2022-07-01"~make_date(year=year(Date)+1,month=month(Date),day=day(Date)),
                                STA=="STA-3/4 Cell 2B" & Date >"2022-07-01"~make_date(year=year(Date),month=month(Date),day=day(Date)),
                                STA=="STA-1W Cell 5B"~make_date(year=year(Date),month=month(Date),day=day(Date)))) %>%
mutate(`Study Period`=case_when(STA=="STA-3/4 Cell 2B" & Date <"2022-07-01"~"Year 1: STA-3/4",
                                STA=="STA-3/4 Cell 2B" & Date >"2022-07-01"~"Year 2: STA-3/4",
                                STA=="STA-1W Cell 5B"~"Year 2: STA-1W"))  


ggplot(filter(WQ_Fig_data,Position=="Downstream",Ecotope!="Naiad",STA=="STA-3/4 Cell 2B"),aes(`Figure Label Date`,TPO4*1000,color=Ecotope,fill=Ecotope,linetype=Ecotope))+
geom_point(aes(`Figure Label Date`,TPO4*1000,color=Ecotope,fill=Ecotope),size=3)+
geom_smooth(se=FALSE)+facet_wrap(~factor(`Study Period`,levels = c("Year 1: STA-3/4","Year 2: STA-3/4","Year 2: STA-1W")),nrow=3,scales = "free_y")+
geom_hline(aes(yintercept = 13),color="#785d37",linetype="longdash")+
scale_shape_manual(values = c(21:24)) +#coord_cartesian(ylim=c(0,80))+#scale_y_continuous(breaks=seq(0,100,10),limits=c(0,100))+
scale_x_date(date_breaks="1 month",labels = date_format("%b"))+ 
guides(x =  guide_axis(angle = 40))+labs(y=expression(TP~(mu~g~L^-1)),x="")+Presentation_theme

ggsave(plot = last_plot(),filename="./Figures/TPO4 over time-flat dark.jpeg",width =13.333, height =7.5, units = "in")

ggsave(plot = last_plot(),filename="./Figures/TPO4 over time-SFER 2023.jpeg",width =8, height =6, units = "in")

#TPO4 Concentration over time points and smooth ( STA34)
ggplot(filter(WQ_Data_Tidy,Position=="Downstream",TEST_NAME=="TPO4",Ecotope!="Naiad",STA=="STA-3/4 Cell 2B"),aes(Date,VALUE*1000,color=Ecotope,fill=Ecotope,linetype=Ecotope))+
geom_point(aes(Date,VALUE*1000,color=Ecotope,fill=Ecotope),size=4)+
geom_smooth(se=FALSE)+facet_wrap(~STA,nrow=2)+geom_hline(aes(yintercept = 13),color="white",linetype="dashed")+
scale_shape_manual(values = c(21:24)) + #scale_y_continuous(breaks=seq(0,100,10),limits=c(0,100))+Presentation_theme+
scale_x_date(date_breaks="1 month",labels = date_format("%b %y"))+guides(x =  guide_axis(angle = 40))+labs(y=expression(TP~(mu~g~L^-1)))

ggsave(plot = last_plot(),filename="./Figures/TPO4 over time-flat dark- STA34 only WY22.jpeg",width =13.333, height =7.5, units = "in")


# SFER Tables -------------------------------------------------------------

#SFER table summary
WQ_Field_Data %>%
group_by(STA,Ecotope) %>%
filter(Date>"2022-06-01")  %>%
filter(Position=="Downstream") %>%
summarise(n=n(),Samples=sum(!is.na(TPO4)),`Mean TP`=mean(TPO4,na.rm=T),`Median TP`=median(TPO4,na.rm=T))


#TPO4 boxplots
TPO4_boxplot <-ggplot(filter(WQ_Field_Data,Position=="Downstream"),aes(Ecotope,TPO4*1000,fill=Ecotope))+geom_boxplot()+
scale_y_continuous(breaks=seq(0,40,5),limits=c(0,40))+ guides(x =  guide_axis(angle = 40))+labs(y=expression(TP~(mu~g~L^-1)))+
Presentation_theme

ggsave(plot = last_plot(),filename="./Figures/TPO4 Boxplot-flat dark.jpeg",width =13.333, height =7.5, units = "in")

#All Analytes Concentration boxplots
ggplot(pivot_longer(WQ_Field_Data,names_to="TEST_NAME",values_to="VALUE",6:33),aes(Ecotope,`VALUE`,fill=Ecotope))+geom_boxplot(color="black")+
facet_wrap(~TEST_NAME,scales = "free_y")+scale_fill_brewer(palette = "Set2",direction = -1)+theme_bw()+scale_color_brewer(palette = "Set2",direction = -1)

#All Analytes Differences (Up-Down)- points and smooth 
ggplot(pivot_longer(WQ_Field_Diff_Data,names_to="TEST_NAME",values_to="VALUE",34:61),aes(Date,`VALUE`,color=Ecotope,fill=Ecotope))+geom_point()+geom_smooth(se=FALSE)+
facet_wrap(~TEST_NAME,scales = "free_y")+scale_fill_brewer(palette = "Set2",direction = -1)+scale_color_brewer(palette = "Set2",direction = -1)+theme_bw()

#All analytes Differences (Up-Down)- columns
ggplot(filter(pivot_longer(WQ_Field_Data,names_to="TEST_NAME",values_to="VALUE",6:33),TEST_NAME=="TPO4"),aes(Date,`VALUE`,color=Ecotope,fill=Ecotope))+geom_col(position = "dodge")+#geom_line(size=1)+geom_point()+
scale_fill_brewer(palette = "Set2",direction = -1)+scale_color_brewer(palette = "Set2",direction = -1)+theme_bw()

#TPO4 only Differences (Up-Down)- points and smooth
ggplot(filter(pivot_longer(WQ_Field_Diff_Data,names_to="TEST_NAME",values_to="VALUE",34:61),TEST_NAME=="Dif TPO4"),aes(Date,VALUE,color=Ecotope,fill=Ecotope))+geom_smooth(size=1,se=FALSE)+geom_point()+
scale_fill_brewer(palette = "Set2",direction = -1)+scale_color_brewer(palette = "Set2",direction = -1)+theme_bw()

#All analytes Differences (Up-Down)- Box plots
ggplot(pivot_longer(WQ_Field_Diff_Data,names_to="TEST_NAME",values_to="VALUE",34:61),aes(Ecotope,VALUE,fill=Ecotope))+geom_boxplot()+geom_hline(yintercept=0)+
facet_wrap(~TEST_NAME,scales = "free_y")+scale_fill_brewer(palette = "Set2",direction = -1)+scale_color_brewer(palette = "Set2",direction = -1)+theme_bw()

#All analytes Differences (Up-Down) by magnitude of average (Up+dowwn)/2- Points and smooth
ggplot(WQ_Upstream_Downstream_Tidy,aes((`Upstream Values`+`Downstream Values`)/2,`Difference`,fill=Ecotope,color=Ecotope))+geom_point(shape=21)+geom_smooth(se=FALSE)+geom_hline(yintercept=0)+
facet_wrap(~TEST_NAME,scales = "free")+scale_fill_brewer(palette = "Set2",direction = -1)+scale_color_brewer(palette = "Set2",direction = -1)+theme_bw()


# Water depth vs TP -----------------------------------------------------

#Depth over time with TP differences
ggplot(select(WQ_Field_Data_Continuous_data,`Average DCS (Field Data)`,`DCS Levelogger`,`Date Time`,Position,Ecotope,`Dif TPO4`),aes(ymd_hms(`Date Time`),`Average DCS (Field Data)`,fill=Position))+geom_point(shape=21,size=2)+
geom_line(aes(ymd_hms(`Date Time`),`DCS Levelogger`*100))+
geom_line(aes(ymd_hms(`Date Time`),`Dif TPO4`*10000),color="red")+ 
facet_wrap(~Ecotope)+
scale_x_datetime(date_breaks="1 month",labels = date_format("%b %y"),limits = as.POSIXct(c("2021-06-01 00:00:00","2022-05-31 00:00:00")))+
scale_fill_brewer(palette = "Set2",direction = -1)+scale_color_brewer(palette = "Set2",direction = -1)+theme_bw()

#DCS Depth vs TP 
ggplot(filter(WQ_Field_with_continuous_same_rows,Position=="Downstream",Ecotope!="Naiad",STA=="STA-3/4 Cell 2B",Date<"2022-07-02"),aes(`DCS (Field Data)`,`TPO4`*1000,fill=Ecotope,color=Ecotope))+
facet_wrap(~Ecotope)+scale_y_continuous(breaks=seq(0,80,10))+Presentation_theme2+coord_cartesian(ylim=c(0,50))+
geom_rect(aes(xmin = 45.72, ymin = -Inf, xmax = 76.2, ymax = 80),alpha=.5,fill="#e0ecf4",color="#e0ecf4")+geom_point(shape=21,size=2.5,color="grey70")+geom_smooth()+
ylab(expression(TP~(mu~g~L^-1)))+xlab("Depth to Consolidated Substrate (cm)")+guides(fill="none",color="none")

ggsave(plot = last_plot(),filename="./Figures/TPO4 vs Water Depth 2023 SFER.jpeg",width =8, height =6, units = "in")

#DCS Depth vs TP wet season vs dry seeason
ggplot(filter(Wet_vs_dry,Position=="Downstream",Ecotope!="Naiad",STA=="STA-3/4 Cell 2B",Date<"2023-07-02"),aes(`DCS (Field Data)`,`TPO4`*1000,fill=Ecotope,color=Ecotope))+
facet_grid(Season~Ecotope)+scale_y_continuous(breaks=seq(0,80,10))+Presentation_theme2+coord_cartesian(ylim=c(0,50))+
geom_rect(aes(xmin = 45.72, ymin = -Inf, xmax = 76.2, ymax = 80),alpha=.5,fill="#e0ecf4",color="#e0ecf4")+
geom_point(shape=21,size=2.5,color="grey70")+geom_smooth(span=10)+
geom_hline(yintercept = 13,linetype="longdash",color="#785d37")+
ylab(expression(TP~(mu~g~L^-1)))+xlab("Depth to Consolidated Substrate (cm)")+guides(fill="none",color="none")

ggsave(plot = last_plot(),filename="./Figures/TPO4 vs Water Depth wet vs dry season 2023 SFER.jpeg",width =8, height =6, units = "in")



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
ggplot(filter(WQ_Field_with_continuous_same_rows,Position=="Downstream",Ecotope!="Naiad",STA=="STA-3/4 Cell 2B",Date<"2023-07-02"),aes(`Mean inflow (cfs)`,`TPO4`*1000,fill=Ecotope,color=Ecotope))+geom_point(shape=21,size=2,color="grey70")+
facet_wrap(~Ecotope)+scale_y_continuous(breaks=seq(0,100,10))+Presentation_theme2+coord_cartesian(ylim=c(0,80))+
geom_ribbon(stat='smooth', method = "loess", se=TRUE, alpha=0.3) +
geom_line(stat='smooth', method = "loess")  + 
ylab(expression(TP~(mu~g~L^-1)))+xlab("Outflow (cfs)")+guides(fill="none",color="none")

#outflow vs TPO4 in STA3/4
ggplot(filter(WQ_Field_with_continuous_same_rows,Position=="Downstream",Ecotope!="Naiad",STA=="STA-3/4 Cell 2B",Date<"2023-07-02"),aes(`Mean outflow (cfs)`,`TPO4`*1000,fill=Ecotope,color=Ecotope))+geom_point(shape=21,size=2.5,color="grey70")+
facet_wrap(~Ecotope)+scale_y_continuous(breaks=seq(0,100,10))+Presentation_theme2+scale_x_continuous(breaks=seq(0,1200,200))+
geom_ribbon(stat='smooth', method = "loess", se=TRUE, alpha=0.3) +coord_cartesian(ylim=c(0,50),xlim=c(50,1300))+
geom_line(stat='smooth', method = "loess")  + geom_hline(yintercept = 13,linetype="longdash",color="#785d37")+
ylab(expression(TP~(mu~g~L^-1)))+xlab("Outflow (cfs)")+guides(fill="none",color="none")

ggsave(plot = last_plot(),filename="./Figures/TPO4 vs Outflow 2023 SFER.jpeg",width =8, height =6, units = "in")

#Flow vs TP wet seaon and dry season
Wet_vs_dry <- WQ_Field_with_continuous_same_rows %>%
mutate(Season=if_else(between(month(Date),6,11)==TRUE,"Wet Season","Dry Season"))  

ggplot(filter(Wet_vs_dry,Position=="Downstream",Ecotope!="Naiad",STA=="STA-3/4 Cell 2B",Date<"2023-07-02"),aes(`Mean outflow (cfs)`,`TPO4`*1000,fill=Ecotope,color=Ecotope))+geom_point(shape=21,size=2.5,color="grey70")+
facet_grid(Season~Ecotope)+scale_y_continuous(breaks=seq(0,100,10))+Presentation_theme2+scale_x_continuous(breaks=seq(0,1200,200))+
geom_ribbon(stat='smooth', method = "loess", se=TRUE, alpha=0.3) +coord_cartesian(ylim=c(0,50),xlim=c(50,1300))+
geom_line(stat='smooth', method = "loess")  + geom_hline(yintercept = 13,linetype="longdash",color="#785d37")+
ylab(expression(TP~(mu~g~L^-1)))+xlab("Outflow (cfs)")+guides(fill="none",color="none")

ggsave(plot = last_plot(),filename="./Figures/TPO4 vs Outflow 2023 SFER.jpeg",width =8, height =6, units = "in")






#outflow vs TPO4 in STA1W (not enough data to use yet)
ggplot(filter(WQ_Field_with_continuous_same_rows,Position=="Downstream",Ecotope!="Naiad",STA=="STA-1W Cell 5B",Date<"2023-07-02"),aes(`Mean outflow (cfs)`,`TPO4`*1000,fill=Ecotope,color=Ecotope))+geom_point(shape=21,size=2,color="grey70")+
facet_wrap(~Ecotope)+scale_y_continuous(breaks=seq(0,100,10))+Presentation_theme2+scale_x_continuous(breaks=seq(0,1200,200))+
geom_ribbon(stat='smooth', method = "loess", se=TRUE, alpha=0.3) +coord_cartesian(ylim=c(0,80),xlim=c(50,1300))+
geom_line(stat='smooth', method = "loess")  + geom_hline(yintercept = 13,linetype="longdash",color="#785d37")+
ylab(expression(TP~(mu~g~L^-1)))+xlab("Outflow (cfs)")+guides(fill="none",color="none")

ggsave(plot = last_plot(),filename="./Figures/TPO4 vs Outflow 2023 SFER.jpeg",width =8, height =6, units = "in")


# Water Depth vs Flow and TP ----------------------------------------------
#Remove NAs
contour_data <- filter(WQ_Field_with_continuous_same_rows,Ecotope!="Naiad",STA=="STA-3/4 Cell 2B",Position=="Downstream") %>%
drop_na(TPO4) %>%
drop_na(`DCS (Field Data)`) %>%
drop_na(`Mean outflow (cfs)`) 

#Interpolate to grid from All stas
contour_grid <- with(contour_data, interp::interp(`DCS (Field Data)`, `Mean outflow (cfs)`, TPO4*1000,duplicate="mean"))
griddf <- subset(data.frame(`Depth` = rep(contour_grid$x, nrow(contour_grid$z)),
                            `CFS`= rep(contour_grid$y, each = ncol(contour_grid$z)),
                            z = as.numeric(contour_grid$z)),!is.na(z)) %>%
rename(`DCS (Field Data)`="Depth",`Mean outflow (cfs)`="CFS",TPO4="z")

ggplot(griddf, aes(`DCS (Field Data)`,`Mean outflow (cfs)` , z = TPO4)) +
geom_contour_filled(binwidth =2,na.fill = TRUE) +  xlab("Depth to Consolidated Substrate (cm)")+ylab("Outflow (cfs)")+Presentation_theme+theme(legend.position="right")+
geom_point(data = contour_data, aes(`DCS (Field Data)`,`Mean outflow (cfs)`))+guides(fill=guide_legend(title=expression(TP~(mu~g~L^-1))))

ggsave(plot = last_plot(),filename="./Figures/TPO4 vs Flow vs Depth-SFER.jpeg",width =8, height =5.5, units = "in")

ggsave(plot = last_plot(),filename="./Figures/TPO4 vs Flow vs Depth.jpeg",width =13.333, height =7.5, units = "in")

#figs for individual ecotopes and all stas
contour_data_chara <- filter(WQ_Field_with_continuous_same_rows,Ecotope=="Chara",Position=="Downstream",between(month(Date),5,11),TPO4<.050) %>% drop_na(TPO4) %>% drop_na(`DCS (Field Data)`) %>% drop_na(`Mean outflow (cfs)`) 
contour_data_typha <- filter(WQ_Field_with_continuous_same_rows,Ecotope=="Typha",Position=="Downstream",between(month(Date),5,11),TPO4<.050) %>% drop_na(TPO4) %>% drop_na(`DCS (Field Data)`) %>% drop_na(`Mean outflow (cfs)`) 
contour_data_mixed <- filter(WQ_Field_with_continuous_same_rows,Ecotope=="Mixed",Position=="Downstream",between(month(Date),5,11),TPO4<.050) %>% drop_na(TPO4) %>% drop_na(`DCS (Field Data)`) %>% drop_na(`Mean outflow (cfs)`) 
contour_data_bare <- filter(WQ_Field_with_continuous_same_rows,Ecotope=="Bare",Position=="Downstream",between(month(Date),5,11),TPO4<.050) %>% drop_na(TPO4) %>% drop_na(`DCS (Field Data)`) %>% drop_na(`Mean outflow (cfs)`) 
contour_grid_chara <- with(contour_data_chara, interp::interp(`DCS (Field Data)`, `Mean outflow (cfs)`, TPO4*1000,duplicate="mean"))
contour_grid_typha <- with(contour_data_typha, interp::interp(`DCS (Field Data)`, `Mean outflow (cfs)`, TPO4*1000,duplicate="mean"))
contour_grid_mixed <- with(contour_data_mixed, interp::interp(`DCS (Field Data)`, `Mean outflow (cfs)`, TPO4*1000,duplicate="mean"))
contour_grid_bare <- with(contour_data_bare, interp::interp(`DCS (Field Data)`, `Mean outflow (cfs)`, TPO4*1000,duplicate="mean"))
griddf_chara <- subset(data.frame(`Depth` = rep(contour_grid_chara$x, nrow(contour_grid_chara$z)), `CFS`= rep(contour_grid_chara$y, each = ncol(contour_grid_chara$z)), z = as.numeric(contour_grid_chara$z)),!is.na(z)) %>%rename(`DCS (Field Data)`="Depth",`Mean outflow (cfs)`="CFS",TPO4="z")
griddf_typha <- subset(data.frame(`Depth` = rep(contour_grid_typha$x, nrow(contour_grid_typha$z)), `CFS`= rep(contour_grid_typha$y, each = ncol(contour_grid_typha$z)), z = as.numeric(contour_grid_typha$z)),!is.na(z)) %>%rename(`DCS (Field Data)`="Depth",`Mean outflow (cfs)`="CFS",TPO4="z")
griddf_mixed <- subset(data.frame(`Depth` = rep(contour_grid_mixed$x, nrow(contour_grid_mixed$z)), `CFS`= rep(contour_grid_mixed$y, each = ncol(contour_grid_mixed$z)), z = as.numeric(contour_grid_mixed$z)),!is.na(z)) %>%rename(`DCS (Field Data)`="Depth",`Mean outflow (cfs)`="CFS",TPO4="z")
griddf_bare <- subset(data.frame(`Depth` = rep(contour_grid_bare$x, nrow(contour_grid_bare$z)), `CFS`= rep(contour_grid_bare$y, each = ncol(contour_grid_bare$z)), z = as.numeric(contour_grid_bare$z)),!is.na(z)) %>%rename(`DCS (Field Data)`="Depth",`Mean outflow (cfs)`="CFS",TPO4="z")

ggplot(griddf_chara, aes(`DCS (Field Data)`,`Mean outflow (cfs)` , z = TPO4)) +
geom_contour_filled(binwidth =2,na.fill = TRUE) +  xlab("Depth to Consolidated Substrate (cm)")+ylab("Outflow (cfs)")+Presentation_theme+theme(legend.position="right")+
geom_point(data = contour_data_chara, aes(`DCS (Field Data)`,`Mean outflow (cfs)`))+guides(fill=guide_legend(title=expression(TP~(mu~g~L^-1))))

ggplot(griddf_typha, aes(`DCS (Field Data)`,`Mean outflow (cfs)` , z = TPO4)) +
geom_contour_filled(binwidth =2,na.fill = TRUE) +  xlab("Depth to Consolidated Substrate (cm)")+ylab("Outflow (cfs)")+Presentation_theme+theme(legend.position="right")+
geom_point(data = contour_data_typha, aes(`DCS (Field Data)`,`Mean outflow (cfs)`))+guides(fill=guide_legend(title=expression(TP~(mu~g~L^-1))))

ggplot(griddf_mixed, aes(`DCS (Field Data)`,`Mean outflow (cfs)` , z = TPO4)) +
geom_contour_filled(binwidth =2,na.fill = TRUE) +  xlab("Depth to Consolidated Substrate (cm)")+ylab("Outflow (cfs)")+Presentation_theme+theme(legend.position="right")+
geom_point(data = contour_data_mixed, aes(`DCS (Field Data)`,`Mean outflow (cfs)`))+guides(fill=guide_legend(title=expression(TP~(mu~g~L^-1))))

ggplot(griddf_bare, aes(`DCS (Field Data)`,`Mean outflow (cfs)` , z = TPO4)) +
geom_contour_filled(binwidth =2,na.fill = TRUE) +  xlab("Depth to Consolidated Substrate (cm)")+ylab("Outflow (cfs)")+Presentation_theme+theme(legend.position="right")+
geom_point(data = contour_data_bare, aes(`DCS (Field Data)`,`Mean outflow (cfs)`))+guides(fill=guide_legend(title=expression(TP~(mu~g~L^-1))))

#Figs wet season vs dry season 
contour_data_wet <- filter(WQ_Field_with_continuous_same_rows,Ecotope!="Naiad",Position=="Downstream",between(month(Date),5,11)) %>% drop_na(TPO4) %>% drop_na(`DCS (Field Data)`) %>% drop_na(`Mean outflow (cfs)`) 
contour_data_dry <- filter(WQ_Field_with_continuous_same_rows,Ecotope!="Naiad",Position=="Downstream",!between(month(Date),5,11)) %>% drop_na(TPO4) %>% drop_na(`DCS (Field Data)`) %>% drop_na(`Mean outflow (cfs)`) 
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



# TP vs physico-chemical parameters ----------------------------------------
ggplot(filter(WQ_Field_with_continuous_same_rows,Position=="Downstream",Ecotope!="Naiad",STA=="STA-3/4 Cell 2B",Date<"2022-06-02"),aes(`Temp`,`TPO4`*1000,color=Ecotope,fill=Ecotope))+geom_smooth()+geom_point(shape=21,size=2,color="grey70")+
facet_wrap(~Ecotope)+scale_y_continuous(breaks=seq(0,80,10))+Presentation_theme2+coord_cartesian(ylim=c(0,40))+
ylab(expression(TP~(mu~g~L^-1)))+xlab("Temp (C)")+guides(fill="none",color="none")

ggsave(plot = last_plot(),filename="./Figures/TPO4 vs Temp.jpeg",width =5, height =6, units = "in")


ggplot(WQ_Field_Data_Continuous_data,aes(SpCond,`TPO4`))+geom_point(shape=21,size=2)+geom_smooth()+
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




# TP Budget Figures -------------------------------------------------------

Cumulative_TP <- TP_Budget %>%
pivot_longer(25:31,names_to="Ecotope",values_to="Value") %>%
filter(row_number() %% 4==1) %>% #Select every 4th row 
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
                            Ecotope=="Cumulative TP Mixed"~"Naiad/Chara Mix",
                            Ecotope=="Cumulative TP G379D"~"G379D",
                            Ecotope=="Cumulative TP G379B"~"G379B")) %>%  
mutate(`x axis`=ymd_hms(`x axis`))  

#All Analyses Concentration over time points and smooth
ggplot(filter(Cumulative_TP,Ecotope %in% c("Chara","Bare","Typha","Naiad/Chara Mix")),aes(`Date Time`,`Value`,color=Ecotope))+geom_text(aes(`x axis`,`y axis`,color=Ecotope,label=Ecotope)) +
scale_y_continuous(sec.axis = sec_axis(~., name = "Outflow (cfs)"))+geom_line()+geom_line(aes(`Date Time`,`Outflow (cfs)`),color="white",linetype="dashed")+
theme(legend.position="bottom",axis.text=element_text(size=14),axis.title = element_text(size = 16),legend.text = element_text(size = 14),legend.title = element_text(size = 16))+  
scale_x_datetime(date_breaks="3 month",labels = date_format("%b %y"))+ ylab("P (kg)")+guides(x =  guide_axis(angle = 40),position="bottom")

ggsave(plot = last_plot(),filename="./Figures/Cumulative TP.jpeg",width =13.333, height =7.5, units = "in")

ggsave(plot = last_plot(),filename="./Figures/Cumulative TP-SFER.jpeg",width =8, height =5.5, units = "in")





# TP Differences ----------------------------------------------------------
#Table with summary stats
TP_Diff_Summary_Stats <- filter(WQ_Upstream_Downstream_Tidy,TEST_NAME=="TPO4") %>%
group_by(Ecotope) %>%  
summarise(n(),`Median diff`=median(Difference,na.rm=TRUE),`mean diff`=mean(Difference,na.rm=TRUE),`mean upstream`=mean(`Upstream Values`,na.rm=TRUE),`mean downstream`=mean(`Downstream Values`,na.rm=TRUE))


#TPO4 Differences
ggplot(filter(WQ_Upstream_Downstream_Tidy,TEST_NAME=="TPO4"),aes(Ecotope,`Difference`*1000,fill=Ecotope))+
scale_y_continuous(breaks=seq(-50,50,5),limits=c(-10,10))+ geom_hline(aes(yintercept = 0),linetype="dashed",color="white")+geom_boxplot()+Presentation_theme2+
ylab(expression(Upstream-Downstream~TP~(mu~g~L^-1)))+xlab("Ecotope")+guides(fill="none",color="none")

ggsave(plot = last_plot(),filename="./Figures/TPO4 Differences Upstream-Downstream.jpeg",width =5, height =5, units = "in")

# TP Differences over time
ggplot(filter(WQ_Upstream_Downstream_Tidy,TEST_NAME=="TPO4"),aes(Date,`Difference`*1000,fill=Ecotope,color=Ecotope))+
geom_point(aes(Date,Difference*1000,color=Ecotope,fill=Ecotope),size=2,shape=21)+
geom_smooth(se=FALSE)+scale_x_date(date_breaks="1 month",labels = date_format("%b %y"))+guides(x =  guide_axis(angle = 40))+labs(y=expression(TP~(mu~g~L^-1)))

# #TP forms over time -----------------------------------------------------
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

ggsave(plot = last_plot(),filename="./Figures/P Forms over time- Monthly.jpeg",width =13.333, height =7.5, units = "in")

#DOP over time
ggplot(filter(TP_forms,STA=="STA-3/4 Cell 2B",Ecotope!="Naiad"),aes(month(Date)+day(Date)/31,Value*1000,color=`P Form`,fill=`P Form`))+geom_point()+
geom_ribbon(stat='smooth', method = "loess", se=TRUE, alpha=0.3) +
geom_line(stat='smooth', method = "loess")  + theme(axis.text.x=element_text(size=rel(0.75)))+
facet_wrap(~Ecotope,nrow=1)+scale_fill_discrete()+Presentation_theme + scale_x_discrete(limits = 1:12, labels = month.abb)+
guides(x =  guide_axis(angle = 40))+labs(y=expression(P~(mu~g~L^-1)),x="Month")+coord_cartesian(ylim = c(0, 30))

ggsave(plot = last_plot(),filename="./Figures/P Forms over time- Points and smooth.jpeg",width =13.333, height =7.5, units = "in")





# FWM Weight by Month ----------------------------------------------------
Outflow_by_Month <-Water_budget %>%
mutate(Month=month(`Date Time`,abbr = T,label=T),Year=year(`Date Time`),Date=as.Date(`Date Time`)) %>%
mutate(`Study Period`=case_when(Date <"2022-06-01"~"Year 1: STA-3/4",
                                Date >"2022-09-01" & Date <"2023-06-30"~"Year 2: STA-3/4"))   %>% 

group_by(`Study Period`,Month) %>%
summarise(n(),`Mean Outflow (cfs)`=-1*mean(`Outflow (cfs)`,na.rm=TRUE))

ggplot(Outflow_by_Month,aes(Month,`Mean Outflow (cfs)`,fill=`Study Period`,color=`Study Period`))+geom_col(position = position_dodge2(width = 0.98, preserve = "single"), binwidth=0) 


TP_Load_by_day <-TP_Budget %>%
mutate(Date=as.Date(`Date Time`)) %>%
mutate(`Figure Label Date`=case_when( Date <"2021-09-15"~make_date(year=year(Date)+2,month=month(Date),day=day(Date)),
                                      Date >="2021-09-15" & Date <"2022-07-01"~make_date(year=year(Date)+1,month=month(Date),day=day(Date)),
                                      Date >"2022-07-01"~make_date(year=year(Date),month=month(Date),day=day(Date)))) %>%
mutate(Day=day(`Figure Label Date`),Month=month(`Figure Label Date`,abbr = T,label=T)) %>%
mutate(`Study Period`=case_when(Date <"2022-06-01"~"Year 1: STA-3/4",
                                Date >"2022-09-01" & Date <"2023-06-30"~"Year 2: STA-3/4"))   %>% 
group_by(`Study Period`,`Figure Label Date`) %>%
summarise(n(),`Daily Load Chara (kg)`=sum(`Load Chara`,na.rm=TRUE),`Daily Load Bare (kg)`=sum(`Load Bare`,na.rm=TRUE),`Daily Load Typha (kg)`=sum(`Load Typha`,na.rm=TRUE),`Daily Load Mixed (kg)`=sum(`Load Mixed`,na.rm=TRUE)) %>%
pivot_longer(names_to = "Ecotope",values_to = "TP Load (kg)",4:7) %>%
drop_na(`Study Period`)

ggplot(TP_Load_by_day,aes(`Figure Label Date`,`TP Load (kg)`,fill=`Study Period`,color=`Study Period`))+geom_area(alpha=.7,position="dodge")+facet_wrap(~Ecotope,nrow=2)+
scale_x_date(date_breaks="1 month",labels = date_format("%b"))+labs(x="")+coord_cartesian(xlim = as.Date(c("2022-09-30","2023-09-01")))+
guides(x =  guide_axis(angle = 40),position="bottom")+theme(legend.position="bottom")

ggsave(plot = last_plot(),filename="./Figures/Seasonal TP laod by Ecotope.jpeg",width =8, height =6, units = "in")

FWM_by_Month <-TP_Budget %>%
mutate(Month=month(`Date Time`,abbr = T,label=T),Year=year(`Date Time`),Date=as.Date(`Date Time`)) %>%
mutate(`Study Period`=case_when(Date <"2022-06-01"~"Year 1: STA-3/4",
                                 Date >"2022-09-01" & Date <"2023-06-30"~"Year 2: STA-3/4"))   %>% 
select(1:2,11:24,39:42,-`Naiad Int`) %>%
group_by(`Study Period`,Month) %>%
summarise(n(),`Monthly Total Outflow`=sum(`Outflow (cfs)`,na.rm=TRUE),`Chara TP Sum`=sum(`Load Chara`,na.rm=TRUE),`Typha TP Sum`=sum(`Load Typha`,na.rm=TRUE),`Bare TP Sum`=sum(`Load Bare`,na.rm=TRUE),`Mixed TP Sum`=sum(`Load Mixed`,na.rm=TRUE))  %>%
pivot_longer(names_to ="Ecotope",values_to = "Load TP",5:8) %>%
mutate(`Monthly FWM TP`=`Load TP`/(`Monthly Total Outflow`*28.316847*60)*1000000) %>%
drop_na(`Study Period`) %>%
mutate(Season=if_else(Month %in% c("Jun","Jul","Aug","Sep","Oct","Nov")==TRUE,"Wet Season","Dry Season"))
  
ggplot(FWM_by_Month,aes(Month,`Monthly FWM TP`*1000,fill=`Ecotope`))+geom_col(position = "dodge")+
facet_wrap(~`Study Period`,nrow=2,scales = "free_y")+geom_hline(aes(yintercept = 13),color="#785d37",linetype="longdash")+
guides(x =  guide_axis(angle = 40))+labs(y=expression(FWMC-TP~(mu~g~L^-1)))+Presentation_theme

FWM_by_Day <-TP_Budget %>%
mutate(Month=month(`Date Time`,abbr = T,label=T),Year=year(`Date Time`),Date=as.Date(`Date Time`)) %>%
mutate(`Study Period`=case_when(Date <"2022-06-01"~"Year 1: STA-3/4", Date >"2022-09-01" & Date <"2023-06-30"~"Year 2: STA-3/4"))   %>% 
mutate(`Date`=case_when( Date <"2021-09-15"~make_date(year=year(Date)+2,month=month(Date),day=day(Date)),
                                        Date >="2021-09-15" & Date <"2022-07-01"~make_date(year=year(Date)+1,month=month(Date),day=day(Date)),
                                        Date >"2022-07-01"~make_date(year=year(Date),month=month(Date),day=day(Date)))) %>%
select(1:2,11:24,39:42,-`Naiad Int`) %>%
group_by(`Study Period`,Date) %>%
summarise(n(),`Daily Total Outflow`=sum(`Outflow (cfs)`,na.rm=TRUE),`Chara TP Sum`=sum(`Load Chara`,na.rm=TRUE),`Typha TP Sum`=sum(`Load Typha`,na.rm=TRUE),`Bare TP Sum`=sum(`Load Bare`,na.rm=TRUE),`Mixed TP Sum`=sum(`Load Mixed`,na.rm=TRUE))  %>%
pivot_longer(names_to ="Ecotope",values_to = "Load TP",5:8) %>%
mutate(`Daily FWM TP`=if_else(`Daily Total Outflow`==0,0,`Load TP`/(`Daily Total Outflow`*28.316847*60)*1000000000)) %>%
drop_na(`Study Period`) %>%
mutate(Season=if_else(between(month(Date),6,11)==TRUE,"Wet Season","Dry Season"))

FWM_Table_month <- FWM_by_Day %>%
mutate(Month=month(Date)) %>%
group_by(`Study Period`,Month,Ecotope) %>%
summarise(n(),`Monthly FWM TP`=mean(`Daily FWM TP`,na.rm=TRUE),`Standard Deviation`=sd(`Daily FWM TP`,na.rm=TRUE))


FWM_Table_Season <- FWM_by_Day %>%
group_by(`Study Period`,Season,Ecotope) %>%
summarise(n(),`Season FWM TP`=mean(`Daily FWM TP`,na.rm=TRUE),`Standard Deviation`=sd(`Daily FWM TP`,na.rm=TRUE))

ggplot(FWM_by_Day,aes(Date,`Daily FWM TP`,fill=`Ecotope`,color=`Ecotope`))+geom_point(shape=21)+
facet_wrap(~`Study Period`,nrow=2,scales = "free_y")+geom_hline(aes(yintercept = 13),color="#785d37",linetype="longdash")+
scale_x_date(date_breaks="1 month",labels = date_format("%b"))+
guides(x =  guide_axis(angle = 40))+labs(y=expression(FWMC-TP~(mu~g~L^-1)))+Presentation_theme

ggplot(FWM_by_Day,aes(month(Date,label = TRUE),`Daily FWM TP`,fill=`Ecotope`,color=`Ecotope`))+geom_jitter(shape=21,height =0)+
facet_wrap(~`Study Period`,nrow=2,scales = "free_y")+geom_hline(aes(yintercept = 13),color="#785d37",linetype="longdash")+
guides(x =  guide_axis(angle = 40))+labs(y=expression(FWMC-TP~(mu~g~L^-1)))+Presentation_theme

ggplot(FWM_by_Day,aes(month(Date,label = TRUE),`Daily FWM TP`,fill=`Ecotope`,color=`Ecotope`))+geom_col(position = "dodge")+ 
geom_errorbar(data=FWM_Table_month ,aes(Month,group=Ecotope,ymax=max(`Monthly FWM TP`+`Standard Deviation`), ymin=min(`Monthly FWM TP`-`Standard Deviation`)),color="#785d37",inherit.aes = FALSE,position="dodge")+
facet_wrap(~`Study Period`,nrow=2,scales = "free_y")+geom_hline(aes(yintercept = 13),color="#785d37",linetype="longdash")+
guides(x =  guide_axis(angle = 40))+labs(y=expression(FWMC-TP~(mu~g~L^-1)))+Presentation_theme

