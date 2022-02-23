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
library(corrplot)
library(GGally)

# Import Data -------------------------------------------------------------


WQ_Data <- read_excel("Data/WQ Data/WQ Data.xlsx",sheet = "Sheet1")



# Tidy Data ---------------------------------------------------------------

WQ_Data_Tidy <-WQ_Data  %>%
filter(STATION!="OCS",SAMPLE_TYPE=="SAMP") %>%  
mutate(date=as.Date(COLLECT_DATE)) %>%
mutate(Ecotope=case_when(str_detect(STATION,"STA34C2B_C")~"Chara",
                            str_detect(STATION,"STA34C2B_T")~"Typha",
                            str_detect(STATION,"STA34C2B_N")~"Naiad",
                            str_detect(STATION,"STA34C2B_M")~"Mixed",
                            str_detect(STATION,"STA34C2B_B")~"Bare"))  %>%
mutate(Position=case_when(str_detect(STATION,"DWN")~"Downstream",
                          str_detect(STATION,"Up")~"Upstream"))

WQ_Upstream <- WQ_Data_Tidy  %>%
filter(Position=="Upstream") %>%
mutate(`Upstream Values`=VALUE)  %>%
select(date,Ecotope,TEST_NAME,`Upstream Values`)

WQ_DownStream <- WQ_Data_Tidy  %>%
filter(Position=="Downstream") %>%
mutate(`Downstream Values`=VALUE) %>%
select(date,Ecotope,TEST_NAME,`Downstream Values`) 

WQ_Upstream_Downstream_Tidy <- WQ_Upstream %>%
left_join(WQ_DownStream,by=c("date","Ecotope","TEST_NAME"))  %>%
mutate(`Difference`=`Upstream Values`-`Downstream Values`)


# Save Data ---------------------------------------------------------------


write.csv(WQ_Upstream_Downstream_Tidy, "./Data/WQ Data/WQ_Upstream_Downstream_Tidy.csv",row.names = FALSE)


# QC Blank Evaluation -----------------------------------------------------

QC_Blanks_Tidy <-WQ_Data  %>%
filter(SAMPLE_TYPE=="FCEB") %>%
group_by(TEST_NAME,REMARK_CODE) %>%
summarise(n=n())
  

# Contamination Evaluation ------------------------------------------------

Sample_Remarks_Code_Tidy <-WQ_Data  %>%
filter(SAMPLE_TYPE=="SAMP") %>%
group_by(TEST_NAME,REMARK_CODE) %>%
summarise(n=n())

# Correlation ----------------Need to enter physico-chemical parameters-----------------------------

#DF of differences in TP vs change in other anaytes
TP_differences_vs_Analytes <- WQ_Upstream_Downstream_Tidy %>%
select(date,Ecotope,Difference,TEST_NAME)  %>%
pivot_wider(names_from = TEST_NAME,values_from=`Difference`) 

#Calculate correlation all ecotopes grouped
TP_Correlation <- TP_differences_vs_Analytes %>%
select(-date,-Ecotope) %>%  
select(sort(current_vars())) %>% #sorts column alphabetically
cor(method="spearman",use = "pairwise.complete.obs")


# Visualize ---------------------------------------------------------------

#All Analytes Concnetration over time points and smoooth
ggplot(filter(WQ_Data_Tidy,MATRIX=="SW"),aes(date,`VALUE`,color=Ecotope,fill=Ecotope))+geom_point()+geom_smooth(se=FALSE)+
facet_wrap(~TEST_NAME,scales = "free_y")+scale_fill_brewer(palette = "Set2",direction = -1)+scale_color_brewer(palette = "Set2",direction = -1)+theme_bw()

#All Analytes Concentration over time points and smoooth
ggplot(filter(WQ_Data_Tidy,MATRIX=="SW"),aes(Ecotope,`VALUE`,fill=Ecotope))+geom_boxplot(color="black")+
facet_wrap(~TEST_NAME,scales = "free_y")+scale_fill_brewer(palette = "Set2",direction = -1)+scale_color_brewer(palette = "Set2",direction = -1)+theme_bw()

#All Analytes Differences (Up-Down)- points and smooth 
ggplot(WQ_Upstream_Downstream_Tidy,aes(date,`Difference`,color=Ecotope,fill=Ecotope))+geom_point()+geom_smooth(se=FALSE)+
facet_wrap(~TEST_NAME,scales = "free_y")+scale_fill_brewer(palette = "Set2",direction = -1)+scale_color_brewer(palette = "Set2",direction = -1)+theme_bw()

#All analytes Differences (Up-Down)- columns
ggplot(filter(WQ_Upstream_Downstream_Tidy,TEST_NAME=="TPO4"),aes(date,`Difference`,color=Ecotope,fill=Ecotope))+geom_col(position = "dodge")+#geom_line(size=1)+geom_point()+
scale_fill_brewer(palette = "Set2",direction = -1)+scale_color_brewer(palette = "Set2",direction = -1)+theme_bw()

#TPO4 only Differences (Up-Down)- points and smooth
ggplot(filter(WQ_Upstream_Downstream_Tidy,TEST_NAME=="TPO4"),aes(date,`Difference`,color=Ecotope,fill=Ecotope))+geom_smooth(size=1,se=FALSE)+geom_point()+
scale_fill_brewer(palette = "Set2",direction = -1)+scale_color_brewer(palette = "Set2",direction = -1)+theme_bw()

#All analytes Differences (Up-Down)- Box plots
ggplot(WQ_Upstream_Downstream_Tidy,aes(Ecotope,`Difference`,fill=Ecotope))+geom_boxplot()+geom_hline(yintercept=0)+
facet_wrap(~TEST_NAME,scales = "free_y")+scale_fill_brewer(palette = "Set2",direction = -1)+scale_color_brewer(palette = "Set2",direction = -1)+theme_bw()

#All analytes Differences (Up-Down) by magnitude of average (Up+dowwn)/2- Points and smooth
ggplot(WQ_Upstream_Downstream_Tidy,aes((`Upstream Values`+`Downstream Values`)/2,`Difference`,fill=Ecotope,color=Ecotope))+geom_point(shape=21)+geom_smooth(se=FALSE)+geom_hline(yintercept=0)+
facet_wrap(~TEST_NAME,scales = "free")+scale_fill_brewer(palette = "Set2",direction = -1)+scale_color_brewer(palette = "Set2",direction = -1)+theme_bw()




#Correlation plot
corrplot(TP_Correlation , type = "upper",  tl.col = "black", tl.srt = 45)

#Correlation Plot with GGALLY 
ggpairs(select(TP_differences_vs_Analytes,-date,-Ecotope,-PH,-DO,-COND,-TEMP), title="correlogram with ggpairs()") #removed physico-chemical parameters since that data hasn't been entered yet.

#just select parameters with significant correlation with TP 
ggpairs(select(TP_differences_vs_Analytes,COLOR,CL,TDPO4,TPO4,`Chlorophyll B`,`Chlorophyll A`,`Pheophytin A`,Hardness,CA,TOTFE), title="correlogram with ggpairs()")
