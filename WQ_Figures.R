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
  
  


# Visualize ---------------------------------------------------------------

ggplot(WQ_Upstream_Downstream_Tidy,aes(date,`Difference`,color=Ecotope,fill=Ecotope))+geom_point()+geom_smooth(se=FALSE)+
facet_wrap(~TEST_NAME,scales = "free_y")+scale_fill_brewer(palette = "Set2",direction = -1)+scale_color_brewer(palette = "Set2",direction = -1)+theme_bw()

ggplot(filter(WQ_Upstream_Downstream_Tidy,TEST_NAME=="TPO4"),aes(date,`Difference`,color=Ecotope,fill=Ecotope))+geom_col(position = "dodge")+#geom_line(size=1)+geom_point()+
scale_fill_brewer(palette = "Set2",direction = -1)+scale_color_brewer(palette = "Set2",direction = -1)+theme_bw()

ggplot(filter(WQ_Upstream_Downstream_Tidy,TEST_NAME=="TPO4"),aes(date,`Difference`,color=Ecotope,fill=Ecotope))+geom_smooth(size=1,se=FALSE)+geom_point()+
scale_fill_brewer(palette = "Set2",direction = -1)+scale_color_brewer(palette = "Set2",direction = -1)+theme_bw()

ggplot(WQ_Upstream_Downstream_Tidy,aes(Ecotope,`Difference`,fill=Ecotope))+geom_boxplot()+geom_hline(yintercept=0)+
facet_wrap(~TEST_NAME,scales = "free_y")+scale_fill_brewer(palette = "Set2",direction = -1)+scale_color_brewer(palette = "Set2",direction = -1)+theme_bw()

ggplot(WQ_Upstream_Downstream_Tidy,aes((`Upstream Values`+`Downstream Values`)/2,`Difference`,fill=Ecotope,color=Ecotope))+geom_point(shape=21)+geom_smooth(se=FALSE)+geom_hline(yintercept=0)+
facet_wrap(~TEST_NAME,scales = "free")+scale_fill_brewer(palette = "Set2",direction = -1)+scale_color_brewer(palette = "Set2",direction = -1)+theme_bw()



