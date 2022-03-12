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


WQ_Upstream_Downstream_Tidy <- read_csv("./Data/WQ Data/WQ_Upstream_Downstream_Tidy.csv")
WQ_Data_Tidy <- read_csv("./Data/WQ Data/WQ_Data_Tidy.csv")
WQ_Data <- read_excel("Data/WQ Data/WQ Data.xlsx",sheet = "Sheet1")


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
select(Date,Ecotope,Difference,TEST_NAME)  %>%
pivot_wider(names_from = TEST_NAME,values_from=`Difference`) 

#Calculate correlation all ecotopes grouped
TP_Correlation <- TP_differences_vs_Analytes %>%
select(-Date,-Ecotope) %>%  
select(sort(current_vars())) %>% #sorts column alphabetically
cor(method="spearman",use = "pairwise.complete.obs")


# Visualize ---------------------------------------------------------------

#All Analyses Concentration over time points and smooth
ggplot(filter(WQ_Data_Tidy,MATRIX=="SW"),aes(Date,`VALUE`,color=Ecotope,fill=Ecotope))+geom_point()+geom_smooth(se=FALSE)+
  facet_wrap(~TEST_NAME,scales = "free_y")+scale_fill_brewer(palette = "Set2",direction = -1)+scale_color_brewer(palette = "Set2",direction = -1)+theme_bw()

#All Analytes Concentration over time points and smoooth
ggplot(filter(WQ_Data_Tidy,MATRIX=="SW"),aes(Ecotope,`VALUE`,fill=Ecotope))+geom_boxplot(color="black")+
  facet_wrap(~TEST_NAME,scales = "free_y")+scale_fill_brewer(palette = "Set2",direction = -1)+scale_color_brewer(palette = "Set2",direction = -1)+theme_bw()

#All Analytes Differences (Up-Down)- points and smooth 
ggplot(WQ_Upstream_Downstream_Tidy,aes(Date,`Difference`,color=Ecotope,fill=Ecotope))+geom_point()+geom_smooth(se=FALSE)+
  facet_wrap(~TEST_NAME,scales = "free_y")+scale_fill_brewer(palette = "Set2",direction = -1)+scale_color_brewer(palette = "Set2",direction = -1)+theme_bw()

#All analytes Differences (Up-Down)- columns
ggplot(filter(WQ_Upstream_Downstream_Tidy,TEST_NAME=="TPO4"),aes(Date,`Difference`,color=Ecotope,fill=Ecotope))+geom_col(position = "dodge")+#geom_line(size=1)+geom_point()+
  scale_fill_brewer(palette = "Set2",direction = -1)+scale_color_brewer(palette = "Set2",direction = -1)+theme_bw()

#TPO4 only Differences (Up-Down)- points and smooth
ggplot(filter(WQ_Upstream_Downstream_Tidy,TEST_NAME=="TPO4"),aes(Date,`Difference`,color=Ecotope,fill=Ecotope))+geom_smooth(size=1,se=FALSE)+geom_point()+
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
ggpairs(select(TP_differences_vs_Analytes,-Date,-Ecotope,-PH,-DO,-COND,-TEMP), title="correlogram with ggpairs()") #removed physico-chemical parameters since that data hasn't been entered yet.

#just select parameters with significant correlation with TP 
ggpairs(select(TP_differences_vs_Analytes,COLOR,CL,TDPO4,TPO4,`Chlorophyll B`,`Chlorophyll A`,`Pheophytin A`,Hardness,CA,TOTFE), title="correlogram with ggpairs()")
