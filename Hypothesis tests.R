#The purpose of this script is to use hypothesis test to determine if there are differences between ECOTOPES 


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
library(readxl)
library(readr)
library(BSDA)
library(ggridges)

# Import Data -------------------------------------------------------------


WQ_Data <- read_excel("Data/WQ Data/WQ Data.xlsx",sheet = "Sheet1")
WQ_Upstream_Downstream_Tidy <- read_csv("./Data/WQ Data/WQ_Upstream_Downstream_Tidy.csv")
WQ_Data_Tidy <- read_csv("./Data/WQ Data/WQ_Data_Tidy.csv")
WQ_Field_Data <- read_csv("./Data/Joined Data/WQ_Field_Data.csv")
TP_Budget <- read_csv("./Data/P Budget/TP_Budget.csv")


# ---Test upstream vs downstream values for each ecotope------------------------------------
#Wilcoxon Ranked Sign Test 
Sign_Test_TP <- WQ_Upstream_Downstream_Tidy %>%  
filter(TEST_NAME=="TPO4")        #filter to only TPO4

#run Wilcoxon ranked sign test fro each ecotope
HPT_Naiad <-SIGN.test(x = filter(Sign_Test_TP,Ecotope=="Naiad")$`Upstream Values`, y = filter(Sign_Test_TP,Ecotope=="Naiad")$`Downstream Values`,md = 0,alternative = "two.sided",conf.level = 0.95)
HPT_Bare <-SIGN.test(x = filter(Sign_Test_TP,Ecotope=="Bare")$`Upstream Values`, y = filter(Sign_Test_TP,Ecotope=="Bare")$`Downstream Values`,md = 0,alternative = "two.sided",conf.level = 0.95)
HPT_Mixed <-SIGN.test(x = filter(Sign_Test_TP,Ecotope=="Mixed")$`Upstream Values`, y = filter(Sign_Test_TP,Ecotope=="Mixed")$`Downstream Values`,md = 0,alternative = "two.sided",conf.level = 0.95)
HPT_Typha <-SIGN.test(x = filter(Sign_Test_TP,Ecotope=="Typha")$`Upstream Values`, y = filter(Sign_Test_TP,Ecotope=="Typha")$`Downstream Values`,md = 0,alternative = "two.sided",conf.level = 0.95)
HPT_Chara <-SIGN.test(x = filter(Sign_Test_TP,Ecotope=="Chara")$`Upstream Values`, y = filter(Sign_Test_TP,Ecotope=="Chara")$`Downstream Values`,md = 0,alternative = "two.sided",conf.level = 0.95)

#extract hypothesis test parameters from each test run
P.values <- c(HPT_Naiad$p.value,HPT_Bare$p.value,HPT_Mixed$p.value,HPT_Typha$p.value,HPT_Chara$p.value)
Ecotope <- c("Naiad","Bare","Mixed","Typha","Chara")
CI_low <- c(HPT_Naiad$conf.int[1],HPT_Bare$conf.int[1],HPT_Mixed$conf.int[1],HPT_Typha$conf.int[1],HPT_Chara$conf.int[1])
CI_high <- c(HPT_Naiad$conf.int[2],HPT_Bare$conf.int[2],HPT_Mixed$conf.int[2],HPT_Typha$conf.int[2],HPT_Chara$conf.int[2])

HPT_table <- data.frame(Ecotope,P.values,CI_low,CI_high)  #combine extracted parameters in a table

write.csv(HPT_table,"./Data/Publish Tables/Upstream vs Downstream by Ecotope.csv",row.names = FALSE)

# Anova Test TP --test for differences between each station from all Ecotopes ----------------------https://www.scribbr.com/statistics/anova-in-r/

AOV_TP <- WQ_Data %>%
filter(SAMPLE_TYPE=="SAMP")  %>%
filter(TEST_NAME=="TPO4")       #filter to only TPO4 
  
one.way <- aov(VALUE ~ STATION, data = AOV_TP)

summary(one.way)

TukeyHSD(one.way)


# Test for lowest mean TP annual-----------------------------------
Lowest_TP_by_Ecotope_Table <- WQ_Field_Data  %>%  
filter(Position=="Downstream") %>%
group_by(Ecotope) %>%
summarise(Observations=sum(!is.na(TPO4)),`Mean TP`=mean(TPO4,na.rm=TRUE),`Median TP`=median(TPO4,na.rm=TRUE),`Std Dev TP`=sd(TPO4,na.rm=TRUE) )

write.csv(Lowest_TP_by_Ecotope_Table,"./Data/Publish Tables/Lowest_TP_by_Ecotope_Table.csv",row.names = FALSE)

#ANOVA to test for all differences between ecotopes at downstream station
AOV_Ecotope_TP <- WQ_Field_Data %>%
filter(Position=="Downstream") 

one.way.ecotope <- aov(TPO4 ~ Ecotope, data = AOV_Ecotope_TP)

summary(one.way.ecotope)

write.csv(Lowest_TP_by_Ecotope_Table,"./Data/Publish Tables/Lowest_TP_by_Ecotope_Table.csv",row.names = FALSE)

#Wilcoxon Ranked Sign Test 
Wilcoxon_Ecotope_TP <- WQ_Field_Data %>%
filter(Position=="Downstream")

pairwise.wilcox.test(Wilcoxon_Ecotope_TP$TPO4, Wilcoxon_Ecotope_TP$Ecotope, p.adjust.method="none")

#Table of average weekly differences

Difference_by_Ecotope_Table <- WQ_Field_Data  %>%  
filter(Position=="Downstream") %>%
select(Date, Ecotope, TPO4) %>%
group_by(Date) %>%
mutate(`Weekly mean TP`=mean(TPO4,na.rm=TRUE)) %>%
pivot_wider(names_from="Ecotope",values_from="TPO4",values_fn=~mean(.x, na.rm = TRUE)) %>%
mutate(`Bare-Chara`=Bare-Chara,`Bare-Mixed`=Bare-Mixed,`Bare-Typha`=Bare-Typha,`Bare-Naiad`=Bare-Naiad,`Chara-Mixed`=Chara-Mixed,`Chara-Naiad`=Chara-Naiad,
`Chara- Typha`=Chara-Typha,`Mixed-Naiad`=Mixed-Naiad,`Mixed-Typha`=Mixed-Typha,`Typha-Naiad`=Typha-Naiad)  %>%
pivot_longer(8:17,names_to="Comparison",values_to="Value")  
  
ggplot(Difference_by_Ecotope_Table ,aes(Comparison,Value*1000,fill=Comparison))+
geom_boxplot()+  guides(x = guide_axis(angle = 40))+labs(y=expression(TP~(mu~g~L^-1)))+geom_abline(intercept = 0, slope = 0,linetype="dashed")+
scale_fill_brewer(palette = "Set3",direction = -1)+scale_y_continuous(breaks=seq(-20,30,2))+guides(fill="none")+labs(x="")+
theme(panel.background = element_rect(fill = "grey90"),plot.background=element_rect(fill="grey90"))

ggsave(plot = last_plot(),filename="./Figures/TPO4 differences Boxplot.jpeg",width =8, height =6, units = "in")


# Hypothesis tests of FWM  --------------------------------------------

FWM_Test <- TP_Budget %>%
filter(`Outflow (cfs)`>0) %>%
filter(!is.na(Chara) | !is.na(Bare) | !is.na(Typha) | !is.na(Mixed) | !is.na(Naiad)) %>%
mutate(Date=as.Date(`Date Time`))  %>%
group_by(Date) %>%
summarise(n(),`FWM Chara`=mean())
  

