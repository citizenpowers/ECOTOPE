#Objectives- Create a GAM model to estimate the effect of independent variables Ecotope type, flow, depth, temp on response variable TP


library(readr)
library(readxl)
library(scales)
library(dplyr)
library(ggpmisc)
library(ggplot2)
library(lubridate)
library(stringr)
library(tidyr)
library(ggpmisc)
library(purrr)
library(broom)
library(mgcv)
library(gratia)
library(zoo)
library(ggeffects)
library(kableExtra)

# Import Data ------------------------------------------------------------


WQ_Field_with_continuous_same_rows <- read.csv("./Data/Joined Data/WQ_Field_with_continuous_same_rows.csv",check.names=FALSE)




# Tidy data for modeling ----------------------Categorical variables need to be changed to factors----------------------------

TP_GAM_DATA <- WQ_Field_with_continuous_same_rows %>%
rename(DCS="DCS (Field Data)",Water_Column="Water Column (Field Data)",Inflow="Mean inflow (cfs)",Outflow="Mean outflow (cfs)",Wind="BELLE GL_WNVS_MPH") %>% #models will not accept variables with blank spaces as input 
select(Ecotope,Position,Date,Hour,DCS,Water_Column,Inflow,Outflow,Wind,Temp,TPO4)  %>%
mutate(Ecotope=as.factor(Ecotope),Month=month(mdy(Date),abbr=TRUE,label=TRUE),Day=yday(mdy(Date))) %>%
drop_na(TPO4)

  
  

# Build GAM models --------------------------------------------------------
#Single Variable Models
GAM_Inflow <- gam(TPO4 ~s(Inflow),method="REML",select=TRUE,data =TP_GAM_DATA)
GAM_Outflow <- gam(TPO4 ~s(Outflow),method="REML",select=TRUE,data =TP_GAM_DATA)
GAM_DCS <- gam(TPO4 ~s(DCS),method="REML",select=TRUE,data =TP_GAM_DATA)
GAM_Water_Column <- gam(TPO4 ~s(Water_Column),method="REML",select=TRUE,data =TP_GAM_DATA)
GAM_Wind <- gam(TPO4 ~s(Wind),method="REML",select=TRUE,data =TP_GAM_DATA)
GAM_Temp <- gam(TPO4 ~s(Temp),method="REML",select=TRUE,data =TP_GAM_DATA)
GAM_Day <- gam(TPO4 ~s(Day),method="REML",select=TRUE,data =TP_GAM_DATA)
GAM_Ecotope <- gam(TPO4 ~Ecotope,method="REML",select=TRUE,data =TP_GAM_DATA)


#Multivariate Models
GAM_Outflow_Ecotope <- gam(TPO4 ~Ecotope+s(Outflow,by=Ecotope),method="REML",select=TRUE,data =TP_GAM_DATA)
GAM_Water_Column_Ecotope_RI <- gam(TPO4 ~Ecotope+s(Water_Column),method="REML",select=TRUE,data =TP_GAM_DATA)  #Random intercepts
GAM_Water_Column_Ecotope_RS <- gam(TPO4 ~Ecotope+s(Water_Column)+s(Water_Column,by=Ecotope,bs="fs", m=1),method="REML",select=TRUE,data =TP_GAM_DATA)  #Random smooth
GAM_Ecotope_Water_column_Outflow_Temp_RS <- gam(TPO4 ~Ecotope+s(Water_Column)+s(Water_Column,by=Ecotope,bs="fs", m=1)+s(Outflow)+s(Temp),method="REML",data =TP_GAM_DATA)  #Random Intercept
GAM_Ecotope_Water_column_Outflow_Temp <- gam(TPO4 ~Ecotope+s(Water_Column)+s(Outflow)+s(Temp),method="REML",data =TP_GAM_DATA)  #Random Intercept
GAM_Ecotope_Water_column_Outflow <- gam(TPO4 ~Ecotope+s(Water_Column)+s(Outflow),method="REML",data =TP_GAM_DATA)  #Random Intercept
GAM_Ecotope_Water_column_Temp <- gam(TPO4 ~Ecotope+s(Water_Column)+s(Temp),method="REML",data =TP_GAM_DATA)  #Random Intercept
GAM_Ecotope_Water_column_Temp_RS <- gam(TPO4 ~Ecotope+s(Water_Column)+s(Water_Column,by=Ecotope,bs="fs", m=1)+s(Temp),method="REML",data =TP_GAM_DATA)  #Random smooth
GAM_Day_Water_column_Outflow_Temp_RI <- gam(TPO4 ~s(Day)+s(Water_Column)+s(Outflow)+s(Temp),method="REML",data =TP_GAM_DATA)  #Random Intercept
GAM_Day_EcotopeWater_column_Outflow_Temp_RI <- gam(TPO4 ~s(Day)+s(Water_Column)+s(Outflow)+s(Temp),method="REML",data =TP_GAM_DATA)  #Random Intercept
GAM_Day_Ecotope_Water_column_Outflow_Temp_RI <- gam(TPO4 ~Ecotope+s(Day)+s(Water_Column)+s(Outflow)+s(Temp),method="REML",data =TP_GAM_DATA)  #Random Intercept
GAM_Day_Ecotope_Water_column_Outflow_Temp_RS <- gam(TPO4 ~Ecotope+s(Day)+s(Day,by=Ecotope,bs="fs", m=1)+s(Water_Column)+s(Outflow)+s(Temp),method="REML",data =TP_GAM_DATA)  #Random smooth


# Model Evaluation --------------------------------------------------------
summary(GAM_Ecotope)  #Ecotope deviance explained 5.29%
plot(GAM_Ecotope, shade = TRUE, pages = 1, scale = 0, seWithMean = TRUE)
summary(GAM_Inflow)  #Inflow deviance explained 5.29%
plot(GAM_Inflow, shade = TRUE, pages = 1, scale = 0, seWithMean = TRUE)
summary(GAM_Outflow)  #outflow deviance explained 45.9%
plot(GAM_Outflow, shade = TRUE, pages = 1, scale = 0, seWithMean = TRUE)
summary(GAM_DCS)  #DCS deviance explained 34.6%
plot(GAM_DCS, shade = TRUE, pages = 1, scale = 0, seWithMean = TRUE)
summary(GAM_Water_Column)  #water column deviance explained 41.7%
plot(GAM_Water_Column, shade = TRUE, pages = 1, scale = 0, seWithMean = TRUE)
summary(GAM_Wind)  #wind deviance explained 12%
plot(GAM_Wind, shade = TRUE, pages = 1, scale = 0, seWithMean = TRUE)
summary(GAM_Temp)  #temp deviance explained 17.9%
plot(GAM_Temp, shade = TRUE, pages = 1, scale = 0, seWithMean = TRUE)
summary(GAM_Day)  #Day deviance explained 66%
plot(GAM_Day, shade = TRUE, pages = 1, scale = 0, seWithMean = TRUE)
summary(GAM_Outflow_Ecotope)  #outflow and ecotope deviance explained 51.4%
plot(GAM_Outflow_Ecotope, shade = TRUE, pages = 1, scale = 0, seWithMean = TRUE)
summary(GAM_Water_Column_Ecotope_RI)  #water column and ecotope deviance explained 44%
plot(GAM_Water_Column_Ecotope_RI, shade = TRUE, pages = 1, scale = 0, seWithMean = TRUE,all.terms=TRUE)

summary(GAM_Ecotope_Water_column_Outflow)  #water column and ecotope deviance explained 54.7%
plot(GAM_Ecotope_Water_column_Outflow, shade = TRUE, pages = 1, scale = 0, seWithMean = TRUE,all.terms=TRUE)

summary(GAM_Ecotope_Water_column_Temp)  #water column and ecotope deviance explained 55.8%
plot(GAM_Ecotope_Water_column_Temp, shade = TRUE, pages = 1, scale = 0, seWithMean = TRUE,all.terms=TRUE)

summary(GAM_Ecotope_Water_column_Temp_RS)  #water column and ecotope deviance explained 56.5%
plot(GAM_Ecotope_Water_column_Temp_RS, shade = TRUE, pages = 1, scale = 0, seWithMean = TRUE,all.terms=TRUE)

summary(GAM_Ecotope_Water_column_Outflow_Temp)  #water column and ecotope deviance explained 64.2%
plot(GAM_Ecotope_Water_column_Outflow_Temp, shade = TRUE, pages = 1, scale = 0, seWithMean = TRUE,all.terms=TRUE)


summary(GAM_Water_Column_Ecotope_RS)  #water column and ecotope deviance explained 46.4%
plot(GAM_Water_Column_Ecotope_RS, shade = TRUE, pages = 1, scale = 0, seWithMean = TRUE,all.terms=TRUE)
summary(GAM_Ecotope_Water_column_Outflow_Temp_RS)  #water column and ecotope deviance explained 64%
plot(GAM_Ecotope_Water_column_Outflow_Temp_RS, shade = TRUE, pages = 1, scale = 0, seWithMean = TRUE,all.terms=TRUE)
summary(GAM_Day_Water_column_Outflow_Temp_RI)  #Day water column outflow and temp explain 85.2% deviance
plot(GAM_Day_Water_column_Outflow_Temp_RI, shade = TRUE, pages = 1, scale = 0, seWithMean = TRUE,all.terms=TRUE)
summary(GAM_Day_Ecotope_Water_column_Outflow_Temp_RI)  #Ecotope Day water column outflow and temp explain 87.4% deviance Random intercept
plot(GAM_Day_Ecotope_Water_column_Outflow_Temp_RI , shade = TRUE, pages = 1, scale = 0, seWithMean = TRUE,all.terms=TRUE)
summary(GAM_Day_Ecotope_Water_column_Outflow_Temp_RS)  #Ecotope Day water column outflow and temp explain 85.2% deviance Randoms smooth
plot(GAM_Day_Ecotope_Water_column_Outflow_Temp_RS , shade = TRUE, pages = 1, scale = 0, seWithMean = TRUE,all.terms=TRUE)

#Concurvity checks
concurvity(GAM_Day_Water_column_Outflow_Temp_RI)
concurvity(GAM_Day_Ecotope_Water_column_Outflow_Temp_RS)
concurvity(GAM_Day_Ecotope_Water_column_Outflow_Temp_RI)
concurvity(GAM_Outflow_Ecotope)
concurvity(GAM_Ecotope_Water_column_Outflow_Temp_RS)
concurvity(GAM_Water_Column_Ecotope_RI)
concurvity(GAM_Water_Column_Ecotope_RS)
concurvity(GAM_Ecotope_Water_column_Outflow)
concurvity(GAM_Ecotope_Water_column_Temp)
concurvity(GAM_Ecotope_Water_column_Outflow_Temp)

#GAM checks
gam.check(GAM_Outflow_Ecotope, k.rep = 1000)
gam.check(GAM_Day_Ecotope_Water_column_Outflow_Temp_RI, k.rep = 1000)
gam.check(GAM_Water_Column_Ecotope_RI, k.rep = 1000)
gam.check(GAM_Water_Column_Ecotope_RS, k.rep = 1000)
gam.check(GAM_Ecotope_Water_column_Outflow_Temp, k.rep = 1000)


# Build GAM models using downstream ecotope data only-------------------------------------------------------
TP_GAM_DATA_DWN <- TP_GAM_DATA %>%
filter(Position=="Downstream")


#Single Variable Models
GAM_Inflow_DWN <- gam(TPO4 ~s(Inflow),method="REML",select=TRUE,data =TP_GAM_DATA_DWN)
GAM_Outflow_DWN <- gam(TPO4 ~s(Outflow),method="REML",select=TRUE,data =TP_GAM_DATA_DWN)
GAM_DCS_DWN <- gam(TPO4 ~s(DCS),method="REML",select=TRUE,data =TP_GAM_DATA_DWN)
GAM_Water_Column_DWN <- gam(TPO4 ~s(Water_Column),method="REML",select=TRUE,data =TP_GAM_DATA_DWN)
GAM_Wind_DWN <- gam(TPO4 ~s(Wind),method="REML",select=TRUE,data =TP_GAM_DATA_DWN)

#Multivariate Models
GAM_Outflow_Water_Column_DWN <- gam(TPO4 ~s(Outflow)+s(Water_Column),method="REML",select=TRUE,data =TP_GAM_DATA_DWN)  #ecotope and outflow
GAM_Outflow_Wind_DWN <- gam(TPO4 ~s(Outflow)+s(Wind),method="REML",select=TRUE,data =TP_GAM_DATA_DWN)  #ecotope and outflow
GAM_Outflow_Ecotope_DWN <- gam(TPO4 ~Ecotope+s(Outflow,by=Ecotope),method="REML",select=TRUE,data =TP_GAM_DATA_DWN)  #ecotope and outflow
GAM_Water_Column_Ecotope_DWN <- gam(TPO4 ~Ecotope+s(Water_Column,by=Ecotope),method="REML",select=TRUE,data =TP_GAM_DATA_DWN) #ecotope and water column random effects
GAM_Day_Water_column_Outflow_Temp_RI_DWN <- gam(TPO4 ~s(Day)+s(Water_Column)+s(Outflow)+s(Temp),method="REML",data =TP_GAM_DATA_DWN)  #Random Intercept
GAM_Day_Ecotope_Water_column_Outflow_Temp_RI_DWN <- gam(TPO4 ~Ecotope+s(Day)+s(Water_Column)+s(Outflow)+s(Temp),method="REML",data =TP_GAM_DATA_DWN)  #Random Intercept
GAM_Day_Ecotope_Water_column_Outflow_Temp_RS_DWN <- gam(TPO4 ~Ecotope+s(Day)+s(Day,by=Ecotope,bs="fs", m=1)+s(Water_Column)+s(Outflow)+s(Temp),method="REML",data =TP_GAM_DATA_DWN)  #Random smooth
GAM_Ecotope_Water_column_Outflow_Temp_DWN <- gam(TPO4 ~Ecotope+s(Water_Column)+s(Outflow)+s(Temp),method="REML",data =TP_GAM_DATA_DWN)  #Random Intercept
GAM_Water_column_Outflow_Temp_DWN <- gam(TPO4 ~s(Water_Column)+s(Outflow)+s(Temp),method="REML",data =TP_GAM_DATA_DWN)  #Random Intercept
GAM_Outflow_Temp_DWN <- gam(TPO4 ~Ecotope+s(Outflow)+s(Temp),method="REML",data =TP_GAM_DATA_DWN)  #Random Intercept


# Model evaluation from downstream ecotope data only ----------------------
summary(GAM_Outflow_Water_Column_DWN)  #Inflow deviance explained 48.8%
plot(GAM_Outflow_Water_Column_DWN, shade = TRUE, pages = 1, scale = 0, seWithMean = TRUE,all.terms=TRUE)
concurvity(GAM_Outflow_Water_Column_DWN)
gam.check(GAM_Outflow_Water_Column_DWN, k.rep = 1000)

summary(GAM_Outflow_Wind_DWN )  #Inflow deviance explained 54.6%
plot(GAM_Outflow_Wind_DWN , shade = TRUE, pages = 1, scale = 0, seWithMean = TRUE,all.terms=TRUE)
concurvity(GAM_Outflow_Wind_DWN )
gam.check(GAM_Outflow_Wind_DWN, k.rep = 1000)

summary(GAM_Outflow_Ecotope_DWN )  #Inflow deviance explained 46.9%
plot(GAM_Outflow_Ecotope_DWN , shade = TRUE, pages = 1, scale = 0, seWithMean = TRUE,all.terms=TRUE)
concurvity(GAM_Outflow_Ecotope_DWN)
gam.check(GAM_Outflow_Ecotope_DWN, k.rep = 1000)

summary(GAM_Water_Column_Ecotope_DWN )  #Inflow deviance explained 39% 
plot(GAM_Water_Column_Ecotope_DWN , shade = TRUE, pages = 1, scale = 0, seWithMean = TRUE,all.terms=TRUE)
concurvity(GAM_Water_Column_Ecotope_DWN)
gam.check(GAM_Water_Column_Ecotope_DWN, k.rep = 1000)

summary(GAM_Water_column_Outflow_Temp_DWN  )  #Inflow deviance explained 64.9%
plot(GAM_Water_column_Outflow_Temp_DWN  , shade = TRUE, pages = 1, scale = 0, seWithMean = TRUE,all.terms=TRUE)
concurvity(GAM_Water_column_Outflow_Temp_DWN )
gam.check(GAM_Water_column_Outflow_Temp_DWN, k.rep = 1000)

summary(GAM_Ecotope_Water_column_Outflow_Temp_DWN )  #Inflow deviance explained 66.8%
plot(GAM_Ecotope_Water_column_Outflow_Temp_DWN, shade = TRUE, pages = 1, scale = 0, seWithMean = TRUE,all.terms=TRUE)
concurvity(GAM_Ecotope_Water_column_Outflow_Temp_DWN )
gam.check(GAM_Ecotope_Water_column_Outflow_Temp_DWN, k.rep = 1000)
draw(GAM_Ecotope_Water_column_Outflow_Temp_DWN,all.terms=TRUE)
smooth_estimates(GAM_Ecotope_Water_column_Outflow_Temp_DWN)

summary(GAM_Outflow_Temp_DWN )  #Inflow deviance explained 62.8%
plot(GAM_Outflow_Temp_DWN, shade = TRUE, pages = 1, scale = 0, seWithMean = TRUE,all.terms=TRUE)
concurvity(GAM_Outflow_Temp_DWN )
gam.check(GAM_Outflow_Temp_DWN, k.rep = 1000)
draw(GAM_Outflow_Temp_DWN,all.terms=TRUE)

summary(GAM_Inflow_DWN)  #Inflow deviance explained 5.0%
plot(GAM_Inflow_DWN, shade = TRUE, pages = 1, scale = 0, seWithMean = TRUE)
summary(GAM_Outflow_DWN)  #Inflow deviance explained 45.9%
plot(GAM_Outflow_DWN, shade = TRUE, pages = 1, scale = 0, seWithMean = TRUE)
summary(GAM_DCS_DWN)  #DCS deviance explained 30.4%
plot(GAM_DCS_DWN, shade = TRUE, pages = 1, scale = 0, seWithMean = TRUE)
summary(GAM_Water_Column_DWN)  #DCS deviance explained 37.8%
plot(GAM_Water_Column_DWN, shade = TRUE, pages = 1, scale = 0, seWithMean = TRUE)
summary(GAM_Wind_DWN)  #DCS deviance explained 8.29%
plot(GAM_Wind_DWN, shade = TRUE, pages = 1, scale = 0, seWithMean = TRUE)
summary(GAM_Outflow_Ecotope_DWN)  #DCS deviance explained 46.9%
plot(GAM_Outflow_Ecotope_DWN, shade = TRUE, pages = 1, scale = 0, seWithMean = TRUE)
summary(GAM_Day_Water_column_Outflow_Temp_RI_DWN)  #DCS deviance explained 85.2%
plot(GAM_Day_Water_column_Outflow_Temp_RI_DWN, shade = TRUE, pages = 1, scale = 0, seWithMean = TRUE,all.terms=TRUE)
summary(GAM_Day_Ecotope_Water_column_Outflow_Temp_RI_DWN )  #DCS deviance explained 87.4%
plot(GAM_Day_Ecotope_Water_column_Outflow_Temp_RI_DWN , shade = TRUE, pages = 1, scale = 0, seWithMean = TRUE,all.terms=TRUE)

summary(GAM_Day_Ecotope_DCS_Outflow_Temp_RS_DWN)  #DCS deviance explained 88.6%
plot(GAM_Day_Ecotope_DCS_Outflow_Temp_RS_DWN, shade = TRUE, pages = 1, scale = 0, seWithMean = TRUE,all.terms=TRUE)
concurvity(GAM_Day_Ecotope_DCS_Outflow_Temp_RS_DWN )
gam.check(GAM_Day_Ecotope_DCS_Outflow_Temp_RS_DWN, k.rep = 1000)



# Create model plots ------------------------------------------------------

GAM_Ecotope_Water_column_Outflow_Temp_DWN_plot <-smooth_estimates(GAM_Ecotope_Water_column_Outflow_Temp_DWN) %>%
add_confint()

Residuals <- TP_GAM_DATA_DWN %>%
add_partial_residuals(GAM_Ecotope_Water_column_Outflow_Temp_DWN)

ilter(smooth == "s(x2)") 
  
ggplot(GAM_Ecotope_Water_column_Outflow_Temp_DWN_plot) +facet_wrap(~smooth,scales="free")+
#geom_rug(aes(x = x2),
#           data = eg1,
#           sides = "b", length = grid::unit(0.02, "npc")) +
  geom_ribbon(aes(ymin = lower_ci, ymax = upper_ci, x =est),
              alpha = 0.2) 
  
  
  geom_point(aes(x = x2, y = `s(x2)`),
             data = eg1, cex = 1.5, colour = "steelblue3") +
  geom_line(aes(x = x2, y = est), lwd = 1.2) +
  labs(y = "Partial effect", title = "s(x2)")
p_sx2



summary(GAM_Ecotope_Water_column_Outflow_Temp_DWN )  #Inflow deviance explained 66.8%
plot(GAM_Ecotope_Water_column_Outflow_Temp_DWN, shade = TRUE, pages = 1, scale = 0, seWithMean = TRUE,all.terms=TRUE)
concurvity(GAM_Ecotope_Water_column_Outflow_Temp_DWN )
gam.check(GAM_Ecotope_Water_column_Outflow_Temp_DWN, k.rep = 1000)
draw(GAM_Ecotope_Water_column_Outflow_Temp_DWN)
smooth_estimates(GAM_Ecotope_Water_column_Outflow_Temp_DWN)


summary(GAM_Ecotope_Water_column_Outflow_Temp_DWN) %>%
  kbl(caption="Table 1: Summary Statistics of Financial Well-Being  
               Score by Gender and Education",
      format= "html",
      align="r") %>%
  kable_classic(full_width = F, html_font = "helvetica")

draw(filter(GAM_Ecotope_Water_column_Outflow_Temp_DWN,smooth=="s(Outflow)"),all.terms=TRUE)
p & theme_bw()  

outflow <- draw(GAM_Ecotope_Water_column_Outflow_Temp_DWN, select = "s(Outflow)",smooth_col="yellow",ci_col="yellow")& Presentation_theme
Water_column<- draw(GAM_Ecotope_Water_column_Outflow_Temp_DWN, select = "s(Water_Column)",smooth_col="orange",ci_col="orange")
Temp <- draw(GAM_Ecotope_Water_column_Outflow_Temp_DWN, select = "s(Temp)",smooth_col="green",ci_col="green")
ecotope <- draw(GAM_Ecotope_Water_column_Outflow_Temp_DWN, term="Ecotope",parametric=TRUE,smooth_col="orange",ci_col="orange")& Presentation_theme
  
Ecotope_GAM_Plot<- draw(GAM_Ecotope_Water_column_Outflow_Temp_DWN, term="Ecotope",parametric=TRUE,smooth_col="orange",ci_col="orange",scales = "fixed")& Presentation_theme2

ggsave(plot = last_plot(),filename="./Figures/Ecotope_GAM_Plot.jpeg",width =13.333, height =7.5, units = "in")
gamOut(summary(GAM_Ecotope_Water_column_Outflow_Temp_DWN),"./Figures/GAM model summary.csv") 
