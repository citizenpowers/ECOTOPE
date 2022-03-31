# Objective of this script is to model TP   

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
library(tidymodels)
library(glmnet)
library(ggrepel)
library(ggpmisc)



# Import Data ------------------------------------------------------------


WQ_Field_with_continuous_same_rows <- read.csv("./Data/Joined Data/WQ_Field_with_continuous_same_rows.csv",check.names=FALSE)



# Tidy Data ---------------------------------------------------------------

Model_Data_Tidy <- WQ_Field_with_continuous_same_rows %>% 
filter(TPO4<30) #remove right handed outliers



# Split data  -------------------------------------------------------------

set.seed(4595)

data_split <- initial_split(Model_Data_Tidy, strata = "TPO4", prop = 0.75)

Data_train <- training(data_split)
Data_test <- testing(data_split)


# Create Models ------------------------------------------------------------

#Temp Model
lm_TEMP_fit <-lm(`TPO4` ~ `Temp`,data = Data_train) #model using temp only

#Temp Model
lm_DCS_fit <-lm(`TPO4` ~ `Average DCS (Field Data)`,data = Data_train) #model using temp only

#Outflow Model
lm_Outflow_fit <-lm(`TPO4` ~ `Mean outflow (cfs)`,data = Data_train) #model using temp only

#Temp and DCS Model
lm_TEMP_DCS_fit <-lm(`TPO4` ~ `Temp`+`Average DCS (Field Data)`,data = Data_train) #model using temp only

#Temp, Outflow and DCS Model
lm_TEMP_Outflow_DCS_fit <-lm(`TPO4` ~ `Temp`+`Average DCS (Field Data)`+`Mean outflow (cfs)`,data = Data_train) #model using temp only

# Model Predictions --------------------------------------------------------------

#Predict Temp Model
lm_Temp_pred <- Data_test %>%
bind_cols(predict(lm_TEMP_fit ,newdata = Data_test )) %>%
rename(`Predicted TP`="...82")

#Predict DCS Model
lm_DCS_pred <- Data_test %>%
bind_cols(predict(lm_DCS_fit ,newdata = Data_test )) %>%
rename(`Predicted TP`="...82")

#Predict Temp Model
lm_Outflow_pred <- Data_test %>%
bind_cols(predict(lm_Outflow_fit ,newdata = Data_test )) %>%
rename(`Predicted TP`="...82")

#Predict Temp and DCS Model
lm_Temp_DCS_pred <- Data_test %>%
bind_cols(predict(lm_TEMP_DCS_fit ,newdata = Data_test )) %>%
rename(`Predicted TP`="...82")

#Predict Temp, Outflow and DCS Model
lm_Temp_Outflow_DCS_pred <- Data_test %>%
bind_cols(predict(lm_TEMP_Outflow_DCS_fit ,newdata = Data_test )) %>%
rename(`Predicted TP`="...82")

colnames(Data_test)

# Visualize Model Predictions ---------------------------------------------

#Temp Only 
ggplot(lm_Temp_pred ,aes(x =`TPO4`*1000, y = `Predicted TP`*1000,label=Ecotope,fill=Ecotope))+geom_point(shape=21, size=3)+geom_smooth(method="lm",se=FALSE)+
stat_poly_eq(formula = y~x, aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~"),color=Ecotope,hjust =-5,vjust=12),parse = TRUE)+
theme_bw()+scale_y_continuous(breaks = pretty_breaks(n=5))+scale_x_continuous(breaks = pretty_breaks(n=5))+
labs(y= expression(Predicted~mu~L^-1),x=expression(Measured~mu~L^-1), title = "Using Outflow to predict TP in L-8 FEB\nSamples from all Depths used in Model ")

#DCS Only 
ggplot(lm_DCS_pred ,aes(x =`TPO4`*1000, y = `Predicted TP`*1000,label=Ecotope,fill=Ecotope))+geom_point(shape=21, size=3)+geom_smooth(method="lm",se=FALSE)+
stat_poly_eq(formula = y~x, aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~"),color=Ecotope,hjust =-5,vjust=10),parse = TRUE)+
theme_bw()+scale_y_continuous(breaks = pretty_breaks(n=5))+scale_x_continuous(breaks = pretty_breaks(n=5))+
labs(y= expression(Predicted~mu~L^-1),x=expression(Measured~mu~L^-1), title = "Using DCS to predict TP in L-8 FEB\nSamples from all Depths used in Model ")

#Outflow Only 
ggplot(lm_Outflow_pred ,aes(x =`TPO4`*1000, y = `Predicted TP`*1000,label=Ecotope,fill=Ecotope))+geom_point(shape=21, size=3)+geom_smooth(method="lm",se=FALSE)+
stat_poly_eq(formula = y~x, aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~"),color=Ecotope,hjust =-5,vjust=5),parse = TRUE)+
theme_bw()+scale_y_continuous(breaks = pretty_breaks(n=5))+scale_x_continuous(breaks = pretty_breaks(n=5))+
labs(y= expression(Predicted~mu~L^-1),x=expression(Measured~mu~L^-1), title = "Using Outflow to predict TP in L-8 FEB\nSamples from all Depths used in Model ")

#Outflow and DCS
ggplot(lm_Temp_DCS_pred,aes(x =`TPO4`*1000, y = `Predicted TP`*1000,label=Ecotope,fill=Ecotope))+geom_point(shape=21, size=3)+geom_smooth(method="lm",se=FALSE)+
stat_poly_eq(formula = y~x, aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~"),color=Ecotope,hjust =-5,vjust=5),parse = TRUE)+
theme_bw()+scale_y_continuous(breaks = pretty_breaks(n=5))+scale_x_continuous(breaks = pretty_breaks(n=5))+
labs(y= expression(Predicted~mu~L^-1),x=expression(Measured~mu~L^-1), title = "Using Temp and DCS to predict TP in L-8 FEB\nSamples from all Depths used in Model ")

#Outflow and DCS
ggplot(lm_Temp_Outflow_DCS_pred ,aes(x =`TPO4`*1000, y = `Predicted TP`*1000,label=Ecotope,fill=Ecotope))+geom_point(shape=21, size=3)+geom_smooth(method="lm",se=FALSE)+
stat_poly_eq(formula = y~x, aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~"),color=Ecotope,hjust =-5,vjust=10),parse = TRUE)+
theme_bw()+scale_y_continuous(breaks = pretty_breaks(n=5))+scale_x_continuous(breaks = pretty_breaks(n=5))+
labs(y= expression(Predicted~mu~L^-1),x=expression(Measured~mu~L^-1), title = "Using Temp and DCS to predict TP in L-8 FEB\nSamples from all Depths used in Model ")



