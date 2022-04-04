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

# Import Data -------------------------------------------------------------


WQ_Data <- read_excel("Data/WQ Data/WQ Data.xlsx",sheet = "Sheet1")
WQ_Upstream_Downstream_Tidy <- read_csv("./Data/WQ Data/WQ_Upstream_Downstream_Tidy.csv")



# Tidy --------------------------------------------------------------------

Sign_Test_TP <- WQ_Upstream_Downstream_Tidy %>%
filter(TEST_NAME=="TPO4")


# Wilcoxon Ranked Sign Test -----------------------------------------------


SIGN.test(x = Sign_Test_TP$`Upstream Values`,
          y = Sign_Test_TP$`Downstream Values`,
          md = 0,                
          alternative = "two.sided",
          conf.level = 0.95)

