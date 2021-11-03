
setwd("C:/Users/mpowers/Desktop/ECOTOPE/Targets") # set root directory


library(targets)
library(tarchetypes) # load this to render RMD (to be shown later)


tar_option_set(packages = c("dplyr", "tidyr", "ggplot2", "stringr","lubridate","scales","RColorBrewer","viridis","Hmisc","ggpmisc","zoo","readxl"))




######
# load package dependecies 
######



# the above loads these packages at a global levle for all targets. You can also choose to load them seperately 

######
#load the functions to be used in targets
######

source("./R/functions.R") 

######
#Define pipeline
######

list(
  tar_target(path_train,"./Data/WQ Data.xlsx"), # define path of of WQ data data
  tar_target(df_train,load_dataset(path_train),format = "rds"),    #Load the Dataset
  tar_target(df_tidy,data_tidy(df_train)), # tidy WQ data
  tar_target(up_dwn,Upstream_downstream(df_tidy)), # calculate differences between upstream and downstream
  tar_target(df_raw_plot,df_raw_data_plot(df_train)), # PLot raw data
  tar_target(differences_plot,differences_plot(up_dwn)) # PLot differences data
)










