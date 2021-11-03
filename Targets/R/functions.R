########################
#Load the data Function
########################

load_dataset = function(path) {
  
  #  @path - defines the location of the data file

  
  df <- read_excel(path,  sheet = "Sheet1")
  
  return(df)
  
}

########################
#Preprocess Function
########################


data_tidy = function(df) {
  
  #  @df - refers to data frame to be pre-processed 
  
 df_t<-df  %>%
 filter(STATION!="OCS",SAMPLE_TYPE=="SAMP") %>%  
 mutate(date=as.Date(COLLECT_DATE)) %>%
 mutate(Ecotope=case_when(str_detect(STATION,"STA34C2B_C")~"Chara",
                          str_detect(STATION,"STA34C2B_T")~"Typha",
                          str_detect(STATION,"STA34C2B_N")~"Naiad",
                          str_detect(STATION,"STA34C2B_M")~"Mixed",
                          str_detect(STATION,"STA34C2B_B")~"Bare"))  %>%
 mutate(Position=case_when(str_detect(STATION,"DWN")~"Downstream",
                           str_detect(STATION,"Up")~"Upstream"))
 return(df_t)
}


Upstream_downstream = function(df) {

WQ_Upstream <-df  %>%
  filter(Position=="Upstream") %>%
  mutate(`Upstream Values`=VALUE)  %>%
  select(date,Ecotope,TEST_NAME,`Upstream Values`)

WQ_DownStream <- df  %>%
  filter(Position=="Downstream") %>%
  mutate(`Downstream Values`=VALUE) %>%
  select(date,Ecotope,TEST_NAME,`Downstream Values`) 

WQ_Upstream_Downstream_Tidy <- WQ_Upstream %>%
  left_join(WQ_DownStream,by=c("date","Ecotope","TEST_NAME"))  %>%
  mutate(`Difference`=`Upstream Values`-`Downstream Values`)

return(WQ_Upstream_Downstream_Tidy)

}


# Visualize parameters ----------------------------------------------------

df_raw_data_plot = function(df) {

p <- ggplot(df,aes(COLLECT_DATE,VALUE,color=STATION,fill=STATION))+geom_point()+geom_smooth(se=FALSE)+
  facet_wrap(~TEST_NAME,scales = "free_y")+scale_fill_brewer(palette = "Set2",direction = -1)+scale_color_brewer(palette = "Set2",direction = -1)+theme_bw()
  return(p) 
}

differences_plot = function(df) {
ggplot(df,aes(Ecotope,`Difference`,fill=Ecotope))+geom_boxplot()+geom_hline(yintercept=0)+
facet_wrap(~TEST_NAME,scales = "free_y")+scale_fill_brewer(palette = "Set2",direction = -1)+scale_color_brewer(palette = "Set2",direction = -1)+theme_bw()
}
