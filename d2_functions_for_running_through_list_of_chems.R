#This script contains all the functions needed to get mann kendall table 
#results based on one location and media type, with a list of chemicals and
#corresponding mcls. 
#Functions include: 
#1. get_stations_list
#2. get_station_chems
#3. get_mk_results

#1. get_stations_list
get_station_list <- function (data, media_type, station)
{
  df_red <- data[ , c(1:19)]
  df_red$MEDIA <- as.factor(df_red$MEDIA)
  df_red_by_media <- split(df_red, df_red$MEDIA) 
  df_red_f <- as.data.frame(df_red_by_media["Filtered Groundwater"])
  df_red_uf <- as.data.frame(df_red_by_media["Unfiltered Groundwater"])
  names <- colnames(df_red)
  colnames(df_red_f) <- c(names)
  colnames(df_red_uf) <- names
  if(media_type == "F")
  {
    newdf <- df_red_f
  } 
  else if(media_type == "UF")
  {
    newdf <- df_red_uf
  }
  else
  {
    print("no media was selected")
  }
  
  #split by station
  newdf$STATION <- as.factor(newdf$STATION)
  dataframe_split <- split(newdf, newdf$STATION)
  
  
  #choose station and chemical
  station_df <- as.data.frame(dataframe_split[station])
  colnames(station_df) <- c(names)
  station_chem_split <- split(station_df, as.factor(station_df$CHEMICAL))
  return(station_chem_split)
}


#2. get_station_chems
get_station_chems <- function(station_list, chems_list)
{
  chemicals <- c()
  newlist <- list()
  for(i in 1:length(chems_list))
  {
    vec <- c(chems_list[i])
    chemicals <- c(vec, chemicals)
    chemdf <- station_list[chemicals[1]]
    newlist <- c(newlist, chemdf)
  }
  return(newlist)
}

#3. get_mk_results
get_mk_results <- function(station_chem_list, mcls)
{ 
  
  for(i in 1:length(mcl))
  { 
    mktable_build1(station_chem_list[i], mcls[i])     
  }
  
}