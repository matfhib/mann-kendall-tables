


split_by_chem_fun <- function(somelist)
{
  lapply(somelist, function(x) split(x, as.factor(x$CHEMICAL)))
}


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

#2. get_stations_list
library(dplyr)
get_station_list2 <- function (data, media_type, station, mcldf)
{
  
  df_red <- data[ , c(1:19)]
  df_red <- left_join(df_red, mcldf, by = "CHEMICAL")
  df_red$MEDIA <- as.factor(df_red$MEDIA)
  df_red_by_media <- split(df_red, df_red$MEDIA) 
  df_red_f <- as.data.frame(df_red_by_media["Filtered Groundwater"])
  df_red_uf <- as.data.frame(df_red_by_media["Unfiltered Groundwater"])
  names <- colnames(df_red)
  colnames(df_red_f) <- c(new_names)
  colnames(df_red_uf) <- c(new_names)
 
  #check that media type is not NULL
  if(length(media_type) > 0)
  {
  #if only one media type is selected
   if(length(media_type)==1)
   {
     if(media_type == "F")
     {
       newdf <- df_red_f
     } 
     else if(media_type == "UF")
     {
       newdf <- df_red_uf
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
  else
  {
    #split original df by station
    df_red$STATION <- as.factor(df_red$STATION)
    df_red_split_by_stn <- split(df_red, df_red$STATION)
    df_split_stn_media <- lapply(df_red_split_by_stn, function(x) split(x, as.factor(x$MEDIA)))
    df_split_stn_media_chem <- lapply(df_split_stn_media, split_by_chem_fun)
      
      
      return(df_split_stn_media_chem)
    }
  }
  
  
  else
  {
    print("no media was selected")
  }
  
  
}

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
