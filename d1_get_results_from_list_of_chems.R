
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
  
gw078_list <- get_station_list(df_raw, "UF", "GW-078")

chemicals_list <- c("Arsenic", "Antimony") 

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

station_and_chems <- get_station_chems(gw078_list, chemicals_list)

mcl <- c(.01, .006 )

get_mk_results <- function(station_chem_list, mcls)
{ 
  
  for(i in 1:length(mcl))
  { 
    mktable_build1(station_chem_list[i], mcls[i])     
  }
  
}

