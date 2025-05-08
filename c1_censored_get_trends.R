library(NADA2)


get_trends <- function (data, media_type, station, chemical, mcl)
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
  chemical_df <- as.data.frame(station_chem_split[chemical])
  colnames(chemical_df) <- c(names)
  
  mktable_censored(chemical_df, mcl)
  
  
}


