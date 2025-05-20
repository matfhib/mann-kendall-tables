
library(Kendall)
setwd('Q:/WRRP/Working Files_Individuals/Mattie Hibbs/Statistics/MK Tables/Data')
df_raw <- read.csv('RER_2025_groundwater_reduced.csv')
mcl_df <- read.csv('Q:/WRRP/Working Files_Individuals/Mattie Hibbs/Statistics/MK Tables/2025 RER MCLs.csv')

get_full_table <- function(data, media_type, station_name, mcl_table)
{
  stn_list <- get_station_list(df_raw, media_type, station_name, mcl_table)
  chemicals_list <- names(stn_list)
  station_and_chems <- get_station_chems(stn_list, chemicals_list)
  resdf <- get_mk_results2(station_and_chems)
  return(resdf)
}
  
  
  
resdf[order(resdf$Anagroup), ]
stn_list <- get_station_list(df_raw, "UF", "GW-078", mcl_df)