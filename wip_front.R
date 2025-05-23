
gw078_list <- get_station_list(df_raw, "UF", "GW-078", mcl_df)
chemicals_list <- names(gw078_list)
mcl <- c(rep(1, length(chemicals_list)))
station_and_chems <- get_station_chems(gw078_list, chemicals_list)


get_mk_results2(station_and_chems, mcl)

resdf <- get_mk_results2(station_and_chems, mcl)
resdf[order(resdf$Anagroup), ]