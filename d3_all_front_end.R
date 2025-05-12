chemicals_list <- c("Arsenic", "Antimony", "Boron", "Cadmium", "Calcium", "Chromium")
station_and_chems <- get_station_chems(gw078_list, chemicals_list)


gw078_list <- get_station_list(df_raw, "UF", "GW-078")
mcl <- c(.01, .006, 'NULL', .005, 'NULL', .1)
get_mk_results(station_and_chems, mcl)