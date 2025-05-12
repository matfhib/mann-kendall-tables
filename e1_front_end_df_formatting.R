chemicals_list <- c("Aluminum", "Antimony", "Arsenic", "Barium", "Beryllium", "Boron", "Cadmium", "Calcium", "Chromium", "Cobalt", "Copper", "Iron", "Lead")
station_and_chems <- get_station_chems(gw078_list, chemicals_list)


gw078_list <- get_station_list(df_raw, "UF", "GW-078")
mcl <- c("Null", .006, .01, 2, .004, "NULL", .005, "NULL", 0.1, "NULL", 1.3, "NULL", .005)
get_mk_results2(station_and_chems, mcl)