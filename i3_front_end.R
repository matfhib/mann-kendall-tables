
stations_i_want <- as.factor(df_raw$STATION)
all_stations <- levels(stations_i_want)

stns <- c(all_stations[1:10])
sup <- wip(df_raw, mcl_df, stns)
get_mk_results2(sup[[1]][[2]])
