mcl_df <- read.csv('Q:/WRRP/Working Files_Individuals/Mattie Hibbs/Statistics/MK Tables/2025 RER MCLs.csv')
df_raw <- read.csv('RER_2025_groundwater_reduced.csv')

stations_i_want <- as.factor(df_raw$STATION)
all_stations <- levels(stations_i_want)

stns <- c(all_stations[1:10])
sup <- wip(df_raw, mcl_df, stns)
list_of_results <- lapply(sup, applying_function)