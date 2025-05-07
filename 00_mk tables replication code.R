#This file performs the tests in the MK tables on a line by line basis. This will only be used for reference when building up funtions. 

setwd('Q:/WRRP/Working Files_Individuals/Mattie Hibbs/Statistics/MK Tables/Data')
df_raw <- read.csv('RER_2025_groundwater_reduced.csv')

#Take only necessary columns
df_red <- df_raw[ , c(1:19)]

#Split dataframe into filterd and unfiltered
df_red$MEDIA <- as.factor(df_red$MEDIA)
df_red_by_media <- split(df_red, df_red$MEDIA)
df_red_f <- as.data.frame(df_red_by_media["Filtered Groundwater"])
df_red_uf <- as.data.frame(df_red_by_media["Unfiltered Groundwater"])
df_red_by_media <- split(df_red, df_red$MEDIA)
df_red_f <- as.data.frame(df_red_by_media["Filtered Groundwater"])
df_red_uf <- as.data.frame(df_red_by_media["Unfiltered Groundwater"])
colnames(df_red_uf) <- names

#take filtered
#split filtered (_f) table into a list of dataframes by station
df_red_f$STATION <- as.factor(df_red_f$STATION)
df_by_station_f <- split(df_red_f, df_red_f$STATION)

#take unw-003 as sample station                      
unw_003_f <- as.data.frame(df_by_station_f["UNW-003"])
colnames(unw_003_f) <- names

#select list of chemicals of interest and split into list
unw_003_chems_f <- unw_003_f[unw_003_f$CHEMICAL %in% chemicals_of_interest, ]
#double check levels - optional 
#levels(as.factor(as.character(unw_003_chems_f$CHEMICAL)))
unw_003_by_chems_f <- split(unw_003_chems_f, as.factor(unw_003_chems_f$CHEMICAL))
unw_003_barium_f <- as.data.frame(unw_003_by_chems_f["Barium"])
colnames(unw_003_barium_f) <- names

#Freq of Detects



#Max Detects
  #10 Year
results_ten_years <- unw_003_barium_f$RESULTS
max_ten <- max(results_ten_years)

  #5 Year 
results_five_years <- unw_003_barium_f$RESULTS[11:20]
max_five <- max(results_five_years)

#MCL Exceedance
mcl = 2
exc_five = length(which(grepl("TRUE", results_five_years >= mcl)))
exc_ten = length(which(grepl("TRUE", results_ten_years >= mcl)))

#Man Kendall Test

library(Kendall)
MannKendall(results_ten_years)
MannKendall(results_five_years)


mk$tau[1]
mk$sl[1]




chemicals_of_interest <- c('Aluminum',
                           'Antimony',
                           'Arsenic',
                           'Barium',
                           'Beryllium',
                           'Boron',
                           'Cadmium',
                           'Calcium',
                           'Chromium',
                           'Cobalt',
                           'Copper',
                           'Iron',
                           'Lead',
                           'Lithium',
                           'Magnesium',
                           'Manganese',
                           'Nickel',
                           'Potassium',
                           'Selenium',
                           'Silicon',
                           'Silver',
                           'Sodium',
                           'Strontium',
                           'Thallium',
                           'Uranium',
                           'Vanadium',
                           'Zinc'
)


unw_015$D_COLLECTED <- as.Date(unw_015$D_COLLECTED, format = "%d-%b-%y")


                             
                             