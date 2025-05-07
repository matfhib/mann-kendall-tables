
#This table contains functions to clean the data frame, split it by station, and split it by chemical, then a function to perform the mann kendall tests. Used for reference and de-bugging.



setwd('Q:/WRRP/Working Files_Individuals/Mattie Hibbs/Statistics/MK Tables/Data')
df_raw <- read.csv('RER_2025_groundwater_reduced.csv')

clean_data_frame <- function (data, media_type)
{
  df_red <- data[ , c(1:19)]
  df_red$MEDIA <- as.factor(df_red$MEDIA)
  df_red_by_media <- split(df_red, df_red$MEDIA)                     
  df_red_f <- as.data.frame(df_red_by_media["Filtered Groundwater"])
  df_red_uf <- as.data.frame(df_red_by_media["Unfiltered Groundwater"])
  names <- colnames(df_red)
  colnames(df_red_f) <- names
  colnames(df_red_uf) <- names
  if(media_type == "F")
        {
          return(df_red_f)
  } 
 if(media_type == "UF")
 {
   return(df_red_uf)
 }
 else
 {
   print("no media was selected")
 }
}


# -> input cleaned data frame into next function

split_by_station <- function(dataframe)
{
  dataframe$STATION <- as.factor(dataframe$STATION)
  dataframe_split <- split(dataframe, dataframe$STATION)
  return(dataframe_split)
}

sta_chem <- function(list, station, chemical)
{
  
  station_df <- as.data.frame(list[station])
  names <- c("WATERSHED","MEDIA","AREA",           "STATION",        "D_COLLECTED",    "ANAGROUP",       "CHEMICAL",       "PARAMTR",        "DETECT",        
             "VALIDATION",     "RSLTQUAL",       "RESULTS",        "DETECT_LIMIT",   "UNITS",          "CHEMICAL_MCL",   "UNITS_MCL",      "SMP_TYPE",       "STA_DESC",      
             "SUB_EVENT_NAME")
  colnames(station_df) <- c(names)
  station_chem_split <- split(station_df, as.factor(station_df$CHEMICAL))
  chemical_df <- as.data.frame(station_chem_split[chemical])
  colnames(chemical_df) <- c(names)
  return(chemical_df)
}

unw_003_barium <- sta_chem(list_stations, "UNW-003", "Barium")

mktable <- function(df, mcl)
  
{
  #Freq of Detection
  freq_of_detect_10yrs <- sum(df$DETECT)
  print(paste('10 year Detection:', freq_of_detect_10yrs, '/', nrow(df)))
  n_row_all <- nrow(df)
  df$D_COLLECTED <- as.Date(df$D_COLLECTED, format = "%d-%b-%y")
  dates_vec <- c(df$D_COLLECTED)
  five_years_start <- which(grepl("TRUE",  dates_vec > '2019-09-30'))[1]
  five_years_df <- df[c(five_years_start:nrow(df)), ]
  freq_of_detect_5yrs <- sum(five_years_df$DETECT)
  
  print(paste('5 year Detection:', freq_of_detect_5yrs, '/', nrow(five_years_df)))
  
  n_row_all_5 <- nrow(five_years_df)
  #Remove Non-Detects
  nondetects_index <- which(grepl("0", df$DETECT))
  if (length(nondetects_index) > 0)
  {
    df <- df[-c(nondetects_index), ]
  }
  else
  {
    df <- df
  } 
  
  
  nondetects_index <- which(grepl("0", five_years_df$DETECT))
  if (length(nondetects_index) > 0)
  {
    five_years_df <- five_years_df[-c(nondetects_index), ]
  }
  else
  {
    five_years_df <- five_years_df
  }
  
  
  
  if (nrow(df) > 0)
  {
    
    
    
    #get ten year maximum
    
    results_ten_years <- df$RESULTS
    max_ten <- max(results_ten_years)
    print(paste('Max detection 10 year:', max_ten))
    
    #Five year max
    
    if(nrow(five_years_df) < 1)
    {
      five_year_results <- NA
      print('ND')
    }
    
    else
    {
      five_year_results <- five_years_df$RESULTS
      max_five <- max(five_year_results)
      print(paste('Max detection 5 year:', max_five))
    }
    
    
    
    #get 2024 maximum
    
    dates_vec <- c(df$D_COLLECTED)
    start_index <-  which(grepl("TRUE",  dates_vec > '2023-09-30'))[1]
    
    if(is.na(start_index))
    {
      print('ND')
    }
    else
    {
      results_last_year <- df[c(start_index:nrow(df)), "RESULTS"]
      max_recent <- max(results_last_year)
      print(paste('Max detection 2024:', max_recent))
    }
    
    
    exc_ten = length(which(grepl("TRUE", results_ten_years >= mcl)))
    exc_five = length(which(grepl("TRUE", five_year_results >= mcl)))
    
    print(paste('Ten Year MCL Exceedances:', exc_ten, '/', n_row_all))
    print(paste('Five Year MCL Exceedances:', exc_five, '/', n_row_all_5))
    
    #Mann Kendall Trends
    #Ten Year MK Results
    if(length(results_ten_years) >= 4)
    {
      mk_results_ten_years <- MannKendall(results_ten_years)
      mk_ten_tau <- mk_results_ten_years$tau[1]
      mk_ten_pval <- mk_results_ten_years$sl[1]
      print(paste('P-value 10 year:', mk_ten_pval))
      print(paste('Tau Value 10 year:', mk_ten_tau))
      if (mk_ten_pval <= .1 & mk_ten_tau < 0)
      {
        print('10-yr Trend: DOWN')
      }
      if (mk_ten_pval <= .1 & mk_ten_tau > 0)
      {
        print('10yr-Trend: UP')
      }
      if (mk_ten_pval > .1)
      {
        print('10yr-Trend: NONE')
      }
    }
    else
    {
      print('not enough data to perform 10-yr trend analysis')
    }
    
    #Five year mk results
    
    
    if(length(five_year_results)>= 4)
    {
      
      mk_results_five_years <-MannKendall(five_year_results)
      
      
      mk_five_tau <- mk_results_five_years$tau[1]
      mk_five_pval <- mk_results_five_years$sl[1]
      print(paste('Pvalue Five Year:', mk_five_pval))
      print(paste('Tau Value Five Year:', mk_five_tau))
      if (mk_five_pval <= .1 & mk_five_tau < 0)
      {
        print('5-yr Trend: DOWN')
      }
      if (mk_five_pval <= .1 & mk_five_tau > 0)
      {
        print('5-yr-Trend: UP')
      }
      if (mk_five_pval > .1)
      {
        print('5-yr-Trend: NONE')
      }
      
      
    }
    else
    {
      print('not enough data to perform 5-yr trend analysis')
    }
  }
  else
  {
    print('No detections for this chemical at this station.')
  }
}






