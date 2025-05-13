mcl_df <- read.csv('Q:/WRRP/Working Files_Individuals/Mattie Hibbs/Statistics/MK Tables/2025 RER MCLs.csv')

#merged mcl_df with df raw in function #2
#pulled true mcl out
#appears in output df; but still working with default mcl = 1

 
library(NADA)
library(NADA2)

mktable_build1 <- function(chemlist)
  
{
  #Frequency of Detection
  df <- as.data.frame(chemlist)
  colnames(df) <- new_names
  #print(as.character(df[1, 'CHEMICAL']))
  chemical_name <- as.character(df[1, 'CHEMICAL'])
  units <- c(df[1, "UNITS"])
  dl_column <- c(df[ , "DETECT_LIMIT"])
  dl <- max(dl_column)
  anagroup <- c(df[1, "ANAGROUP"])
  mcl <- c(df[1, "MCL"])
  
  freq_of_detect_10yrs <- sum(df$DETECT)
  #print(paste('10 year Detection:', freq_of_detect_10yrs, '/', nrow(df)))
  
  frequency_10yr <- c(paste(freq_of_detect_10yrs, '/', nrow(df)))
  
  n_row_all <- nrow(df)
  df$D_COLLECTED <- as.Date(df$D_COLLECTED, format = "%d-%b-%y")
  dates_vec <- c(df$D_COLLECTED)
  five_years_start <- which(grepl("TRUE",  dates_vec > '2019-09-30'))[1]
  
  if(is.na(five_years_start))
  {
    five_years_df <- data.frame(matrix(ncol = 0, nrow = 0))
    #print('5 year detections:0/0')
    freq_of_detect_5yrs <- c(0) 
    frequency_5yr <- c(paste(0, '/', 0))
  }
  else 
  {
    five_years_df <- df[c(five_years_start:nrow(df)), ]
    freq_of_detect_5yrs <- sum(five_years_df$DETECT)
    #print(paste('5 year Detection:', freq_of_detect_5yrs, '/', nrow(five_years_df)))
    n_row_all_5 <- nrow(five_years_df)
    df_five_uncensored <- five_years_df
    df_five_uncensored$D_COLLECTED <- as.Date(df_five_uncensored$D_COLLECTED, format = "%d-%b-%y")
    frequency_5yr <- c(paste(freq_of_detect_5yrs, '/', nrow(five_years_df)))
  }
  
  df_uncensored <- df
  df_uncensored$D_COLLECTED <- as.Date(df$D_COLLECTED, format = "%d-%b-%y")
  
  
  
  #identify non-detects 
  full_res <- df$RESULTS
  cen_vec <- df$DETECT == 0
  
  five_res <- five_years_df$RESULTS
  five_cen <- five_years_df$DETECT == 0
  
  #Remove Non-Detects for maxes 
  
  nondetects_index <- which(grepl("0", df$DETECT))
  if (length(nondetects_index) > 0)
  {
    df <- df[-c(nondetects_index), ]
  }
  else
  {
    df <- df
  } 
  
  if(nrow(five_years_df) > 0)
  {
    nondetects_index <- which(grepl("0", five_years_df$DETECT))
    if (length(nondetects_index) > 0)
    {
      five_years_df <- five_years_df[-c(nondetects_index), ]
    }
    else
    {
      five_years_df <- five_years_df
    }
  }
  else
  {
    five_year_df <- five_years_df
  }
  
  #If there is enough data, perform the statistical calculations
  
  if (nrow(df) > 0)
  {
    
    #get ten year maximum
    
    results_ten_years <- df$RESULTS
    max_ten <- max(results_ten_years)
    #print(paste('Max detection 10 year:', max_ten))
    
    #Five year max
    
    if(nrow(five_years_df) < 1)
    {
      five_year_results <- NA
      max_five <- c("ND")
      #print('ND')
    }
    
    else
    {
      five_year_results <- five_years_df$RESULTS
      max_five <- max(five_year_results)
      #print(paste('Max detection 5 year:', max_five))
    }
    
    
    #2024 maximum
    
    dates_vec <- c(df$D_COLLECTED)
    start_index <-  which(grepl("TRUE",  dates_vec > '2023-09-30'))[1]
    
    if(is.na(start_index))
    {
      #print('ND')
      max_recent <- c('ND')
    }
    else
    {
      results_last_year <- df[c(start_index:nrow(df)), "RESULTS"]
      max_recent <- max(results_last_year)
      #print(paste('Max detection 2024:', max_recent))
    }
    
    #Get ten- and five- year exceedences
    if(is.na(mcl))
    {
      exc_ten_for_table = c('--')
      exc_five_for_table = c('--')
    }
    else
    {
      exc_ten = length(which(grepl("TRUE", results_ten_years >= mcl)))
      #print(paste('Ten Year MCL Exceedances:', exc_ten, '/', n_row_all))
      exc_ten_for_table = c(paste(exc_ten, '/', n_row_all))
      if(nrow(five_years_df) < 1)
      {
      five_year_results <- NA
      
      #print('Five year MCL Exceedance: --')
      exc_five_for_table = c('--')
      }
    
      else
      {
      exc_five = length(which(grepl("TRUE", five_year_results >= mcl)))
      #print(paste('Five Year MCL Exceedances:', exc_five, '/', n_row_all_5))
      exc_five_for_table <- c(paste(exc_five, '/', n_row_all_5))
      }
    
    }
    
    
    
    #Mann Kendall Trends
    #Ten Year MK Results
    if(length(results_ten_years) >= 4)
    {
      #New MK method
      res_censored <- ATSmini(full_res, cen_vec, as.numeric(df_uncensored$D_COLLECTED))
      if(res_censored$slope == 0)
      {
        #print('10yr trend: Stable')
        trend_10yr <- c('stable')
      }
      else
      {
        cen_pval <- res_censored$pval
        cen_s <- res_censored$S
        #print(paste('Censored P-Value:', cen_pval))
        #print(paste('Censored S:', cen_s))
        
        if (cen_pval <= .1 & cen_s < 0 )
        {
          #print('10-year Trend: DOWN')
          trend_10yr <- c('down')
        }
        if(cen_pval <= .1 & cen_s > 0)
        {
          #print('10-year Trend: UP')
          trend_10yr <- c('up')
        }
        if(cen_pval > .1 & cen_s >= 0)
        {
          #print('10-year Trend: No Trend')
          trend_10yr <- c('no trend')
        }
        if(cen_pval > .1 & cen_s <= 0)
        {
          sum_stats = cenfit(full_res, cen_vec)
          mean_censored = mean(sum_stats)
          mean_cen = mean_censored[1]
          #print(paste('KM Mean:', mean_cen))
          
          sd_censored = sd(sum_stats)
          sd_cen = sd_censored[1]
          #print(paste('KM SD:', sd_cen))
          
          cv = sd_cen/mean_cen
          #print(paste('CV (KP):', cv))
          if( cv >= 1)
          {
            #print('10-year Trend: No Trend')
            trend_10yr <- c('no trend')
          }
          if( cv < 1)
          {
            #print('10-year Trend: Stable')
            trend_10yr <- c('stable')
          }
          
        }
      }
    }
    else
    {
      #print('not enough data to perform 10-yr trend analysis')
      trend_10yr <- c('--')
    }
    
    #Five year mk results
    
    if(nrow(five_years_df) < 1)
    {
      #print('Not enough data to perform 5 year trend analysis')
    }
    if (freq_of_detect_5yrs >= 4)
    {
      
      #New MK method
      res_censored_five <- ATSmini(five_res, five_cen, as.numeric(df_five_uncensored$D_COLLECTED))
      
      cen_five_pval <- res_censored_five$pval
      cen_five_s <- res_censored_five$S
      #print(paste('Censored P-Value 5yr:', cen_five_pval))
      #print(paste('Censored S 5yr:', cen_five_s))
      
      if (cen_five_pval <= .1 & cen_five_s < 0 )
      {
        #print('5-year Trend: DOWN')
        trend_5yr <- c('down')
      }
      if(cen_five_pval  <= .1 & cen_five_s > 0)
      {
        #print('5-year Trend: UP')
        trend_5yr <- c('up')
      }
      if(cen_five_pval  > .1 & cen_five_s > 0)
      {
        #print('5-year Trend: No Trend')
        trend_5yr <- c('no trend')
      }
      if(cen_five_pval  > .1 & cen_five_s <= 0)
      {
        sum_stats = cenfit(five_res, five_cen)
        mean_censored = mean(sum_stats)
        mean_cen = mean_censored[1]
        #print(paste('KM Mean:', mean_cen))
        
        sd_censored = sd(sum_stats)
        sd_cen = sd_censored[1]
        #print(paste('KM SD:', sd_cen))
        
        cv = sd_cen/mean_cen
        #print(paste('CV (KP):', cv))
        if( cv >= 1)
        {
          #print('5-year Trend: No Trend')
          trend_5yr <- c('no trend')
        }
        if( cv < 1)
        {
          #print('5-year Trend: Stable')
          trend_5yr <- c('stable')
        }
        
      }
      
      
      
    }
    else
    {
      #print('not enough data to perform 5-yr trend analysis')
      trend_5yr <- c('--')
    }
  }
  else 
  {
    max_ten <- c('ND')
    max_five <- c('ND')
    max_recent <- c('ND')
    exc_ten_for_table <- c('--')
    exc_five_for_table <- c('--')
    trend_10yr <- c('--')
    trend_5yr <- c('--')
  }
  if (is.na(mcl))
  {
    newmcl <- c('--')
  }
  else
  {
    newmcl <- c(mcl)
  }
  all_results_vector <- c(anagroup, chemical_name, units, frequency_10yr, frequency_5yr, dl, max_ten, max_five, max_recent, newmcl, exc_ten_for_table, exc_five_for_table, trend_10yr, trend_5yr)
  return(all_results_vector)
}


#2. get_stations_list
library(dplyr)
get_station_list <- function (data, media_type, station, mcldf)
{
  df_red <- data[ , c(1:19)]
  df_red <- left_join(df_red, mcldf, by = "CHEMICAL")
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


#3. get_station_chems
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


get_mk_results2 <- function(station_chem_list)
{ 
  res <- c()
  column_names <- c("Anagroup", "Chemical", "Units", "10-yr Detection Frequency", "5-yr Detection Frequency", "Max DL", "10-yr Max", "5-yr Max", "2024 Max", "MCL", "10-yr Exceedances", "5-yr Exceedences", "10-yr Trend", "5-yr Trend")
  try <- as.data.frame(t(rep(0, length(column_names))))
  for(i in 1:length(station_chem_list))
  { 
    res <- as.data.frame(t(mktable_build1(station_chem_list[i]) ))
    try <- rbind(try, res)
  }
  colnames(try) <- c(column_names)
  print(try)
}
