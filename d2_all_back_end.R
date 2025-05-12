#This script contains all the functions needed to get mann kendall table 
#results based on one location and media type, with a list of chemicals and
#corresponding mcls. 
#Functions include: 

#1. mktable_build1
#2. get_stations_list
#3. get_station_chems
#4. get_mk_results


#1. mktable_build1

library(NADA)
library(NADA2)

mktable_build1 <- function(chemlist, mcl)
  
{
  #Frequency of Detection
  df <- as.data.frame(chemlist)
  colnames(df) <- column_names
  print(as.character(df[1, 'CHEMICAL']))
  freq_of_detect_10yrs <- sum(df$DETECT)
  print(paste('10 year Detection:', freq_of_detect_10yrs, '/', nrow(df)))
  
  n_row_all <- nrow(df)
  df$D_COLLECTED <- as.Date(df$D_COLLECTED, format = "%d-%b-%y")
  dates_vec <- c(df$D_COLLECTED)
  five_years_start <- which(grepl("TRUE",  dates_vec > '2019-09-30'))[1]
  
  if(is.na(five_years_start))
  {
    five_years_df <- data.frame(matrix(ncol = 0, nrow = 0))
    print('5 year detections:0/0')
    freq_of_detect_5yrs <- c(0) 
  }
  else 
  {
    five_years_df <- df[c(five_years_start:nrow(df)), ]
    freq_of_detect_5yrs <- sum(five_years_df$DETECT)
    print(paste('5 year Detection:', freq_of_detect_5yrs, '/', nrow(five_years_df)))
    n_row_all_5 <- nrow(five_years_df)
    df_five_uncensored <- five_years_df
    df_five_uncensored$D_COLLECTED <- as.Date(df_five_uncensored$D_COLLECTED, format = "%d-%b-%y")
    
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
    
    
    #2024 maximum
    
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
    
    #Get ten- and five- year exceedences
    exc_ten = length(which(grepl("TRUE", results_ten_years >= mcl)))
    print(paste('Ten Year MCL Exceedances:', exc_ten, '/', n_row_all))
    
    if(nrow(five_years_df) < 1)
    {
      five_year_results <- NA
      
      print('Five year MCL Exceedance: --')
    }
    
    else
    {
      exc_five = length(which(grepl("TRUE", five_year_results >= mcl)))
      print(paste('Five Year MCL Exceedances:', exc_five, '/', n_row_all_5))
    }
    
    
    
    
    
    #Mann Kendall Trends
    #Ten Year MK Results
    if(length(results_ten_years) >= 4)
    {
      #New MK method
      res_censored <- ATSmini(full_res, cen_vec, as.numeric(df_uncensored$D_COLLECTED))
      if(res_censored$slope == 0)
      {
        print('10yr trend: Stable')
      }
      else
      {
        cen_pval <- res_censored$pval
        cen_s <- res_censored$S
        print(paste('Censored P-Value:', cen_pval))
        print(paste('Censored S:', cen_s))
        
        if (cen_pval <= .1 & cen_s < 0 )
        {
          print('10-year Trend: DOWN')
        }
        if(cen_pval <= .1 & cen_s > 0)
        {
          print('10-year Trend: UP')
        }
        if(cen_pval > .1 & cen_s >= 0)
        {
          print('10-year Trend: No Trend')
        }
        if(cen_pval > .1 & cen_s <= 0)
        {
          sum_stats = cenfit(full_res, cen_vec)
          mean_censored = mean(sum_stats)
          mean_cen = mean_censored[1]
          print(paste('KM Mean:', mean_cen))
          
          sd_censored = sd(sum_stats)
          sd_cen = sd_censored[1]
          print(paste('KM SD:', sd_cen))
          
          cv = sd_cen/mean_cen
          print(paste('CV (KP):', cv))
          if( cv >= 1)
          {
            print('10-year Trend: No Trend')
          }
          if( cv < 1)
          {
            print('10-year Trend: Stable')
          }
          
        }
      }
    }
    else
    {
      print('not enough data to perform 10-yr trend analysis')
    }
    
    #Five year mk results
    
    if(nrow(five_years_df) < 1)
    {
      print('Not enough data to perform 5 year trend analysis')
    }
    if (freq_of_detect_5yrs >= 4)
    {
      
      #New MK method
      res_censored_five <- ATSmini(five_res, five_cen, as.numeric(df_five_uncensored$D_COLLECTED))
      
      cen_five_pval <- res_censored_five$pval
      cen_five_s <- res_censored_five$S
      print(paste('Censored P-Value 5yr:', cen_five_pval))
      print(paste('Censored S 5yr:', cen_five_s))
      
      if (cen_five_pval <= .1 & cen_five_s < 0 )
      {
        print('5-year Trend: DOWN')
      }
      if(cen_five_pval  <= .1 & cen_five_s > 0)
      {
        print('5-year Trend: UP')
      }
      if(cen_five_pval  > .1 & cen_five_s > 0)
      {
        print('5-year Trend: No Trend')
      }
      if(cen_five_pval  > .1 & cen_five_s <= 0)
      {
        sum_stats = cenfit(five_res, five_cen)
        mean_censored = mean(sum_stats)
        mean_cen = mean_censored[1]
        print(paste('KM Mean:', mean_cen))
        
        sd_censored = sd(sum_stats)
        sd_cen = sd_censored[1]
        print(paste('KM SD:', sd_cen))
        
        cv = sd_cen/mean_cen
        print(paste('CV (KP):', cv))
        if( cv >= 1)
        {
          print('5-year Trend: No Trend')
        }
        if( cv < 1)
        {
          print('5-year Trend: Stable')
        }
        
      }
      
      
      
    }
    else
    {
      print('not enough data to perform 5-yr trend analysis')
    }
  }
}



#2. get_stations_list
get_station_list <- function (data, media_type, station)
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

#4. get_mk_results
get_mk_results <- function(station_chem_list, mcls)
{ 
  
  for(i in 1:length(mcl))
  { 
    mktable_build1(station_chem_list[i], mcls[i])     
  }
  
}

