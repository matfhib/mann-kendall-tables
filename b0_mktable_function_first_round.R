#this is the only function that is called in the condensed mann kendall table generation function


mktable <- function(df, mcl)
  
{
  #Frequency of Detection
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
  #4/17 leave this in for  now, but per conversation with Dennis Beal, look at how to appropritely deal with non-detects
  
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
    exc_five = length(which(grepl("TRUE", five_year_results >= mcl)))
    
    print(paste('Ten Year MCL Exceedances:', exc_ten, '/', n_row_all))
    print(paste('Five Year MCL Exceedances:', exc_five, '/', n_row_all_5))
    
    
    #Mann Kendall Trends
    #Ten Year MK Results
    if(length(results_ten_years) >= 4)
    {
      ten_year_avg <- mean(results_ten_years)
      ten_year_std <- sd(results_ten_years)
      ten_year_cv <- (ten_year_std/ten_year_avg)
      mk_results_ten_years <- MannKendall(results_ten_years)
      mk_ten_tau <- mk_results_ten_years$tau[1]
      
      mk_ten_pval <- mk_results_ten_years$sl[1]
      mk_ten_s <- mk_results_ten_years$S[1]
    
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
        print(paste('CV:', ten_year_cv))
        print(paste('S:', mk_ten_s))
        if (ten_year_cv <1 ) 
        {
          print('10yr-Trend: Stable')
        }
        else
        {
        print('10yr-Trend: NO TREND')
        }
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



