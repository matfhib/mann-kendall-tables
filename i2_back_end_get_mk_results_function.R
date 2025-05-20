
get_mk_results2 <- function(station_chem_list)
{
  if (length(station_chem_list) > 0)
  {
  res <- c()
  column_names <- c("Anagroup", "Chemical", "Units", "10-yr Detection Frequency", "5-yr Detection Frequency", "Max DL", "10-yr Max", "5-yr Max", "2024 Max", "MCL", "10-yr Exceedances", "5-yr Exceedences", "10-yr Trend", "5-yr Trend")
  try <- as.data.frame(t(rep(0, length(column_names))))
  for(i in 1:length(station_chem_list))
  { 
    res <- as.data.frame(t(mktable_build1(station_chem_list[i]) ))
    try <- rbind(try, res)
    
  }
  #colnames(try) <- c(column_names)
 # print(try)
  return(try)
  }
  else
  {
    no_data_df <- as.data.frame(t(c(rep("ND",14))))
    return(no_data_df)
  }
}

lapply(sup[[1]], get_mk_results2)

applying_function <- function(some_list)
{
  lapply(some_list, get_mk_results2)
}

list_of_results <- lapply(sup, applying_function)