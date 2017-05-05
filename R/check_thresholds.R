#'Check thresholds
#' @param df a dataframe as retrieved by import_data_haridwar()
#' @param thresholds thresholds dataframe as retrieved by get_thresholds()
#' (default: "raw")
#' @return dataframe with thresholds check results for selected time period (i.e.
#' whether Parameters are below/above min/max thresholds defined in dataframe
#' 'thresholds')
#' @export


check_thresholds <- function(df, #haridwar_day_list,
                             thresholds = aquanes.report::get_thresholds()) {

thresholds$ParameterThresholdComparisonR <- gsub(pattern = "^[=]",
                                                replacement = "==",
                                                thresholds$ParameterThresholdComparison)



thresholds$numberOfExceedance <- 0
thresholds$exceedanceLabel <- "Satisfies threshold"


for (idx in seq_len(nrow(thresholds))) {
  
  cond1 <- df$ParameterCode == thresholds$ParameterCode[idx] & df$SiteCode == thresholds$SiteCode[idx]
  cond2 <-  eval(parse(text=sprintf("df$ParameterValue %s %s", 
                                    thresholds$ParameterThresholdComparisonR[idx],
                                    thresholds$ParameterThreshold[idx])))
  
  condition <- cond1 & cond2
  
  number_of_exceedances <- nrow(df[condition,])
  
  if(number_of_exceedances > 0) {
    thresholds$numberOfExceedance[idx] <- number_of_exceedances
    thresholds$exceedanceLabel[idx] <- "Not satisfying threshold"
  }
}

return(thresholds)


}






