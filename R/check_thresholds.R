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

thresholds$ParameterThresholdComparison <- gsub(pattern = "^[=]",
                                                replacement = "==",
                                                thresholds$ParameterThresholdComparison)


}






