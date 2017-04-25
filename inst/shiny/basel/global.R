use_live_data <- TRUE

if (use_live_data) {

library(aquanes.report)

  library(readxl)
  library(tidyr)

  analytics <- read.csv2("data/Beispiel_LisaDaten.csv")

  import_operation_basel <- function(xlsx_path = "data/Export_Rhein_20170330.xlsx") {

    parameters <- readxl::read_excel(path = xlsx_path, sheet = "Parameter")

    sites <- readxl::read_excel(path = xlsx_path, sheet = "Messpunkte")

    for (sheet_parameter in parameters$ParameterCode) {
      print(paste("Importing sheet: ", sheet_parameter))
      tmp <-  readxl::read_excel(path = xlsx_path,sheet = sheet_parameter,col_names = TRUE)

      tmp_tidy <- tmp %>%
        tidyr::gather_(key_col = "SiteCode",
                       value_col = "ParameterValue",
                       gather_cols = dplyr::setdiff(names(tmp),names(tmp)[1])) %>%
        dplyr::mutate_("ParameterCode" = ~sheet_parameter) %>%
        dplyr::mutate(Source = "online",
                      DataType = "raw")

      if(sheet_parameter == parameters$ParameterCode[1]) {
        res <- tmp_tidy
      } else {
        res <- rbind(res, tmp_tidy)
      }}

    names(res)[1] <- "DateTime"
    return(list(parameters = parameters,
                sites = sites,
                data = res))

  }

  rhein <- import_operation_basel(xlsx_path = "data/Export_Rhein_20170330.xlsx")

  rhein_tbl <- rhein$data %>%
    dplyr::inner_join(x = rhein$parameters %>%
                       dplyr::select_("ParameterCode", "ParameterName", "ParameterUnit")) %>%
    dplyr::inner_join(x = rhein$sites %>%
                       dplyr::select_("SiteCode", "SiteName")) %>%
    dplyr::select_("DateTime", "SiteName", "ParameterName", "ParameterValue", "ParameterUnit", "Source", "DataType")

  rhein_tbl <- rhein_tbl %>%
                 dplyr::mutate("SiteName_ParaName_Unit" = sprintf('%s: %s (%s)',
                                                                   rhein_tbl$SiteName,
                                                                   rhein_tbl$ParameterName,
                                                                   rhein_tbl$ParameterUnit)) %>%
    as.data.frame()

  print("Setting time zone to 'CET'")
  rhein_tbl <- aquanes.report::set_timezone(df = rhein_tbl, tz = "CET")

system.time(siteData_raw_list <- rhein_tbl)

system.time(
siteData_10min_list <- aquanes.report::group_datetime(siteData_raw_list,
                                                      by = 10*60))

system.time(
siteData_hour_list <- aquanes.report::group_datetime(siteData_raw_list,
                                                     by = 60*60))

system.time(
  siteData_day_list <- aquanes.report::group_datetime(siteData_raw_list,
                                                        by = "day"))



saveRDS(siteData_raw_list, file = "data/siteData_raw_list.Rds")
saveRDS(siteData_10min_list, file = "data/siteData_10min_list.Rds")
saveRDS(siteData_hour_list, file = "data/siteData_hour_list.Rds")
saveRDS(siteData_day_list, file = "data/siteData_day_list.Rds")

} else {
  #siteData_raw_list <- readRDS("data/siteData_raw_list.Rds")
  siteData_10min_list <- readRDS("data/siteData_10min_list.Rds")
  #siteData_hour_list <- readRDS("data/siteData_hour_list.Rds")
  #siteData_day_list <- readRDS("data/siteData_day_list.Rds")
}
