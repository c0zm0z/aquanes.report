use_live_data <- TRUE

if (use_live_data) {

library(aquanes.report)

analytics <- file.path(getwd(),
                       "data/analytics.xlsx")

mySQL <- file.path(getwd(),
                   ".my.cnf")

op_meta <- file.path(getwd(),
                     "data/operation_parameters.csv")

system.time(
haridwar_raw_list <- aquanes.report::import_data_julo(analytics_path = analytics,
                                          operation_mySQL_conf = mySQL,
                                          operation_meta_path = op_meta))

print("### Step 4: Performing temporal aggregation ##########################")
system.time(
haridwar_10min_list <- aquanes.report::group_datetime(haridwar_raw_list,
                                                      by = 10*60))

system.time(
haridwar_hour_list <- aquanes.report::group_datetime(haridwar_raw_list,
                                                     by = 60*60))

system.time(
  haridwar_day_list <- aquanes.report::group_datetime(haridwar_raw_list,
                                                        by = "day"))



saveRDS(haridwar_raw_list, file = "data/haridwar_raw_list.Rds")
saveRDS(haridwar_10min_list, file = "data/haridwar_10min_list.Rds")
saveRDS(haridwar_hour_list, file = "data/haridwar_hour_list.Rds")
saveRDS(haridwar_day_list, file = "data/haridwar_day_list.Rds")

} else {
  #haridwar_raw_list <- readRDS("data/haridwar_raw_list.Rds")
  haridwar_10min_list <- readRDS("data/haridwar_10min_list.Rds")
  #haridwar_hour_list <- readRDS("data/haridwar_hour_list.Rds")
  #haridwar_day_list <- readRDS("data/haridwar_day_list.Rds")
}

print("### Step 5: Importing threshold information ##########################")
thresholds <- aquanes.report::get_thresholds()

print("### Step 6: Specify available months for reporting ##########################")
report_months <- aquanes.report::create_monthly_selection()

print("### Step 7: Add default calculated operational parameters ##########################")
report_calc_paras <- unique(aquanes.report::calculate_operational_parameters(df = haridwar_10min_list)$ParameterName)