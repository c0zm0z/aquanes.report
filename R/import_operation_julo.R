#'Helper function: store MySQL database credentials in ".my.cnf"
#'
#' @param dbname name of database to be accessed
#' @param group name of group
#' @param dir path to the MySQL configuration file (default: file.path(getwd(),
#' ".my.cnf"))
#' @param host hostname of MySQL database (default: NULL)
#' @param user username of MySQL database (default: NULL)
#' @param password password of MySQL database (default: NULL)
#' @param ... further arguments passed to dplyr::src_mysql()
#' @return sets dplyr::src_mysql to work with MySQL config file
#' @import dplyr
#' @importFrom plyr rbind.fill
#' @keywords internal
src_mysql_from_cnf <- function(dbname,
                               group = NULL,
                               dir=file.path(getwd(),".my.cnf"),
                               host=NULL,
                      user=NULL,
                      password=NULL,
                      ...) {

  dir <- normalizePath(dir)
  if(!(file.exists(dir))) {
    stop(sprintf("No such file '%s'",dir)) }
  dplyr::src_mysql(
    dbname,
    group = group,
    default.file = dir,
    # explicitly passing null unless otherwise specified.
    host = host,
    user = user,
    password = password,
    ...)
}

#'Imports operational data
#'
#' @param mysql_conf path to the MySQL configuration file
#' @return returns data frame operational data from MySQL db
#' @import dplyr
#' @export
import_operation <- function(mysql_conf = file.path(getwd(),".my.cnf")) {
sumewa <- src_mysql_from_cnf(dbname = "sumewa",
                    group = "autarcon",
                    dir = mysql_conf)


operation <- dplyr::tbl(sumewa, "live") %>%
  dplyr::filter_(~AnlagenID == 4015) %>%
  dplyr::rename_("Redox_Out1" = "Red1",
                 "Redox_Out2" = "Red2",
                 "DateTime" = "time") %>%
  dplyr::mutate_("DateTime" = "as.POSIXct(DateTime,tz = 'UTC')") %>%
  left_join(data.frame(AnlagenID = 4015,
                       LocationName = "Julo")) %>%
  dplyr::tbl_df()            

return(operation)
}



if (FALSE) {

operation <- import_operation()

### Calculate additional parameters:
operation <- operation  %>%
    dplyr::mutate_(Redox_Out = "(Redox_Out1+Redox_Out2)/2",
                   Power_pump = "Up*Ip",
                   Power_cell = "Uz*Iz",
                   Pump_WhPerCbm = "Power_pump/Flux/1000",
                   Cell_WhPerCbm = "Power_cell/Flux/1000")



### Aggregation of online data to user defined
drop.cols <- "LocationName"
operation_grouped <- kwb.base::hsGroupByInterval(data = operation %>%
                                                        dplyr::select_(.dots = setdiff(names(.),drop.cols)),
                                                  interval = 60*15,
                                                  tsField = "DateTime",
                                                   FUN = "median")

### Plot it
pdf(file = "report/datenanalyse_julo.pdf", width = 7, height = 5)

operation_grouped_tidy1 <- tidyr::gather(operation_grouped[,c("DateTime", "Redox_Out1", "Redox_Out2")],
                                       key = "Parameter",
                                       value = "Value", -DateTime)

p1 <- ggplot(data = operation_grouped_tidy1, aes(x = DateTime, y = Value, col = Parameter)) +
  geom_point() +
  labs(list(x = "Datetime (UTC)", y = "Redox potential (mV)")) +
  theme_bw() +
  theme(legend.position = "top")
print(p1)


operation_grouped_tidy2 <- tidyr::gather(operation_grouped[,c("DateTime", "Redox_Out")],
                                       key = "Parameter",
                                       value = "Value", -DateTime)

p2 <- ggplot(data = operation_grouped_tidy2, aes(x = DateTime, y = Value, col = Parameter)) +
  geom_point() +
  labs(list(x = "Datetime (UTC)", y = "Redox potential (mV)")) +
  theme_bw() +
  theme(legend.position = "top")
print(p2)

backwash <- operation[operation$Anlauf == 90,"DateTime"]

p4 <- ggplot(data = operation_grouped %>% filter(DiffPressure < 10), aes(x = DateTime, y = DiffPressure)) +
  geom_point() +
  geom_vline(xintercept = as.numeric(backwash), col = "red") +
  labs(list(x = "Datetime (UTC)",
            y = "Pressure difference (out - in)")) +
  theme_bw() +
  theme(legend.position = "top")
print(p4)


energy_tidy <- operation %>%  select(DateTime, Pump_WhPerCbm, Cell_WhPerCbm) %>% gather(key = "Key", value = "Value",-DateTime)
energy_tidy <- tidyr::separate(energy_tidy,
                               col = "Key",
                               into = c("System component", "Unit"),
                               sep = "_")

energy_title <- sprintf("Based on 15 minute median values of online data\n(period: %s to %s)",
                        min(energy_tidy$DateTime),
                        max(energy_tidy$DateTime))

p5 <- ggplot(energy_tidy , aes_string(x = "DateTime",
                                      y = "Value",
                                      col = "`System component`")) +
  geom_point() +
  geom_vline(xintercept = as.numeric(backwash), col = "red") +
  #facet_wrap(~ `System component`) +
  labs(list(x = "Datetime (UTC)",
            y = "Specific energy demand (Wh/m3)",
            title = energy_title)) +
  theme_bw() +
  theme(legend.position = "top")
print(p5)

p6 <- ggplot(energy_tidy , aes_string(x = "`System component`",
                                      y = "Value",
                                      col = "`System component`")) +
  geom_jitter(height = 0, width = 0.3, alpha = 0.5) +
  #facet_wrap(~ `System component`) +
  labs(list(x = "System component",
            y = "Specific energy demand (Wh/m3)",
            title = energy_title)) +
  theme_bw() +
  theme(legend.position = "top")
print(p6)

dev.off()
}
