## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
options(tibble.print_min = 6L, tibble.print_max = 6L, digits = 3)

## ---- results = "hide", warning = FALSE, message = FALSE----------------------
library(tdplyr)

## ----odbc, results = 'hide', eval=FALSE---------------------------------------
#  con <- td_create_context("TeradataDSN")

## ---- echo=FALSE, include=FALSE-----------------------------------------------
# Reading Vantage cluster details from td_config.cfg
config_file <- system.file("extdata", "td_config.cfg", package = "tdplyr")
config_df <- read.csv(config_file, sep = ":", stringsAsFactors = FALSE, header = TRUE)
config_list <- unlist(lapply(config_df$value, function(x) {trimws(x)}))
host_name <- config_list[1]
user_name <- config_list[2]
password  <- config_list[3]

## ----native, results = 'hide'-------------------------------------------------
# host_name = Fully qualified domain name or IP address of the Teradata Vantage System.
# user_name = UserId for the connection.
# password  = Password for the connection.
con <- td_create_context(host = host_name, uid = user_name, pwd = password, dType = "native")

## -----------------------------------------------------------------------------
class(con)[1] # The connection object is an object of class: "Teradata"

## -----------------------------------------------------------------------------
names(td_get_context())

## -----------------------------------------------------------------------------
con <- td_get_context()$connection

## -----------------------------------------------------------------------------
td_remove_context()

## ---- eval=FALSE--------------------------------------------------------------
#  td_create_context("TeradataDSN", temp.database = "DATABASE_1")

## -----------------------------------------------------------------------------
con <- td_create_context(host = host_name, uid = user_name, pwd = password, dType = "native", temp.database = "DATABASE_1")
td_get_context()$temp.database

## -----------------------------------------------------------------------------
td_remove_context()

## ---- eval=FALSE--------------------------------------------------------------
#  con <- DBI::dbConnect(odbc::odbc(), dsn = "TeradataDSN")
#  td_set_context(con)

## ---- eval=TRUE---------------------------------------------------------------
con <- DBI::dbConnect(tdplyr::NativeDriver(), host = host_name, uid = user_name, pwd = password, dType = "native")
td_set_context(con)

## -----------------------------------------------------------------------------
td_set_context(con, "DATABASE_1")
td_get_context()$temp.database

## ---- echo=FALSE--------------------------------------------------------------
td_remove_context()

## ---- eval=FALSE--------------------------------------------------------------
#  td_create_context("TeradataDSN", temp.database = "DATABASE_1")
#  
#  # Change the temporary database in the current context
#  td_set_context(td_get_context()$connection, "DATABASE_2")

## -----------------------------------------------------------------------------
con <- td_create_context(host = host_name, uid = user_name, pwd = password, dType = "native", temp.database = "DATABASE_1")
td_get_context()$temp.database

# Change the temporary database in the current context
td_set_context(td_get_context()$connection, "DATABASE_2")
td_get_context()$temp.database

## ---- results = "hide"--------------------------------------------------------
odbc::odbcListDataSources()

## ---- results = "hide"--------------------------------------------------------
con <- td_get_context()$connection
DBI::dbGetInfo(con)

## ---- results = "hide"--------------------------------------------------------
dplyr::db_desc(con)

## -----------------------------------------------------------------------------
options(td.db_desc.output = list(db = TRUE, driver = TRUE, machine = FALSE))
dplyr::db_desc(con)

## -----------------------------------------------------------------------------
# create a volatile table using the copy_to function
dplyr::copy_to(con, data.frame(col1 = c(1:3)), "volatile_table_1", temporary = TRUE)
# View all volatile tables active in the given connection
volatile_tables <- DBI::dbGetQuery(con, "help volatile table")
volatile_tables$"Table SQL Name"

## ---- results = "hide"--------------------------------------------------------
DBI::dbListFields(con, volatile_tables$"Table SQL Name")

## -----------------------------------------------------------------------------
df <- DBI::dbGetQuery(con, "select database;")
df

## -----------------------------------------------------------------------------
td_remove_context()

## ---- results = 'hide', eval=FALSE--------------------------------------------
#  con1 <- td_create_context(dsn = "TeradataDSN")
#  tables <- DBI::dbListTables(con1, schema = td_get_context()$default.database, table_name = "%")
#  tables

