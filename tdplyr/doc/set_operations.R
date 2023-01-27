## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
options(tibble.print_min = 6L, tibble.print_max = 6L, digits = 3)
library(knitr)

## ----create_connection, echo=FALSE, include=FALSE------------------------
library(dplyr)
library(dbplyr)
library(DBI)
library(tdplyr)

# Reading Vantage cluster details from td_config.cfg
config_file <- system.file("extdata", "td_config.cfg", package = "tdplyr")
config_df <- read.csv(config_file, sep = ":", stringsAsFactors = FALSE, header = TRUE)
config_list <- unlist(lapply(config_df$value, function(x) {trimws(x)}))

con <- td_create_context(host = config_list[1], uid = config_list[2], pwd = config_list[3], dType = "native")


## ----create_tbls, message=FALSE, warning=FALSE, results="hide"-----------
first <- mtcars[1:20, ]
second <- mtcars[10:32, ]

# Copy into a remote data source
df_first <- copy_to(con, first, name = "mtcars_first", row.names = TRUE)
df_second <- copy_to(con, second, name = "mtcars_second", row.names = TRUE)

## ----Print the data using **head** function------------------------------
# Print data.frame instead of tbl_teradata to display all rows.
print(as.data.frame(df_first))

# Print data.frame instead of tbl_teradata to display all rows.
print(as.data.frame(df_second))

## ----union---------------------------------------------------------------
df_union <- df_first %>% union(df_second)

# Print data.frame instead of tbl_teradata to display full results.
print(as.data.frame(df_union))

## ----union_all-----------------------------------------------------------
df_union_all <- df_first %>% union_all(df_second)

# Print data.frame instead of tbl_teradata to display full results.
print(as.data.frame(df_union_all))

## ----intersect-----------------------------------------------------------
df_intersect <- df_first %>% intersect(df_second)

# Print data.frame instead of tbl_teradata to display full results.
print(as.data.frame(df_intersect))

## ----setdiff-------------------------------------------------------------
df_setdiff <- df_first %>% setdiff(df_second)

# Print data.frame instead of tbl_teradata to display full results.
print(as.data.frame(df_setdiff))

