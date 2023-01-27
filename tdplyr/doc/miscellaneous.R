## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
options(tibble.print_min = 6L, tibble.print_max = 6L, digits = 3)
library(knitr)
library(dbplyr)
library(dplyr)
library(tdplyr)

opts <- list(db = FALSE, machine = FALSE, driver = FALSE)
options(td.db_desc.output = opts)

# Reading Vantage cluster details from td_config.cfg
config_file <- system.file("extdata", "td_config.cfg", package = "tdplyr")
config_df <- read.csv(config_file, sep = ":", stringsAsFactors = FALSE, header = TRUE)
config_list <- unlist(lapply(config_df$value, function(x) {trimws(x)}))

con <- td_create_context(host = config_list[1], uid = config_list[2], pwd = config_list[3], dType = "native")


## ---- results = "hide", warning=FALSE,  message = FALSE-----------------------
loadExampleData("antiselect_example", "antiselect_input")

# Create tbl_teradata object.
df <- tbl(con, "antiselect_input")

# Create a tibble from tbl_teradata object.
df1 <- as_tibble(df)

# Create a R dataframe from tbl_teradata object.
df2 <- as.data.frame(df)

## -----------------------------------------------------------------------------
# Print the tbl_teradata object.
df %>% arrange(rowids)

# Get number of rows of tbl_teradata object.
td_nrow(df)

# Get number of rows of tibble object.
td_nrow(df1)

# Get number of rows of R dataframe.
td_nrow(df2)

## -----------------------------------------------------------------------------
# Reducing the number of columns.
df <- df %>% select(rowids, orderid, priority, quantity)

# Example 1: Get two samples of 3 rows and 2 rows each. Note that the column 'sampleid' takes 2 values. Each sampleid value represents one sample.
df1 <- td_sample(df = df, n = c(3, 2))

# Print the results.
df1 %>% arrange(rowids)

# Example 2: Get a sample of 3 rows. Note that all the rows have sampleid = 1.
df1 <- td_sample(df = df, n = 3)

# Print the results.
df1 %>% arrange(rowids)

# Example 3: Get 50% of total rows.
df1 <- td_sample(df = df, n = 0.5)

# Print the results.
df1 %>% arrange(rowids)

# Example 4: Get two samples each containing 30% and 50% of rows.
df1 <- td_sample(df = df, n = c(0.3, 0.5))

# Print the results.
df1 %>% arrange(rowids)

# Example 5: Get 10 rows from a tbl_teradata object of 7 rows without setting with.replacement. Default value of the argument 'with.replacement' is FALSE. Note that only 7 rows are returned as the table doesn't contain 10 rows.
df1 <- td_sample(df = df, n = 10)

# Print the results.
df1 %>% arrange(rowids)

# Check the number of rows of 'df1'.
td_nrow(df1)

# Example 6: Get 10 rows from a tbl_teradata object of 7 rows setting with.replacement to TRUE. 'randomize = TRUE'ensures sampling is done across AMPs in large datasets. Note that 10 rows are returned and there are duplicates for some rows.
df1 <- td_sample(df = df, n = 10, with.replacement = TRUE, randomize = TRUE)

# Print the results.
df1 %>% arrange(rowids)

# Check the number of rows of 'df1'.
td_nrow(df1)

# Example 7 : Get 5 rows which satisfy the condition 'orderid < 300' from a tbl_teradata object. Here, only three rows are returned as the total number of rows which satisfy this condition is 3.
df1 <- td_sample(df, when_then = list("orderid < 300" = 5))

# Print the results.
df1 %>% arrange(rowids)

# Check the number of rows of 'df1'.
td_nrow(df1)

# If with.replacement is set to TRUE, then 5 rows will be returned.
df1 <- td_sample(df, when_then = list("orderid < 300" = 5), with.replacement = TRUE)

# Check the number of rows of 'df1'.
td_nrow(df1)

# Example 8: Get 4 rows (1 row in first sample and 3 rows in second sample) which satisfy the condition 'orderid < 300' from a tbl_teradata object. Here, only 2 rows have sampleid = 2 as the total number of rows which satisfy this condition is 3.
df1 <- td_sample(df, when_then = list("orderid < 300" = c(1, 3)))

# Print the results.
df1 %>% arrange(rowids)

# Check the number of rows of 'df1'.
td_nrow(df1)

# If with.replacement is set to TRUE, then 3 rows having sampleid = 2 will be returned.
df1 <- td_sample(df, when_then = list("orderid < 300" = c(1, 3)), with.replacement = TRUE)

# Check the number of rows of 'df1'.
td_nrow(df1)

# Example 9: Using stratified sampling with multiple conditions : 4 rows (1 row in first sample and 3 rows in second sample) when orderid < 300 and 2 rows when priority != "high". Note that only 3 rows are returned when orderid < 300.
df1 <- td_sample(df, when_then = list("orderid < 300" = c(1, 3), "priority <> 'high'" = 2))

# Print the results.
df1 %>% arrange(rowids)

# Example 10: Using 'case_else' argument for stratified sampling : 2 rows when orderid < 300 and 3 rows from the remaining rows (rows which doesn't satisfy orderid < 300).
df1 <- td_sample(df, when_then = list("orderid < 300" = 2), case_else = 3)

# Print the results.
df1 %>% arrange(rowids)

