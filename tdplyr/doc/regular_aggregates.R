## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
options(tibble.print_min = 6L, tibble.print_max = 6L, digits = 3)

## ----create_connection, echo=FALSE, include=FALSE-----------------------------
library(dplyr)
library(DBI)
library(tdplyr)

# Reading Vantage cluster details from td_config.cfg
config_file <- system.file("extdata", "td_config.cfg", package = "tdplyr")
config_df <- read.csv(config_file, sep = ":", stringsAsFactors = FALSE, header = TRUE)
config_list <- unlist(lapply(config_df$value, function(x) {trimws(x)}))

con <- td_create_context(host = config_list[1], uid = config_list[2], pwd = config_list[3], dType = "native")


## ----create_tbl_objects, results = "hide", warning=FALSE, message = FALSE-----
loadExampleData("time_series_example", "ocean_buoys_seq")
# Create tbl_teradata objects.
df_seq <- tbl(con, "ocean_buoys_seq")

## ----median_runnable----------------------------------------------------------
# Calculate the median value of the 'temperature' column grouped by 'buoyid' column.
df_median <- df_seq %>% group_by(buoyid) %>% summarise(median_temp = median(temperature))

# Print the results.
df_median %>% arrange(buoyid)

# Calculate the median value of the 'temperature' column without any grouping.
df_median <- df_seq %>% summarise(median_temp = median(temperature))

# Print the results.
df_median

## ----median_not_runnable, eval=FALSE------------------------------------------
#  # Example 1
#  
#  # Using 'distinct()' to calculate median on distinct temperature values.
#  df_median <- df_seq %>% group_by(buoyid) %>% summarise(median_temp = median(distinct(temperature)))
#  
#  # Print the results.
#  # This throws an error "Illegal use of DISTINCT Aggregate Expressions."
#  df_median
#  
#  # Example 2
#  
#  # Incorrect usage of median() with mutate and filter.
#  df_median <- df_seq %>% mutate(median_t = median(temperature))
#  df_median <- df_seq %>% filter(temperature <= median(temperature))

## ----kurtosis_runnable--------------------------------------------------------
# Calculate the kurtosis value of the 'temperature' column grouped by 'buoyid' column.
df_kurtosis <- df_seq %>% group_by(buoyid) %>% summarise(kurtosis_temp = kurtosis(temperature))

# Print the results.
df_kurtosis %>% arrange(buoyid)

# Exclude duplicates in the same aggregate operation above.
df_kurtosis <- df_seq %>% group_by(buoyid) %>% summarise(kurtosis_temp = kurtosis(distinct(temperature)))

# Print the results.
df_kurtosis %>% arrange(buoyid)

# Calculate the kurtosis value of the 'temperature' column without any grouping.
df_kurtosis <- df_seq %>% summarise(kurtosis_temp = kurtosis(temperature))

# Print the results.
df_kurtosis

# Exclude duplicates in the same aggregate operation above.
df_kurtosis <- df_seq %>% summarise(kurtosis_temp = kurtosis(distinct(temperature)))

# Print the results.
df_kurtosis

## ----kurtosis_not_runnable, eval=FALSE----------------------------------------
#  # Incorrect usage of kurtosis() with mutate and filter.
#  df_kurtosis <- df_seq %>% mutate(kurtosis_t = kurtosis(temperature))
#  df_kurtosis <- df_seq %>% filter(temperature <= kurtosis(temperature))

## ----skew_runnable------------------------------------------------------------
# Calculate the skewness of the 'temperature' column grouped by 'buoyid' column.
df_skew <- df_seq %>% group_by(buoyid) %>% summarise(skew_temp = skew(temperature))

# Print the results.
df_skew %>% arrange(buoyid)

# Exclude duplicates in the same aggregate operation above.
df_skew <- df_seq %>% group_by(buoyid) %>% summarise(skew_temp = skew(distinct(temperature)))

# Print the results.
df_skew %>% arrange(buoyid)

# Calculate the skewness of the 'temperature' column without any grouping.
df_skew <- df_seq %>% summarise(skew_temp = skew(temperature))

# Print the results.
df_skew

# Exclude duplicates in the same aggregate operation above.
df_skew <- df_seq %>% summarise(skew_temp = skew(distinct(temperature)))

# Print the results.
df_skew

## ----skew_not_runnable, eval=FALSE--------------------------------------------
#  # Incorrect usage of skew() with mutate and filter.
#  df_skew <- df_seq %>% mutate(skew_t = skew(temperature))
#  df_skew <- df_seq %>% filter(temperature <= skew(temperature))

## ----sum_group----------------------------------------------------------------
# Calculate the sum of the values in the 'temperature' column grouped by 'buoyid' column.
# Please note that the output of the function remains the same (excludes NULL/NA values in computation) irrespective of the argument 'na.rm' set to TRUE. Warning 'Missing values are always removed in SQL.' will be suppressed if this is set to TRUE, otherwise the warning is raised.
df_sum <- df_seq %>% group_by(buoyid) %>% summarise(sum_temp = sum(temperature, na.rm = TRUE))

# Print the results.
df_sum %>% arrange(buoyid)

# Exclude duplicates in the same aggregate operation above.
df_sum <- df_seq %>% group_by(buoyid) %>% summarise(sum_temp = sum(distinct(temperature)))

# Print the results.
df_sum %>% arrange(buoyid)

# Calculate the sum of the values in the 'temperature' column without any grouping.
df_sum <- df_seq %>% summarise(sum_temp = sum(temperature))

# Print the results.
df_sum

# Exclude duplicates in the same aggregate operation above.
df_sum <- df_seq %>% summarise(sum_temp = sum(distinct(temperature)))

# Print the results.
df_sum

## ----sum_window---------------------------------------------------------------
# Create a new column 'sum_t' which holds the sum of values in the 'temperature' column.
df_sum <- df_seq %>% mutate(sum_t = sum(temperature))

# Print the query.
df_sum %>% show_query()

# Print the results.
df_sum %>% arrange(TD_TIMECODE, TD_SEQNO)

# Filter the rows with temperature values less than one-hundredth of the sum of values in the 'temperature' column.
df_sum <- df_seq %>% filter(temperature < sum(temperature)/100)

# Print the results.
df_sum

## ----stddev_samp_group--------------------------------------------------------
# Calculate the sample standard deviation values of the 'temperature' column grouped by 'buoyid' column.
# Please note that the output of the function remains the same (excludes NULL/NA values in computation) irrespective of the argument 'na.rm' set to TRUE. Warning 'Missing values are always removed in SQL.' will be suppressed if this is set to TRUE, otherwise the warning is raised.
df_sd <- df_seq %>% group_by(buoyid) %>% summarise(sd_temp = sd(temperature, na.rm = TRUE))

# Print the results.
df_sd %>% arrange(buoyid)

# Exclude duplicates in the same aggregate operation above.
df_sd <- df_seq %>% group_by(buoyid) %>% summarise(sd_temp = sd(distinct(temperature)))

# Print the results.
df_sd %>% arrange(buoyid)

# Calculate the sample standard deviation of the values in the 'temperature' column without any grouping.
df_sd <- df_seq %>% summarise(sd_temp = sd(temperature))

# Print the results.
df_sd

# Exclude duplicates in the same aggregate operation above.
df_sd <- df_seq %>% summarise(sd_temp = sd(distinct(temperature)))

# Print the results.
df_sd

## ----stddev_samp_window-------------------------------------------------------
# Create a new column 'sd_t' which holds the sample standard deviation of the values in the 'temperature' column.
df_sd <- df_seq %>% mutate(sd_t = sd(temperature))

# Print the query.
df_sd %>% show_query()

# Print the results.
df_sd %>% arrange(TD_TIMECODE, TD_SEQNO)

# Filter the rows with temperature values less than the sample standard deviation of the values in the 'temperature' column.
df_sd <- df_seq %>% filter(temperature < sd(temperature))

# Print the results.
df_sd

## ----stddev_pop_group---------------------------------------------------------
# Calculate the population standard deviation values of the 'temperature' column grouped by 'buoyid' column.
# Please note that the output of the function remains the same (excludes NULL/NA values in computation) irrespective of the argument 'na.rm' set to TRUE. Warning 'Missing values are always removed in SQL.' will be suppressed if this is set to TRUE, otherwise the warning is raised.
df_sdp <- df_seq %>% group_by(buoyid) %>% summarise(sdp_temp = sdp(temperature, na.rm = TRUE))

# Print the results.
df_sdp %>% arrange(buoyid)

# Exclude duplicates in the same aggregate operation above.
df_sdp <- df_seq %>% group_by(buoyid) %>% summarise(sdp_temp = sdp(distinct(temperature)))

# Print the results.
df_sdp %>% arrange(buoyid)

# Calculate the population standard deviation of the values in the 'temperature' column without any grouping.
df_sdp <- df_seq %>% summarise(sdp_temp = sdp(temperature))

# Print the results.
df_sdp

# Exclude duplicates in the same aggregate operation above.
df_sdp <- df_seq %>% summarise(sdp_temp = sdp(distinct(temperature)))

# Print the results.
df_sdp

## ----stddev_pop_window--------------------------------------------------------
# Create a new column 'sd_t' which holds the population standard deviation of the values in the 'temperature' column.
df_sdp <- df_seq %>% mutate(sd_t = sdp(temperature))

# Print the query.
df_sdp %>% show_query()

# Print the results.
df_sdp %>% arrange(TD_TIMECODE, TD_SEQNO)

# Filter the rows with temperature values less than the population standard deviation of the values in the 'temperature' column.
df_sdp <- df_seq %>% filter(temperature < sdp(temperature))

# Print the results.
df_sdp

## ----min_group----------------------------------------------------------------
# Calculate the minimum value in the 'temperature' column grouped by 'buoyid' column.
# Please note that the output of the function remains the same (excludes NULL/NA values in computation) irrespective of the argument 'na.rm' set to TRUE. Warning 'Missing values are always removed in SQL.' will be suppressed if this is set to TRUE, otherwise the warning is raised.
df_min <- df_seq %>% group_by(buoyid) %>% summarise(min_temp = min(temperature, na.rm = TRUE))

# Print the results.
df_min %>% arrange(buoyid)

# Exclude duplicates in the same aggregate operation above.
df_min <- df_seq %>% group_by(buoyid) %>% summarise(min_temp = min(distinct(temperature)))

# Print the results.
df_min %>% arrange(buoyid)

# Calculate the minimum value in the 'temperature' column without any grouping.
df_min <- df_seq %>% summarise(min_temp = min(temperature))

# Print the results.
df_min

# Exclude duplicates in the same aggregate operation above.
df_min <- df_seq %>% summarise(min_temp = min(distinct(temperature)))

# Print the results.
df_min

## ----min_window---------------------------------------------------------------
# Create a new column 'min_t' which holds the minimum value in the 'temperature' column.
df_min <- df_seq %>% mutate(min_t = min(temperature))

# Print the query.
df_min %>% show_query()

# Print the results.
df_min %>% arrange(TD_TIMECODE, TD_SEQNO)

# Filter the rows with one-tenth of temperature values greater than or equal to the minimum value in 'temperature' column.
df_min <- df_seq %>% filter(temperature/10 >= min(temperature))

# Print the results.
df_min

## ----max_group----------------------------------------------------------------
# Calculate the maximum value in the 'temperature' column grouped by 'buoyid' column.
# Please note that the output of the function remains the same (excludes NULL/NA values in computation) irrespective of the argument 'na.rm' set to TRUE. Warning 'Missing values are always removed in SQL.' will be suppressed if this is set to TRUE, otherwise the warning is raised.
df_max <- df_seq %>% group_by(buoyid) %>% summarise(max_temp = max(temperature, na.rm = TRUE))

# Print the results.
df_max %>% arrange(buoyid)

# Exclude duplicates in the same aggregate operation above.
df_max <- df_seq %>% group_by(buoyid) %>% summarise(max_temp = max(distinct(temperature)))

# Print the results.
df_max %>% arrange(buoyid)

# Calculate the maximum value in the 'temperature' column without any grouping.
df_max <- df_seq %>% summarise(max_temp = max(temperature))

# Print the results.
df_max

# Exclude duplicates in the same aggregate operation above.
df_max <- df_seq %>% summarise(max_temp = max(distinct(temperature)))

# Print the results.
df_max

## ----max_window---------------------------------------------------------------
# Create a new column 'max_t' which holds the maximum value in the 'temperature' column.
df_max <- df_seq %>% mutate(max_t = max(temperature))

# Print the query.
df_max %>% show_query()

# Print the results.
df_max %>% arrange(TD_TIMECODE, TD_SEQNO)

# Filter the rows with temperature values less than or equal to one-tenth of the maximum value in 'temperature' column.
df_max <- df_seq %>% filter(temperature <= max(temperature)/10)

# Print the results.
df_max

## ----var_samp_group-----------------------------------------------------------
# Calculate the sample variance of the 'temperature' column grouped by 'buoyid' column.
# Please note that the output of the function remains the same (excludes NULL/NA values in computation) irrespective of the argument 'na.rm' set to TRUE. Warning 'Missing values are always removed in SQL.' will be suppressed if this is set to TRUE, otherwise the warning is raised.
df_var <- df_seq %>% group_by(buoyid) %>% summarise(var_temp = var(temperature, na.rm = TRUE))

# Print the results.
df_var %>% arrange(buoyid)

# Exclude duplicates in the same aggregate operation above.
df_var <- df_seq %>% group_by(buoyid) %>% summarise(var_temp = var(distinct(temperature)))

# Print the results.
df_var %>% arrange(buoyid)

# Calculate the sample variance of the values in the 'temperature' column without any grouping.
df_var <- df_seq %>% summarise(var_temp = var(temperature))

# Print the results.
df_var

# Exclude duplicates in the same aggregate operation above.
df_var <- df_seq %>% summarise(var_temp = var(distinct(temperature)))

# Print the results.
df_var

## ----var_samp_window----------------------------------------------------------
# Create a new column 'var_t' which holds the sample variance of the values in the 'temperature' column.
df_var <- df_seq %>% mutate(var_t = var(temperature))

# Print the query.
df_var %>% show_query()

# Print the results.
df_var %>% arrange(TD_TIMECODE, TD_SEQNO)

# Filter the rows with temperature values less than one-tenth of the sample variance of the values in the 'temperature' column.
df_var <- df_seq %>% filter(temperature < var(temperature)/10)

# Print the results.
df_var

## ----var_pop_group------------------------------------------------------------
# Calculate the population variance of the 'temperature' column grouped by 'buoyid' column.
# Please note that the output of the function remains the same (excludes NULL/NA values in computation) irrespective of the argument 'na.rm' set to TRUE. Warning 'Missing values are always removed in SQL.' will be suppressed if this is set to TRUE, otherwise the warning is raised.
df_varp <- df_seq %>% group_by(buoyid) %>% summarise(varp_temp = varp(temperature, na.rm = TRUE))

# Print the results.
df_varp %>% arrange(buoyid)

# Exclude duplicates in the same aggregate operation above.
df_varp <- df_seq %>% group_by(buoyid) %>% summarise(varp_temp = varp(distinct(temperature)))

# Print the results.
df_varp %>% arrange(buoyid)

# Calculate the population variance of the values in the 'temperature' column without any grouping.
df_varp <- df_seq %>% summarise(varp_temp = varp(temperature))

# Print the results.
df_varp

# Exclude duplicates in the same aggregate operation above.
df_varp <- df_seq %>% summarise(varp_temp = varp(distinct(temperature)))

# Print the results.
df_varp

## ----var_pop_window-----------------------------------------------------------
# Create a new column 'var_t' which holds the population variance of the values in the 'temperature' column.
df_varp <- df_seq %>% mutate(var_t = varp(temperature))

# Print the query.
df_varp %>% show_query()

# Print the results.
df_varp %>% arrange(TD_TIMECODE, TD_SEQNO)

# Filter the rows with temperature values less than one-tenth of the population variance of the values in the 'temperature' column.
df_varp <- df_seq %>% filter(temperature < varp(temperature)/10)

# Print the results.
df_varp

## ----avg_group----------------------------------------------------------------
# Calculate the average value in the 'temperature' column grouped by 'buoyid' column.
# Please note that the output of the function remains the same (excludes NULL/NA values in computation) irrespective of the argument 'na.rm' set to TRUE. Warning 'Missing values are always removed in SQL.' will be suppressed if this is set to TRUE, otherwise the warning is raised.
df_avg <- df_seq %>% group_by(buoyid) %>% summarise(avg_temp = mean(temperature, na.rm = TRUE))

# Print the results.
df_avg %>% arrange(buoyid)

# Exclude duplicates in the same aggregate operation above.
df_avg <- df_seq %>% group_by(buoyid) %>% summarise(avg_temp = mean(distinct(temperature)))

# Print the results.
df_avg %>% arrange(buoyid)

# Calculate the average value in the 'temperature' column without any grouping.
df_avg <- df_seq %>% summarise(avg_temp = mean(temperature))

# Print the results.
df_avg

# Exclude duplicates in the same aggregate operation above.
df_avg <- df_seq %>% summarise(avg_temp = mean(distinct(temperature)))

# Print the results.
df_avg

## ----avg_window---------------------------------------------------------------
# Create a new column 'avg_t' which holds the average value in the 'temperature' column.
df_avg <- df_seq %>% mutate(avg_t = mean(temperature))

# Print the query.
df_avg %>% show_query()

# Print the results.
df_avg %>% arrange(TD_TIMECODE, TD_SEQNO)

# Filter the rows with temperature values less than one-fifth of the average value in 'temperature' column.
df_avg <- df_seq %>% filter(temperature < mean(temperature)/5)

# Print the results.
df_avg

## ----count_group--------------------------------------------------------------
# Calculate the number of rows in the 'temperature' column grouped by 'buoyid' column.
df_count <- df_seq %>% group_by(buoyid) %>% summarise(count_temp = n(temperature))

# Print the results.
df_count %>% arrange(buoyid)

# Exclude duplicates in the same aggregate operation above.
df_count <- df_seq %>% group_by(buoyid) %>% summarise(count_temp = n(distinct(temperature)))

# Print the results.
df_count %>% arrange(buoyid)

# Another way of excluding duplicates in the same aggregate operation above.
df_count <- df_seq %>% group_by(buoyid) %>% summarise(count_temp = n_distinct(temperature))

# Print the results.
df_count %>% arrange(buoyid)

# Calculate the number of rows in the table grouped by 'buoyid' column.
# Note that the argument is not provided for the function `n()`.
df_count <- df_seq %>% group_by(buoyid) %>% summarise(count_temp = n())

# Print the results.
df_count %>% arrange(buoyid)

# Calculate the number of rows in the 'temperature' column without any grouping.
df_count <- df_seq %>% summarise(count_temp = n(temperature))

# Print the results.
df_count

# Exclude duplicates in the same aggregate operation above.
df_count <- df_seq %>% summarise(count_temp = n(distinct(temperature)))

# Print the results.
df_count

# Calculate the number of rows in the table without any grouping.
# Note that the argument is not provided for the function `n()`.
df_count <- df_seq %>% summarise(count_temp = n())

# Print the results.
df_count

## ----count_window-------------------------------------------------------------
# Create a new column 'count_t' which holds the number of rows in the 'temperature' column.
df_count <- df_seq %>% mutate(count_t = n(temperature))

# Print the query.
df_count %>% show_query()

# Print the results.
df_count %>% select(TD_TIMECODE, TD_SEQNO, buoyid, count_t) %>% arrange(TD_TIMECODE, TD_SEQNO)

# Create a new column 'count_t' which holds the number of rows in the table.
# Note that the argument is not provided for the function `n()`.
df_count <- df_seq %>% mutate(count_t = n())

# Print the query.
df_count %>% show_query()

# Print the results.
df_count %>% select(TD_TIMECODE, TD_SEQNO, buoyid, count_t) %>% arrange(TD_TIMECODE, TD_SEQNO)

# Filter the rows with temperature values less than the number of rows in 'temperature' column.
df_count <- df_seq %>% filter(temperature < n(temperature))

# Print the results.
df_count %>% arrange(TD_TIMECODE, TD_SEQNO)

