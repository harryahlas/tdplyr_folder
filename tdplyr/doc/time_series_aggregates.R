## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
options(tibble.print_min = 6L, tibble.print_max = 6L, digits = 3)
library(knitr)

## ----load_lib_create_connection, echo=FALSE, include=FALSE--------------------
library(dplyr)
library(DBI)
library(tdplyr)

# Reading Vantage cluster details from td_config.cfg
config_file <- system.file("extdata", "td_config.cfg", package = "tdplyr")
config_df <- read.csv(config_file, sep = ":", stringsAsFactors = FALSE, header = TRUE)
config_list <- unlist(lapply(config_df$value, function(x) {trimws(x)}))

con <- td_create_context(host = config_list[1], uid = config_list[2], pwd = config_list[3], dType = "native")


## ----create_tbls, message=FALSE, warning=FALSE, results="hide"----------------
loadExampleData("time_series_example", "ocean_buoys_seq", "ocean_buoys_nonseq", "ocean_buoys_nonpti", "package_tracking_pti", "package_tracking_nonpti")
# Create tbl_teradata objects.
df_seq <- tbl(con, "ocean_buoys_seq")
df_nonseq <- tbl(con, "ocean_buoys_nonseq")
df_nonpti <- tbl(con, "ocean_buoys_nonpti")
df_pack_pti <- tbl(con, "package_tracking_pti")
df_pack_nonpti <- tbl(con, "package_tracking_nonpti")

## ----group_by_time------------------------------------------------------------
# Grouping the sequenced PTI tables based on time with timebucket duration of 30 minutes and the column 'buoyid'. Note the use of shorthand notation for timebucket duration.
df_seq_grp <- df_seq %>% 
  group_by_time(timebucket.duration = "30m", value.expression = "buoyid")

# Grouping the non-PTI tables based on time with timebucket duration of 1 minute and filling the missing timebuckets with previous values. Note the use of formal notation for timebucket duration and timecode.column argument (mandatory for non-PTI table).
df_nonpti_grp <- df_nonpti %>% 
  group_by_time(timebucket.duration = "MINUTES(1)", timecode.column = "TIMECODE", fill = "PREV")

# Grouping the non-PTI tables based on time with timebucket duration of 1 Calendar Year with fill = 10000 (some numeric constant)
df_nonseq_grp <- df_nonseq %>% 
  group_by_time(timebucket.duration = "CAL_YEARS(1)", fill = 10000)

## ----mode_runnable------------------------------------------------------------
# Calculate the mode of the 'temperature' column of sequenced PTI table.
df_seq_mode <- df_seq_grp %>% summarise(mode_temp = ts.mode(temperature))

# Print the results.
df_seq_mode %>% arrange(TIMECODE_RANGE, buoyid, mode_temp)

# Calculate the mode of the 'temperature' column of non-sequenced PTI table.
df_nonseq_mode <- df_nonseq_grp %>% summarise(mode_temp = ts.mode(temperature))

# Print the results.
df_nonseq_mode %>% arrange(TIMECODE_RANGE, mode_temp)

# Calculate the mode of the 'temperature' column of non-PTI table.
df_nonpti_mode <- df_nonpti_grp %>% summarise(mode_temp = ts.mode(temperature))

# Print the results.
df_nonpti_mode %>% arrange(TIMECODE_RANGE, mode_temp)

## ----bottom_runnable----------------------------------------------------------
# Get the smallest 2 values of the 'temperature' column for each group without ties of sequenced PTI table.
df_seq_bottom <- df_seq_grp %>% summarise(bottom_temp = ts.bottom(temperature, 2))

# Print the results.
df_seq_bottom %>% arrange(TIMECODE_RANGE, buoyid, bottom_temp)

# Get the smallest 4 values of the 'temperature' column for each group with ties of non-sequenced PTI table.
df_nonseq_bottom <- df_nonseq_grp %>% summarise(bottom_temp = ts.bottom(temperature, 4, TRUE))

# Print the results.
df_nonseq_bottom %>% arrange(TIMECODE_RANGE, bottom_temp)

# Get the smallest 2 values of the 'temperature' column for each group with ties of non-PTI table.
df_nonpti_bottom <- df_nonpti_grp %>% summarise(bottom_temp = ts.bottom(temperature, 2, TRUE))

# Print the results.
df_nonpti_bottom %>% arrange(TIMECODE_RANGE, bottom_temp)

## ----top_runnable-------------------------------------------------------------
# Get the largest 2 values of the 'temperature' column for each group without ties of sequenced PTI table.
df_seq_top <- df_seq_grp %>% summarise(top_temp = ts.top(temperature, 2))

# Print the results.
df_seq_top %>% arrange(TIMECODE_RANGE, buoyid, top_temp)

# Get the largest 4 values of the 'temperature' column for each group with ties of non-sequenced PTI table.
df_nonseq_top <- df_nonseq_grp %>% summarise(top_temp = ts.top(temperature, 4, TRUE))

# Print the results.
df_nonseq_top %>% arrange(TIMECODE_RANGE, top_temp)

# Get the largest 2 values of the 'temperature' column for each group with ties of non-PTI table.
df_nonpti_top <- df_nonpti_grp %>% summarise(top_temp = ts.top(temperature, 2, TRUE))

# Print the results.
df_nonpti_top %>% arrange(TIMECODE_RANGE, top_temp)

## ----median_runnable----------------------------------------------------------
# Calculate the median of the 'temperature' column of sequenced PTI table.
df_seq_median <- df_seq_grp %>% summarise(median_temp = ts.median(temperature))

# Print the results.
df_seq_median %>% arrange(TIMECODE_RANGE, buoyid, median_temp)

# Calculate the median of the 'temperature' column of non-sequenced PTI table, excluding the duplicates in the computation.
df_nonseq_median <- df_nonseq_grp %>% summarise(median_temp = ts.median(temperature, TRUE))

# Another way of excluding duplicates for median.
df_nonseq_median <- df_nonseq_grp %>% summarise(median_temp = ts.median(distinct(temperature)))

# Print the results.
df_nonseq_median %>% arrange(TIMECODE_RANGE, median_temp)

# Calculate the median of the 'temperature' column of non-PTI table.
df_nonpti_median <- df_nonpti_grp %>% summarise(median_temp = ts.median(temperature))

# Print the results.
df_nonpti_median %>% arrange(TIMECODE_RANGE, median_temp)

## ----median_not_runnable, eval=FALSE------------------------------------------
#  # Incorrect usage of ts.median() with mutate and filter
#  df_median <- df_seq %>% mutate(median_t = ts.median(temperature))
#  df_median <- df_seq %>% filter(temperature <= ts.median(temperature))

## ----first_runnable-----------------------------------------------------------
# Get the oldest value of the 'temperature' column of sequenced PTI table.
df_seq_first <- df_seq_grp %>% summarise(first_temp = ts.first(temperature))

# Print the results.
df_seq_first %>% arrange(TIMECODE_RANGE, buoyid, first_temp)

# Get the oldest value of the 'temperature' column of non-sequenced PTI table.
df_nonseq_first <- df_nonseq_grp %>% summarise(first_temp = ts.first(temperature))

# Print the results.
df_nonseq_first %>% arrange(TIMECODE_RANGE, first_temp)

# Get the oldest value of the 'temperature' column of non-PTI table.
df_nonpti_first <- df_nonpti_grp %>% summarise(first_temp = ts.first(temperature))

# Print the results.
df_nonpti_first %>% arrange(TIMECODE_RANGE, first_temp)

## ----last_runnable------------------------------------------------------------
# Get the newest value of the 'temperature' column of sequenced PTI table.
df_seq_last <- df_seq_grp %>% summarise(last_temp = ts.last(temperature))

# Print the results.
df_seq_last %>% arrange(TIMECODE_RANGE, buoyid, last_temp)

# Get the newest value of the 'temperature' column of non-sequenced PTI table.
df_nonseq_last <- df_nonseq_grp %>% summarise(last_temp = ts.last(temperature))

# Print the results.
df_nonseq_last %>% arrange(TIMECODE_RANGE, last_temp)

# Get the newest value of the 'temperature' column of non-PTI table.
df_nonpti_last <- df_nonpti_grp %>% summarise(last_temp = ts.last(temperature))

# Print the results.
df_nonpti_last %>% arrange(TIMECODE_RANGE, last_temp)

## ----last_not_runnable, eval = FALSE------------------------------------------
#  # Invalid example 1: Using ts.last() on `group_by` with aggregate column not as grouping column.
#  df1 <- df_seq %>% group_by(buoyid) %>% summarise(temp = ts.last(temperature))
#  
#  # Note that this will not print the result but throws an exception - "Selected non-aggregate values must be part of the associated group.". This is because the aggregate operation "LAST" (considered here) is the Period data type function.
#  df1
#  
#  # Invalid example 2: Using ts.last() on `group_by` with aggregate column as one of the grouping columns.
#  df2 <- df %>% group_by(buoyid, temperature) %>% summarise(temp = ts.last(temperature))
#  
#  # Note that this will not print the result but throws an exception - "Invalid argument for the LAST function. The argument must have a Period data type.". This is because the aggregate operation "LAST" (considered here) is the Period data type function.
#  df2

## ----mad_runnable-------------------------------------------------------------
# Calculate the MAD value of the 'temperature' column of sequenced PTI table.
df_seq_mad <- df_seq_grp %>% summarise(mad_temp = ts.mad(temperature))

# Print the results.
df_seq_mad %>% arrange(TIMECODE_RANGE, buoyid, mad_temp)

# Calculate the MAD value of the 'temperature' column of non-sequenced PTI table.
df_nonseq_mad <- df_nonseq_grp %>% summarise(mad_temp = ts.mad(temperature, 2))

# Print the results.
df_nonseq_mad %>% arrange(TIMECODE_RANGE, mad_temp)

# Calculate the MAD value of the 'temperature' column of non-PTI table.
df_nonpti_mad <- df_nonpti_grp %>% summarise(mad_temp = ts.mad(temperature, 5))

# Print the results.
df_nonpti_mad %>% arrange(TIMECODE_RANGE, mad_temp)

## ----percentile_runnable------------------------------------------------------
# Calculate the 25th percentile of the 'temperature' column of sequenced PTI table. When the desired result value lies between two data points, linear interpolation is used as it is the default interpolation scheme.
df_seq_percentile <- df_seq_grp %>% summarise(percent_temp = ts.percentile(temperature, 25))

# Print the results.
df_seq_percentile %>% arrange(TIMECODE_RANGE, buoyid, percent_temp)

# Calculate the 50th percentile of the 'temperature' column of non-sequenced PTI table. When the desired result value lies between two data points, nearest value interpolation is used.
df_nonseq_percentile <- df_nonseq_grp %>% summarise(percent_temp = ts.percentile(temperature, 50, "NEAREST"))

# Print the results.
df_nonseq_percentile %>% arrange(TIMECODE_RANGE, percent_temp)

# Calculate the 75th percentile of the distinct values in the 'temperature' column of non-PTI table. When the desired result value lies between two data points, low value interpolation is used.
df_nonpti_percentile <- df_nonpti_grp %>% summarise(percent_temp = ts.percentile(distinct(temperature), 75, "LOW"))

# Print the results.
df_nonpti_percentile %>% arrange(TIMECODE_RANGE, percent_temp)

## ----delta_t_runnable---------------------------------------------------------
# Example 1: Measures the time between minimum and maximum observed temperatures every 30 minutes between 8:00 AM and 10:30 AM on each buoy of a nonsequenced PTI table.

# Filter the data and grab all rows between timestamp '2014-01-06 08:00:00' and '2014-01-06 10:30:00'.
df_filter <- df_nonseq %>% filter(TD_TIMECODE >= "2014-01-06 08:00:00" && TD_TIMECODE < "2014-01-06 10:30:00")

# Get the minimum and maximum temperature within time range of 30 minutes.
df_min_max_temp <- df_filter %>% group_by_time("30m", value.expression = "buoyid", timecode.column = "TD_TIMECODE") %>% 
                                 summarise(min_t = min(temperature, na.rm = TRUE), max_t = max(temperature, na.rm = TRUE))

# Join the tbl_teradata 'df_min_max_temp' with original tbl_teradata 'df_nonseq'.
df_join <- inner_join(df_nonseq, df_min_max_temp, by = "buoyid")

# Execute 'ts.delta_t' after grouping the joined tbl_teradata into time buckets of 1 day.
df_grp1 <- df_join %>% group_by_time(timebucket.duration = "DAYS(1)", value.expression = "buoyid", timecode.column = "TD_TIMECODE")
df_out <- df_grp1 %>% summarise(delta_val = ts.delta_t(temperature == min_t, temperature == max_t))

# Print the results.
df_out %>% arrange(TIMECODE_RANGE, buoyid)

# Using strings to arguments 'start.condition' and 'end.condition'. This will give the same output as that of 'df_out'.
df_out1 <- df_grp1 %>% summarise(delta_val = ts.delta_t(start.condition = "temperature = min_t", end.condition = "temperature = max_t"))

# Print the results.
df_out1 %>% arrange(TIMECODE_RANGE, buoyid)

# Example 2: Finding Time Elapsed between Shipping and Receiving an Item. Input data used for this example contains information about parcels sent by a delivery service.

# Case 1: Using tbl_teradata on PTI Table and showcasing usage of unbounded time in grouping.

# Execute group_by_time() using unbounded time for timebucket.duration.
df_grp <- df_pack_pti %>% group_by_time(timebucket.duration = "*", value.expression = "parcel_number")

# Execute 'ts.delta_t', with start and end conditions specified as strings.
df_out <- df_grp %>% summarise(delta_t = ts.delta_t("Status LIKE 'picked%up%customer'", "Status LIKE 'delivered%customer'"))

# Print the results.
df_out %>% arrange(TIMECODE_RANGE, parcel_number)

# Case 2: Using tbl_teradata on Non-PTI Table and showcasing usage of unbounded time in grouping.

# Execute group_by_time() using unbounded time for timebucket.duration.
df_grp <- df_pack_nonpti %>% group_by_time(timebucket.duration = "*", value.expression = "parcel_number", timecode.column = "clock_time")

# Execute 'ts.delta_t', with start and end conditions specified as strings.
df_out <- df_grp %>% summarise(delta_t = ts.delta_t("Status LIKE 'picked%up%customer'", "Status LIKE 'delivered%customer'"))

# Print the results.
df_out %>% arrange(TIMECODE_RANGE, parcel_number)

## ----kurtosis_runnable--------------------------------------------------------
# Calculate the Kurtosis of the 'temperature' column of sequenced PTI table.
df_seq_kurtosis <- df_seq_grp %>% summarise(kurtosis_temp = ts.kurtosis(temperature))

# Print the results.
df_seq_kurtosis %>% arrange(TIMECODE_RANGE, buoyid, kurtosis_temp)

# Exclude duplicates in the same aggregate operation above.
df_seq_kurtosis <- df_seq_grp %>% summarise(kurtosis_temp = ts.kurtosis(distinct(temperature)))

# Print the results.
df_seq_kurtosis %>% arrange(TIMECODE_RANGE, buoyid, kurtosis_temp)

# Calculate the Kurtosis of the 'temperature' column of non-sequenced PTI table.
df_nonseq_kurtosis <- df_nonseq_grp %>% summarise(kurtosis_temp = ts.kurtosis(temperature))

# Print the results.
df_nonseq_kurtosis %>% arrange(TIMECODE_RANGE, kurtosis_temp)

# Calculate the Kurtosis of the 'temperature' column of non-PTI table.
df_nonpti_kurtosis <- df_nonpti %>% group_by_time(timebucket.duration = "10m", timecode.column = "TIMECODE") %>% summarise(kurtosis_temp = ts.kurtosis(temperature))

# Print the results.
df_nonpti_kurtosis %>% arrange(TIMECODE_RANGE, kurtosis_temp)

## ----kurtosis_not_runnable, eval=FALSE----------------------------------------
#  # Incorrect usage of ts.kurtosis() with mutate and filter.
#  df_kurtosis <- df_seq %>% mutate(kurtosis_t = ts.kurtosis(temperature))
#  df_kurtosis <- df_seq %>% filter(temperature <= ts.kurtosis(temperature))

## ----skew_runnable------------------------------------------------------------
# Calculate the skewness of the 'temperature' column of sequenced PTI table.
df_seq_skew <- df_seq_grp %>% summarise(skew_temp = ts.skew(temperature))

# Print the results.
df_seq_skew %>% arrange(TIMECODE_RANGE, buoyid, skew_temp)

# Exclude duplicates in the same aggregate operation above.
df_seq_skew <- df_seq_grp %>% summarise(skew_temp = ts.skew(distinct(temperature)))

# Print the results.
df_seq_skew %>% arrange(TIMECODE_RANGE, buoyid, skew_temp)

# Calculate the skewness of the 'temperature' column of non-sequenced PTI table.
df_nonseq_skew <- df_nonseq_grp %>% summarise(skew_temp = ts.skew(temperature))

# Print the results.
df_nonseq_skew %>% arrange(TIMECODE_RANGE, skew_temp)

# Calculate the skewness of the 'temperature' column of non-PTI table.
df_nonpti_skew <- df_nonpti_grp %>% group_by_time(timebucket.duration = "10m", timecode.column = "TIMECODE") %>% summarise(skew_temp = ts.skew(temperature))

# Print the results.
df_nonpti_skew %>% arrange(TIMECODE_RANGE, skew_temp)

## ----skew_not_runnable, eval=FALSE--------------------------------------------
#  # Incorrect usage of ts.skew() with mutate and filter.
#  df_skew <- df_seq %>% mutate(skew_t = ts.skew(temperature))
#  df_skew <- df_seq %>% filter(temperature <= ts.skew(temperature))

## ----sum_runnable-------------------------------------------------------------
# Calculate the sum of the values in the 'temperature' column of sequenced PTI table.
df_seq_sum <- df_seq_grp %>% summarise(sum_temp = ts.sum(temperature))

# Print the results.
df_seq_sum %>% arrange(TIMECODE_RANGE, buoyid, sum_temp)

# Exclude duplicates in the same aggregate operation above.
df_seq_sum <- df_seq_grp %>% summarise(sum_temp = ts.sum(distinct(temperature)))

# Print the results.
df_seq_sum %>% arrange(TIMECODE_RANGE, buoyid, sum_temp)

# Calculate the sum of the values in the 'temperature' column of non-sequenced PTI table.
df_nonseq_sum <- df_nonseq_grp %>% summarise(sum_temp = ts.sum(temperature))

# Print the results.
df_nonseq_sum %>% arrange(TIMECODE_RANGE, sum_temp)

# Calculate the sum of the values in the 'temperature' column of non-PTI table.
df_nonpti_sum <- df_nonpti %>% group_by_time(timebucket.duration = "10m", timecode.column = "TIMECODE") %>% summarise(sum_temp = ts.sum(temperature))

# Print the results.
df_nonpti_sum %>% arrange(TIMECODE_RANGE, sum_temp)

## ----sum_not_runnable, eval=FALSE---------------------------------------------
#  # Incorrect usage of ts.sum() with mutate and filter.
#  df_sum <- df_seq %>% mutate(sum_t = ts.sum(temperature))
#  df_sum <- df_seq %>% filter(temperature <= ts.sum(temperature))

## ----stddev_samp_runnable-----------------------------------------------------
# Calculate the sample standard deviation in the 'temperature' column of sequenced PTI table.
df_seq_sd <- df_seq_grp %>% summarise(sd_temp = ts.sd(temperature))

# Print the results.
df_seq_sd %>% arrange(TIMECODE_RANGE, buoyid, sd_temp)

# Exclude duplicates in the same aggregate operation above.
df_seq_sd <- df_seq_grp %>% summarise(sd_temp = ts.sd(distinct(temperature)))

# Print the results.
df_seq_sd %>% arrange(TIMECODE_RANGE, buoyid, sd_temp)

# Calculate the sample standard deviation in the 'temperature' column of non-sequenced PTI table.
df_nonseq_sd <- df_nonseq_grp %>% summarise(sd_temp = ts.sd(temperature))

# Print the results.
df_nonseq_sd %>% arrange(TIMECODE_RANGE, sd_temp)

# Calculate the sample standard deviation in the 'temperature' column of non-PTI table.
df_nonpti_sd <- df_nonpti %>% group_by_time(timebucket.duration = "10m", timecode.column = "TIMECODE") %>% summarise(sd_temp = ts.sd(temperature))

# Print the results.
df_nonpti_sd %>% arrange(TIMECODE_RANGE, sd_temp)

## ----stddev_samp_not_runnable, eval=FALSE-------------------------------------
#  # Incorrect usage of ts.sd() with mutate and filter.
#  df_sd <- df_seq %>% mutate(sd_t = ts.sd(temperature))
#  df_sd <- df_seq %>% filter(temperature <= ts.sd(temperature))

## ----stddev_pop_runnable------------------------------------------------------
# Calculate the population standard deviation in the 'temperature' column of sequenced PTI table.
df_seq_sdp <- df_seq_grp %>% summarise(sdp_temp = ts.sdp(temperature))

# Print the results.
df_seq_sdp %>% arrange(TIMECODE_RANGE, buoyid, sdp_temp)

# Exclude duplicates in the same aggregate operation above.
df_seq_sdp <- df_seq_grp %>% summarise(sdp_temp = ts.sdp(distinct(temperature)))

# Print the results.
df_seq_sdp %>% arrange(TIMECODE_RANGE, buoyid, sdp_temp)

# Calculate the population standard deviation in the 'temperature' column of non-sequenced PTI table.
df_nonseq_sdp <- df_nonseq_grp %>% summarise(sdp_temp = ts.sdp(temperature))

# Print the results.
df_nonseq_sdp %>% arrange(TIMECODE_RANGE, sdp_temp)

# Calculate the population standard deviation in the 'temperature' column of non-PTI table.
df_nonpti_sdp <- df_nonpti %>% group_by_time(timebucket.duration = "10m", timecode.column = "TIMECODE") %>% summarise(sdp_temp = ts.sdp(temperature))

# Print the results.
df_nonpti_sdp %>% arrange(TIMECODE_RANGE, sdp_temp)

## ----stddev_pop_not_runnable, eval=FALSE--------------------------------------
#  # Incorrect usage of ts.sdp() with mutate and filter.
#  df_sdp <- df_seq %>% mutate(sdp_t = ts.sdp(temperature))
#  df_sdp <- df_seq %>% filter(temperature <= ts.sdp(temperature))

## ----min_runnable-------------------------------------------------------------
# Calculate the minimum value in the 'temperature' column of sequenced PTI table.
df_seq_min <- df_seq_grp %>% summarise(min_temp = ts.min(temperature))

# Print the results.
df_seq_min %>% arrange(TIMECODE_RANGE, buoyid, min_temp)

# Exclude duplicates in the same aggregate operation above.
df_seq_min <- df_seq_grp %>% summarise(min_temp = ts.min(distinct(temperature)))

# Print the results.
df_seq_min %>% arrange(TIMECODE_RANGE, buoyid, min_temp)

# Calculate the minimum value in the 'temperature' column of non-sequenced PTI table.
df_nonseq_min <- df_nonseq_grp %>% summarise(min_temp = ts.min(temperature))

# Print the results.
df_nonseq_min %>% arrange(TIMECODE_RANGE, min_temp)

# Calculate the minimum value in the 'temperature' column of non-PTI table.
df_nonpti_min <- df_nonpti %>% group_by_time(timebucket.duration = "10m", timecode.column = "TIMECODE") %>% summarise(min_temp = ts.min(temperature))

# Print the results.
df_nonpti_min %>% arrange(TIMECODE_RANGE, min_temp)

## ----min_not_runnable, eval=FALSE---------------------------------------------
#  # Incorrect usage of ts.min() with mutate and filter.
#  df_min <- df_seq %>% mutate(min_t = ts.min(temperature))
#  df_min <- df_seq %>% filter(temperature <= ts.min(temperature))

## ----max_runnable-------------------------------------------------------------
# Calculate the maximum value in the 'temperature' column of sequenced PTI table.
df_seq_max <- df_seq_grp %>% summarise(max_temp = ts.max(temperature))

# Print the results.
df_seq_max %>% arrange(TIMECODE_RANGE, buoyid, max_temp)

# Exclude duplicates in the same aggregate operation above.
df_seq_max <- df_seq_grp %>% summarise(max_temp = ts.max(distinct(temperature)))

# Print the results.
df_seq_max %>% arrange(TIMECODE_RANGE, buoyid, max_temp)

# Calculate the maximum value in the 'temperature' column of non-sequenced PTI table.
df_nonseq_max <- df_nonseq_grp %>% summarise(max_temp = ts.max(temperature))

# Print the results.
df_nonseq_max %>% arrange(TIMECODE_RANGE, max_temp)

# Calculate the maximum value in the 'temperature' column of non-PTI table.
df_nonpti_max <- df_nonpti %>% group_by_time(timebucket.duration = "10m", timecode.column = "TIMECODE") %>% summarise(max_temp = ts.max(temperature))

# Print the results.
df_nonpti_max %>% arrange(TIMECODE_RANGE, max_temp)

## ----max_not_runnable, eval=FALSE---------------------------------------------
#  # Incorrect usage of ts.max() with mutate and filter.
#  df_max <- df_seq %>% mutate(max_t = ts.max(temperature))
#  df_max <- df_seq %>% filter(temperature <= ts.max(temperature))

## ----var_samp_runnable--------------------------------------------------------
# Calculate the sample variance of values in the 'temperature' column of sequenced PTI table.
df_seq_var <- df_seq_grp %>% summarise(var_temp = ts.var(temperature))

# Print the results.
df_seq_var %>% arrange(TIMECODE_RANGE, buoyid, var_temp)

# Exclude duplicates in the same aggregate operation above.
df_seq_var <- df_seq_grp %>% summarise(var_temp = ts.var(distinct(temperature)))

# Print the results.
df_seq_var %>% arrange(TIMECODE_RANGE, buoyid, var_temp)

# Calculate the sample variance of values in the 'temperature' column of non-sequenced PTI table.
df_nonseq_var <- df_nonseq_grp %>% summarise(var_temp = ts.var(temperature))

# Print the results.
df_nonseq_var %>% arrange(TIMECODE_RANGE, var_temp)

# Calculate the sample variance of values in the 'temperature' column of non-PTI table.
df_nonpti_var <- df_nonpti %>% group_by_time(timebucket.duration = "10m", timecode.column = "TIMECODE") %>% summarise(var_temp = ts.var(temperature))

# Print the results.
df_nonpti_var %>% arrange(TIMECODE_RANGE, var_temp)

## ----var_samp_not_runnable, eval=FALSE----------------------------------------
#  # Incorrect usage of ts.var() with mutate and filter.
#  df_var <- df_seq %>% mutate(var_t = ts.var(temperature))
#  df_var <- df_seq %>% filter(temperature <= ts.var(temperature))

## ----var_pop_runnable---------------------------------------------------------
# Calculate the population variance of values in the 'temperature' column of sequenced PTI table.
df_seq_varp <- df_seq_grp %>% summarise(varp_temp = ts.varp(temperature))

# Print the results.
df_seq_varp %>% arrange(TIMECODE_RANGE, buoyid, varp_temp)

# Exclude duplicates in the same aggregate operation above.
df_seq_varp <- df_seq_grp %>% summarise(varp_temp = ts.varp(distinct(temperature)))

# Print the results.
df_seq_varp %>% arrange(TIMECODE_RANGE, buoyid, varp_temp)

# Calculate the population variance of values in the 'temperature' column of non-sequenced PTI table.
df_nonseq_varp <- df_nonseq_grp %>% summarise(varp_temp = ts.varp(temperature))

# Print the results.
df_nonseq_varp %>% arrange(TIMECODE_RANGE, varp_temp)

# Calculate the population variance of values in the 'temperature' column of non-PTI table.
df_nonpti_varp <- df_nonpti %>% group_by_time(timebucket.duration = "10m", timecode.column = "TIMECODE") %>% summarise(varp_temp = ts.varp(temperature))

# Print the results.
df_nonpti_varp %>% arrange(TIMECODE_RANGE, varp_temp)

## ----var_pop_not_runnable, eval=FALSE-----------------------------------------
#  # Incorrect usage of ts.varp() with mutate and filter.
#  df_varp <- df_seq %>% mutate(varp_t = ts.varp(temperature))
#  df_varp <- df_seq %>% filter(temperature <= ts.varp(temperature))

## ----avg_runnable-------------------------------------------------------------
# Calculate the average value in the 'temperature' column of sequenced PTI table.
df_seq_avg <- df_seq_grp %>% summarise(avg_temp = ts.mean(temperature))

# Print the results.
df_seq_avg %>% arrange(TIMECODE_RANGE, buoyid, avg_temp)

# Exclude duplicates in the same aggregate operation above.
df_seq_avg <- df_seq_grp %>% summarise(avg_temp = ts.mean(distinct(temperature)))

# Print the results.
df_seq_avg %>% arrange(TIMECODE_RANGE, buoyid, avg_temp)

# Calculate the average value in the 'temperature' column of non-sequenced PTI table.
df_nonseq_avg <- df_nonseq_grp %>% summarise(avg_temp = ts.mean(temperature))

# Print the results.
df_nonseq_avg %>% arrange(TIMECODE_RANGE, avg_temp)

# Calculate the average value in the 'temperature' column of non-PTI table.
df_nonpti_avg <- df_nonpti %>% group_by_time(timebucket.duration = "10m", timecode.column = "TIMECODE")  %>% summarise(avg_temp = ts.mean(temperature))

# Print the results.
df_nonpti_avg %>% arrange(TIMECODE_RANGE, avg_temp)

## ----avg_not_runnable, eval=FALSE---------------------------------------------
#  # Incorrect usage of ts.mean() with mutate and filter.
#  df_avg <- df_seq %>% mutate(avg_t = ts.mean(temperature))
#  df_avg <- df_seq %>% filter(temperature <= ts.mean(temperature))

## ----count_runnable-----------------------------------------------------------
# Calculate the number of rows in the 'temperature' column of sequenced PTI table.
df_seq_count <- df_seq_grp %>% summarise(count_temp = ts.n(temperature))

# Print the results.
df_seq_count %>% arrange(TIMECODE_RANGE, buoyid, count_temp)

# Exclude duplicates in the same aggregate operation above.
df_seq_count <- df_seq_grp %>% summarise(count_temp = ts.n(distinct(temperature)))

# Print the results.
df_seq_count %>% arrange(TIMECODE_RANGE, buoyid, count_temp)

# Calculate the number of rows in the sequenced PTI table. 
# Note that the argument is not provided for the function `ts.n()`.
df_seq_count <- df_seq_grp %>% summarise(count_temp = ts.n())

# Print the results.
df_seq_count %>% arrange(TIMECODE_RANGE, buoyid, count_temp)


# Calculate the number of rows in the 'temperature' column of non-sequenced PTI table.
df_nonseq_count <- df_nonseq_grp %>% summarise(count_temp = ts.n(temperature))

# Print the results.
df_nonseq_count %>% arrange(TIMECODE_RANGE, count_temp)

# Calculate the number of rows in the 'temperature' column of non-PTI table.
df_nonpti_count <- df_nonpti %>% group_by_time(timebucket.duration = "10m", timecode.column = "TIMECODE") %>% summarise(count_temp = ts.n(temperature))

# Print the results.
df_nonpti_count %>% arrange(TIMECODE_RANGE, count_temp)

## ----count_not_runnable, eval=FALSE-------------------------------------------
#  # Incorrect usage of ts.n() with mutate and filter.
#  df_count <- df_seq %>% mutate(count_t = ts.n(temperature))
#  df_count <- df_seq %>% filter(temperature <= ts.n(temperature))

## ---- echo = FALSE, results='asis'--------------------------------------------
without_distinct <- c("Maximum(column_name)", "Miniumum(column_name)",
                      "Average(column_name)", "STDDEV_SAMP(column_name)",
                      "MEDIAN(column_name)", "MODE(column_name)",
                      "PERCENTILE(column_name, 25, LINEAR))",
                      "PERCENTILE(column_name, 50, LINEAR))",
                      "PERCENTILE(column_name, 75, LINEAR))")
with_distinct <- c("Maximum(Distinct(column_name))", "Minimum(Distinct(column_name))",
                   "Average(Distinct(column_name)", "STDDEV_SAMP(Distinct(column_name))",
                   "MEDIAN(Distinct(column_name))", "MODE(column_name)",
                   "PERCENTILE(Distinct(column_name, 25, LINEAR))",
                   "PERCENTILE(Distinct(column_name, 50, LINEAR))",
                   "PERCENTILE(Distinct(column_name, 75, LINEAR))")

resultant_cols <- data.frame(without_distinct, with_distinct)
colnames(resultant_cols) <- c('Without distinct()', 'With distinct()')

kable(resultant_cols, caption = 'Resultant column names for ts.describe()')

## ----describe_runnable--------------------------------------------------------
# Calculate the statistics of the 'temperature' column of sequenced PTI table.
df_seq_describe <- df_seq_grp %>% summarise(describe_temp = ts.describe(temperature))

# Print the results.
df_seq_describe %>% arrange(TIMECODE_RANGE, buoyid, `MODE(temperature)`)

# Calculate the statistics of the 'temperature' column of non-sequenced PTI table.
df_nonseq_describe <- df_nonseq_grp %>% summarise(ts.describe(temperature))

# Print the results.
df_nonseq_describe

# Calculate the statistics of only distinct values of the 'temperature' column of non-PTI table.
df_nonpti_describe <- df_nonpti %>% group_by_time(timebucket.duration = "10m", timecode.column = "TIMECODE") %>% summarise(describe_temp = ts.describe(distinct(temperature)))

# Print the results.
df_nonpti_describe %>% arrange(TIMECODE_RANGE, `Mode(temperature)`)

## ----describe_accessing-------------------------------------------------------
# Select some columns from the describe operation and perform filter on one of the selected columns.
df_sel <- df_nonpti_describe %>% select(`GROUP BY TIME(MINUTES(10))`, 
                                        `PERCENTILE(Distinct(temperature, 75, LINEAR))`, 
                                        `Average(Distinct(temperature))`,
                                        `MODE(temperature)`
                                        )
df_filter <- df_sel %>% filter(`PERCENTILE(Distinct(temperature, 75, LINEAR))` < 77)

# Print the results.
df_filter %>% arrange(`GROUP BY TIME(MINUTES(10))`, `MODE(temperature)`)

