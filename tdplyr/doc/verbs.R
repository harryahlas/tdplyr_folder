## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
options(tibble.print_min = 6L, tibble.print_max = 6L, digits = 3)

## ---- echo=FALSE, include=FALSE-----------------------------------------------
library(dplyr)
library(DBI)
library(odbc)
library(tdplyr)

# Reading Vantage cluster details from td_config.cfg
config_file <- system.file("extdata", "td_config.cfg", package = "tdplyr")
config_df <- read.csv(config_file, sep = ":", stringsAsFactors = FALSE, header = TRUE)
config_list <- unlist(lapply(config_df$value, function(x) {trimws(x)}))

con <- td_create_context(host = config_list[1], uid = config_list[2], pwd = config_list[3], dType = "native")

try(db_drop_table(con, 'mtcars'), silent = TRUE)
try(db_drop_table(con, 'table1'), silent = TRUE)
try(db_drop_table(con, 'CO2'), silent = TRUE)

DBI::dbWriteTable(con, 'mtcars', mtcars, row.names = TRUE)
DBI::dbWriteTable(con, 'CO2', as.data.frame(CO2), row.names = TRUE)

temp <- data.frame(idx = 1:200,val = c(1:100, rep(NA_real_, 100)))
DBI::dbWriteTable(con, 'table1', temp, row.names = TRUE)

opts <- list(db = FALSE, machine = FALSE, driver = FALSE)
options(td.db_desc.output = opts)

## -----------------------------------------------------------------------------
t <- tbl(con, 'mtcars')
t %>% select(row_names, mpg) %>% arrange(row_names)

## ---- eval = FALSE------------------------------------------------------------
#  t %>% select('row_names', 'mpg') %>% arrange(row_names) # as strings
#  t %>% select(c('row_names', 'mpg')) %>% arrange(row_names) # as a collection
#  t %>% select(1, 2) %>% arrange(row_names)          # by column position (note that indexing starts at 1)
#  t %>% select(row_names:mpg) %>% arrange(row_names) # as a range of columns
#  t %>% select(.data$row_names, .data$mpg) %>% arrange(row_names) # using the .data pronoun
#  t %>% select(-(3:12)) %>% arrange(row_names) # all columns except 3 thru 12
#  t %>% select(-(cyl:carb)) %>% arrange(row_names) # all columns except cyl thru carb

## -----------------------------------------------------------------------------
results <- t %>% rename(column_1 = row_names, miles_per_gallon = mpg)
show_query(results)

## -----------------------------------------------------------------------------
new_col_names_vec <- c(a=1, b=2, c=3)
results <- t %>% select(new_col_names_vec)
show_query(results)

## -----------------------------------------------------------------------------
t %>% select(starts_with('c')) %>% arrange(row_names)
t %>% select(-starts_with('c')) %>% arrange(row_names)

## -----------------------------------------------------------------------------
t %>% select(starts_with('c'), ends_with('s')) %>% arrange(row_names)

## -----------------------------------------------------------------------------
t %>% select_all(toupper) %>% arrange(row_names)

## -----------------------------------------------------------------------------
my_renaming_fn <- function(col){
    first <- substr(col, 1, 2)
    second <- substr(col, 3, nchar(col))
    paste0(toupper(first), second)
}
t %>% select_all(my_renaming_fn) %>% arrange(ROw_names)

## -----------------------------------------------------------------------------
my_letters <- function(x){
    idx <- as.integer(substr(x, 2, nchar(x)))
    letters[idx]
}
t %>% select(v = everything()) %>% select_all(my_letters) %>% arrange(a)

## -----------------------------------------------------------------------------
t %>% select(v = everything())  %>% arrange(v1)

## -----------------------------------------------------------------------------
is_whole <- function(x) all(floor(x) == x)
is_integerish <- function(x) is.numeric(x) && is_whole(x)
t %>% rename_if(is_integerish, toupper)  %>% arrange(row_names)

## -----------------------------------------------------------------------------
t %>% select_if(is_integerish, toupper) %>% arrange(HP, CYL, VS, AM, GEAR, CARB)

## -----------------------------------------------------------------------------
t %>% filter(row_names %in% c('Valiant', 'Merc 280C')) %>% arrange(row_names)

## -----------------------------------------------------------------------------
t %>% filter(row_names %in% c('Valiant', 'Merc 280C')) %>% show_query()

## -----------------------------------------------------------------------------
# basic expressions

# & AND
show_query(t %>% filter(mpg > 35 & hp > 90))

# | OR
show_query(t %>% filter(cyl < 6 | wt < 2.5 ))

# != <>
show_query(t %>% filter(vs != am))

# multiple expressions
res <- t %>% filter(mpg > 25, hp > 90)
show_query(res)

## -----------------------------------------------------------------------------
# get rows with strings representing col names
show_query(t %>% filter('vs' == 'am'))
tally(t %>% filter('vs' == 'am'))

# get rows with integers representing col positions
show_query(t %>% filter(9L == 10L))
tally(t %>% filter(9L == 10L))

# now tally the actual columns
tally(t %>% filter(vs == am))

## -----------------------------------------------------------------------------
# use a non-mapped function
t %>% filter(my_function(mpg, cyl, hp, gear) > 0L) %>% show_query()

plus_one <- function(x){ x + 1 }
t %>% filter(plus_one(4) < cyl) %>% show_query()

## -----------------------------------------------------------------------------
t %>% filter(mpg > 35) %>% show_query
# vs.
t %>% filter(mpg > 35L) %>% show_query

## -----------------------------------------------------------------------------
# filter based on columns of chr type
res <- t %>% filter_if(is.character, all_vars(. %in% c('Merc 230', 'Mazda RX4', 'Porsche 914-2')))

## -----------------------------------------------------------------------------
show_query(res)

## -----------------------------------------------------------------------------
# any_vars
t %>% filter_if(is.numeric, any_vars(floor(.) == ceil(.))) %>% show_query()

# all_vars
t %>% filter_if(is.numeric, all_vars(floor(.) == ceil(.))) %>% show_query()

## ---- error= TRUE-------------------------------------------------------------
# no any_vars() or all_vars() used
t %>% filter_if(is.character, . %in% c('Merc 230', 'Mazda RX4', 'Porsche 914-2'))

## -----------------------------------------------------------------------------
# predicate applied to columns ending with p
res <- t %>% filter_at(vars(ends_with('p')), any_vars(floor(.) > 30L))
show_query(res)

## -----------------------------------------------------------------------------
# AND/OR predicates over all columns
t %>% select(-row_names) %>% filter_all(any_vars(ceil(.) == .)) %>% show_query()
t %>% select(-row_names) %>% filter_all(all_vars(floor(.) == .)) %>% show_query()

## -----------------------------------------------------------------------------
res <- t %>% select(row_names:hp) %>% mutate(mpg2 = mpg / hp, disp / cyl)
res %>% arrange(row_names)

## -----------------------------------------------------------------------------
res <- t %>% transmute(mpg2 = mpg / hp, disp / cyl)
res  %>% arrange(mpg2, `disp/cyl`)

## -----------------------------------------------------------------------------
res <- t %>% select(mpg:hp) %>% 
       mutate(ranking = case_when(
          mpg > 30 ~ 'high',
          mpg <= 30 & mpg > 20 ~ 'medium',
          mpg <= 20 ~ 'low'
))
res %>% arrange(mpg, cyl, disp, hp, ranking)

## -----------------------------------------------------------------------------
res %>% show_query

## -----------------------------------------------------------------------------
res <- t %>% select(row_names, mpg, hp) %>% mutate_if(is.numeric, funs(power, log), 2L)
res %>% arrange(row_names)

## -----------------------------------------------------------------------------
res %>% show_query

## -----------------------------------------------------------------------------
res <- t %>% select(row_names, mpg, hp) %>% transmute_if(is.numeric, funs(power, log), 2L)
res %>% arrange(mpg_power, hp_power, mpg_log, hp_log)

## -----------------------------------------------------------------------------
# table with some NULL values
t1 <- tbl(con, 'table1')
t1 %>% arrange(row_names)

has_na <- function(x) any(is.na(x))
res <- t1 %>% mutate_if(has_na, funs(if_else(is.na(.), 0L, floor(.))))

res %>% arrange(row_names)

## -----------------------------------------------------------------------------
res %>% show_query

## -----------------------------------------------------------------------------
res <- t %>% mutate_at(vars(ends_with('p')), funs(if_else(. / 2 <= 100L, power(., 3L), log(., 2L))))
res %>% arrange(row_names)

res %>% show_query

## -----------------------------------------------------------------------------
res <- t %>% mutate_at(vars(ends_with('p')), funs(power(., 3L), log(., 2L)))
colnames(res) 

res %>% show_query

## -----------------------------------------------------------------------------
t %>% select(mpg, hp) %>% mutate_all(funs(. * 2, . / 2)) %>% arrange(mpg, hp)

t %>% select(mpg, hp) %>% transmute_all(funs(. * 2, . / 2)) %>% arrange(`mpg_*`, `hp_*`)

## -----------------------------------------------------------------------------
t %>% select(mpg, hp) %>% mutate_all(funs(. / 2, . / 3)) %>% arrange(mpg, hp)

## -----------------------------------------------------------------------------
t %>% select(mpg, hp) %>% mutate_all(funs(a = . / 2, b = . / 3)) %>% arrange(mpg, hp)

## -----------------------------------------------------------------------------
t %>% arrange(row_names)

## -----------------------------------------------------------------------------
t %>% arrange(desc(row_names))

## -----------------------------------------------------------------------------
res <- t %>% select(cyl, mpg) %>% arrange(cyl, desc(mpg))
res

res %>% show_query

## -----------------------------------------------------------------------------
res <- t %>% arrange(1L, desc(2L))
res

res %>% show_query

## -----------------------------------------------------------------------------
t %>% arrange(1)

t %>% arrange(1) %>% show_query

## -----------------------------------------------------------------------------
t %>% arrange('row_names')

t %>% arrange('row_names') %>% show_query

## ---- error=TRUE--------------------------------------------------------------
# arrange before filter
t %>% select(row_names, mpg) %>% arrange(mpg, row_names) %>% filter(mpg > 20)

## -----------------------------------------------------------------------------
# arrange last in pipeline
t %>% select(row_names, mpg) %>% filter(mpg > 20) %>% arrange(mpg, row_names)

## ---- error=TRUE--------------------------------------------------------------
t %>% arrange(mpg) %>% pull(mpg)

## -----------------------------------------------------------------------------
df <- t %>% select(mpg) %>% arrange(mpg) %>% as.data.frame
head(df)

## -----------------------------------------------------------------------------
res <- t %>% arrange_if(is.numeric)
res %>% show_query

res

## -----------------------------------------------------------------------------
res <- t %>% 
      select(my_mpg = mpg, my_cyl = cyl, hp) %>% 
      arrange_if(is.numeric)
res %>% show_query

res

## -----------------------------------------------------------------------------
c2 <- tbl(con, 'CO2')
c2 %>% arrange(row_names)

## -----------------------------------------------------------------------------
res <- c2 %>% 
  group_by(Plant) %>% 
  summarize(avg_uptake = avg(uptake)) %>% 
  arrange_if(is.numeric)
res %>% show_query

res

## -----------------------------------------------------------------------------
res <- c2 %>% arrange_at(vars(contains('t')))
res %>% show_query

res

## -----------------------------------------------------------------------------
res <- c2 %>% 
  group_by(type, Plant) %>% 
  summarize(avg(uptake)) %>% 
  arrange_all()

res %>% show_query

res

## -----------------------------------------------------------------------------
t %>% group_by(hp, disp) %>% show_query

## -----------------------------------------------------------------------------
t %>% group_by(hp, disp) %>% group_vars()

## ---- warning = FALSE---------------------------------------------------------
res <- t %>% 
  group_by(hp, disp) %>% 
  summarize(mean(hp), mean(disp), count = n())

res %>% show_query()

## -----------------------------------------------------------------------------
res <- t %>% 
       select(row_names, mpg:am) %>%
       group_by(sizes = case_when(
                   hp > 2 * mean(hp, na.rm = TRUE)  ~ "large",
                   hp <= 2 * mean(hp, na.rm = TRUE) & hp >= mean(hp, na.rm = TRUE) ~ "medium",
                   hp < mean(hp, na.rm = TRUE) ~ "small"))
res %>% arrange(row_names)

## -----------------------------------------------------------------------------
res %>% group_vars()

## -----------------------------------------------------------------------------
res %>% show_query()

## -----------------------------------------------------------------------------
summary <- res %>% summarize(mean = mean(hp, na.rm = TRUE), 
                             sd = sd(hp, na.rm = TRUE), 
                             min = min(hp, na.rm = TRUE), 
                             max = max(hp, na.rm = TRUE), 
                             count = n())
summary %>% show_query()

summary %>% arrange(sizes)

## -----------------------------------------------------------------------------
t %>% 
  summarize(hp_mean = mean(hp, na.rm = TRUE), 
                hp_sd = sd(hp, na.rm =TRUE), 
                hp_max = max(hp, na.rm = TRUE), 
                hp_min = min(hp, na.rm = TRUE), 
                hp_n = n()) %>% 
  show_query

## -----------------------------------------------------------------------------
t %>% summarize(hp_mean = mean(hp, na.rm = TRUE), 
                hp_sd = sd(hp, na.rm = TRUE), 
                hp_max = max(hp, na.rm = TRUE), 
                hp_min = min(hp, na.rm = TRUE), 
                hp_n = n())

## -----------------------------------------------------------------------------
multi <- res %>% group_by(sizes, cyl)
multi %>% group_vars()

## -----------------------------------------------------------------------------
res %>%
  group_by(sizes) %>%
  group_by(cyl) %>%
  group_vars()

multi %>%
  group_by(vs) %>%
  group_vars()

## -----------------------------------------------------------------------------
unnest <- multi %>% summarize(mean_mpg = mean(mpg, na.rm = TRUE))
unnest %>% arrange(sizes, cyl)

## -----------------------------------------------------------------------------
unnest %>% pull(mean_mpg)

## -----------------------------------------------------------------------------
# get the correct mean values
unnest %>% ungroup %>% pull(mean_mpg)

# summarize further by calling sum
ungrouped <- unnest %>% summarize(sum_of_mean_mp = sum(mean_mpg, na.rm = TRUE))
ungrouped %>% arrange(sizes)

## -----------------------------------------------------------------------------
res %>% mutate(mean = mean(hp, na.rm = TRUE), 
               sd = sd(hp, na.rm = TRUE), 
               min = min(hp, na.rm = TRUE), 
               max = max(hp, na.rm = TRUE), 
               count = n()) %>% 
  show_query()

res %>% mutate(mean = mean(hp, na.rm = TRUE), 
               sd = sd(hp, na.rm = TRUE), 
               min = min(hp, na.rm = TRUE), 
               max = max(hp, na.rm = TRUE), 
               count = n()) %>% arrange(row_names)

## -----------------------------------------------------------------------------
summary %>% show_query()
summary %>% arrange(sizes)

## -----------------------------------------------------------------------------
t %>% group_by_all() %>% group_vars()

## -----------------------------------------------------------------------------
t %>% group_by_at(vars(contains('t')), funs(as.integer)) %>% show_query

## -----------------------------------------------------------------------------
# no alias
t %>% group_by_at(vars(contains('t')), funs(as.integer)) %>% arrange(row_names)

#aliased
t %>% select(drat:am) %>% 
      group_by_at(vars(contains('t')), funs(as_int = as.integer)) %>% arrange(drat, wt, qsec)

## -----------------------------------------------------------------------------
t %>% group_by_if(is.numeric) %>% group_vars()

## -----------------------------------------------------------------------------
res %>% select(-row_names) %>% summarize_all(funs(mean, max, min), na.rm = TRUE) %>% arrange(sizes)

## -----------------------------------------------------------------------------
res %>% summarize_at(vars(ends_with('t')), funs(mean, max, min), na.rm = TRUE) %>% arrange(sizes)

## -----------------------------------------------------------------------------
is_whole <- function(col){ is.numeric(col) && all(floor(col) == col) }
res %>% summarize_if(is_whole, funs(mean, max, min), na.rm = TRUE) %>% arrange(sizes)

## -----------------------------------------------------------------------------
res <- t %>% select(mpg:disp) %>% 
             summarize_all(funs(
                    mean(., na.rm = TRUE), 
                    sd(., na.rm = TRUE),
                    max(., na.rm = TRUE), 
                    min(., na.rm = TRUE)))
res

## -----------------------------------------------------------------------------
means <- res %>% select(contains('mean'))
means

sds   <- res %>% select(dplyr::matches('sd$')) 
sds

maxs  <- res %>% select(ends_with('max'))
maxs

mins  <- res %>% select(ends_with('min'))
mins

## ---- echo=FALSE, include=FALSE-----------------------------------------------
db_drop_table(con, 'mtcars')
db_drop_table(con, 'table1')
db_drop_table(con, 'CO2')
td_remove_context()

