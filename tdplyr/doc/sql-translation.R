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

try(db_drop_table(con, 'landmass'), silent = TRUE)

landmass <- row.names(as.data.frame(islands))
DBI::dbWriteTable(con, 'landmass', as.data.frame(islands), row.names = TRUE)

## -----------------------------------------------------------------------------
df <- tbl(con, 'landmass')
translate_sql(my_udf(col1, col2) + 1L)
df %>% mutate(udf = my_udf(row_names) + 1L) %>% show_query()

## ---- warning=FALSE-----------------------------------------------------------
translate_sql(mean(x), MEAN(y), con = dbplyr::simulate_teradata())

df %>% 
  mutate(x = mean(islands), y = MEAN(islands)) %>%
  show_query()

## -----------------------------------------------------------------------------
translate_sql(avg(x), AVG(x), aVg(x), con = dbplyr::simulate_teradata())
df %>% 
  group_by(row_names) %>% 
  summarize(x = avg(islands), y = AVG(islands), z = aVg(islands)) %>%
  show_query()

## ---- echo = FALSE, results='asis'--------------------------------------------
ops <- c('+', '-', '/', '*', '%%', '%/%', '^')
exps <- c('x + y', 'x - y', 'x / y', 'x * y', 'x %% y', 'x %/% y', 'x ^ y')
sqls <- c(translate_sql(x + y, con = con), 
          translate_sql(x - y, con = con), 
          translate_sql(x / y, con = con), 
          translate_sql(x * y, con = con),
          translate_sql(x %% y, con = con),
          translate_sql(x %/% y, con = con),
          translate_sql(x ^ y, con = con)
          )

math_ops <- data.frame(ops, exps, sqls)
colnames(math_ops) <- c('R operators', 'R expressions', 'SQL')

kable(math_ops, caption = 'Math Operators')

ops <- c('abs', 
         'acos', 
         'acosh', 
         'asin', 
         'asinh', 
         'atan', 
         'atan2',
         'atanh', 
         'ceil',
         'cos',
         'cosh',
         'cot',
         'coth',
         'degrees',
         'exp',
         'floor',
         'ln',
         'log',
         'log10',
         'radians',
         'round',
         'sign',
         'sin',
         'sinh',
         'sqrt',
         'tan')

exps <- c('abs(x)', 
         'acos(x)', 
         'acosh(x)', 
         'asin(x)', 
         'asinh(x)', 
         'atan(x)', 
         'atan2(y, x)',
         'atanh(x)', 
         'ceil(x)',
         'cos(x)',
         'cosh(x)',
         'cot(x)',
         'coth(x)',
         'degrees(x)',
         'exp(x)',
         'floor(x)',
         'ln(x)',
         'log(x, base = exp(1))',
         'log10(x)',
         'radians(x)',
         'round(x, digits = 0L)',
         'sign(x)',
         'sin(x)',
         'sinh(x)',
         'sqrt(x)',
         'tan(x)')

sqls <- c(translate_sql(abs(x), con = con), 
          translate_sql(acos(x), con = con), 
          translate_sql(acosh(x), con = con), 
          translate_sql(asin(x), con = con),
          translate_sql(asinh(x), con = con),
          translate_sql(atan(x), con = con),
          translate_sql(atan2(y, x), con = con),
          translate_sql(atanh(x), con = con),
          translate_sql(ceil(x), con = con),
          translate_sql(cos(x), con = con),
          translate_sql(cosh(x), con = con),
          translate_sql(cot(x), con = con),
          translate_sql(coth(x), con = con),
          translate_sql(degrees(x), con = con),
          translate_sql(exp(x), con = con),
          translate_sql(floor(x), con = con),
          translate_sql(ln(x), con = con),
          translate_sql(log(x, y), con = con),
          translate_sql(log10(x), con = con),
          translate_sql(radians(x), con = con),
          translate_sql(round(x), con = con),
          translate_sql(sign(x), con = con),
          translate_sql(sin(x), con = con),
          translate_sql(sinh(x), con = con),
          translate_sql(sqrt(x), con = con),
          translate_sql(tan(x), con = con))

math_fns <- data.frame(ops, exps, sqls)
colnames(math_fns) <- c('R operators', 'R expressions', 'SQL')

kable(math_fns, caption = 'Math Functions')

## ---- echo = FALSE, results='asis'--------------------------------------------
ops <- c('!', '&', '&&', '|', '||', 'xor', 'if', 'if_else')
exps <- c('! x', 
          'x & y', 'x && y', 
          'x | y', 'x || y', 
          'xor(x, y)', 
          '`if`(cond, true, false = NULL)', 'if_else(cond, true, false)')
sqls <- c(translate_sql(! x, con = con), 
          translate_sql(x & y, con = con), 
          translate_sql(x && y, con = con), 
          translate_sql(x | y, con = con),
          translate_sql(x || y, con = con),
          translate_sql(xor(x,  y), con = con),
          translate_sql(`if`(cond,  true), con = con),
          translate_sql(if_else(cond,  true, false), con = con))
logical_ops <- data.frame(ops, exps, sqls)
colnames(logical_ops) <- c('R operators', 'R expressions', 'SQL')

kable(logical_ops, caption = 'Logical Operators')

## ---- echo = FALSE, results='asis'--------------------------------------------
ops <- c('<', '<=', '>', '>=', '==', '!=', 'is.null', 'is.na')
exps <- c('x < y', 'x <= y', 
          'x > y', 'x >= y', 'x == y', 'x != y', 
          'is.null(x)', 'is.na(x)')
sqls <- c(translate_sql(x < y, con = con), 
          translate_sql(x <= y, con = con), 
          translate_sql(x > y, con = con), 
          translate_sql(x >= y, con = con),
          translate_sql(x == y, con = con),
          translate_sql(x != y, con = con),
          translate_sql(is.null(x), con = con),
          translate_sql(is.na(x), con = con))
comparison_ops <- data.frame(ops, exps, sqls)
colnames(comparison_ops) <- c('R operators', 'R expressions', 'SQL')

kable(comparison_ops, caption = 'Comparison Operators')

## ---- echo = FALSE, results='asis'--------------------------------------------
ops <- c('as.integer', 'as.numeric', 'as.double', 'as.character', 'as', 'as.Date')
exps <- c('as.integer(x)', 
          'as.numeric(x, precision = 5L, scale = 0L)', 
          'as.double(x)',
          'as.character(x, n = NULL, charset = NULL)',
          'as(x, \'y\')',
          'as.Date(x, format)')
sqls <- c(translate_sql(as.integer(x), con = con), 
          translate_sql(as.numeric(x), con = con), 
          translate_sql(as.double(x), con = con),
          translate_sql(as.character(x), con = con), 
          translate_sql(as(x, 'y'), con = con),
          translate_sql(as.Date(x, format="YYYY-MM-DD"), con = con))

cast <- data.frame(ops, exps, sqls)
colnames(cast) <- c('R operators', 'R expressions', 'SQL')

kable(cast, caption = 'Conversion functions')

## ---- echo = FALSE, results='asis', strip.white= FALSE------------------------
ops <- c('tolower', 'toupper', 'nchar',
         'str_length', 'substr', 'substring',
         'grep', 'str_extract', 'sub', 'gsub',
         'str_replace', 'paste', 'paste0', 'str_join', 
         'strsplit', 'str_locate', 'str_trim','str_replace_all',
         'str_detect')

exps <- c('tolower(x)', 'toupper(x)', 'nchar(x)',
          'str_length(x)', 'substr(x, start , stop = NULL)',
          'substring(x, start, stop = NULL)', 
          'grep(pattern, x, ignore.case = FALSE)',
          'str_extract(string, pattern)', 
          'sub(pattern, replacement, x, ignore.case = FALSE)', 
          'gsub(pattern, replacement, x, ignore.case = FALSE)', 
          'str_replace(string, pattern, replacement)', 
          'paste(..., sep = \'\\  \')', 
          'paste0(...)', 
          'str_join(..., sep = \'\\   \')',
          'strsplit(x, split = \'\\   \', tokennum = 1L)',
          'str_locate(string, pattern, return_opt = 0L)',
          'str_trim(string, side = c("both", "left", "right"))',
          'str_replace_all(string, pattern, replacement)',
          'str_detect(string, pattern)')
          
markdown_whitespace <- '\\\\n \\\\t\\\\r\\\\v\\\\f'
sqls <- c(translate_sql(tolower(x), con = con), 
          translate_sql(toupper(x), con = con), 
          translate_sql(nchar(x), con = con),
          translate_sql(str_length(x), con = con),
          translate_sql(substr(x, start, stop = NULL), con = con),
          translate_sql(substring(x, start, stop = NULL), con = con),
          translate_sql(grep(pattern, x, ignore.case = FALSE), con = con),
          translate_sql(str_extract(string, pattern), con = con),
          translate_sql(sub(pattern, replacement, x, ignore.case = FALSE), con = con),
          translate_sql(gsub(pattern, replacement, x, ignore.case = FALSE), con = con),
          translate_sql(str_replace(string, pattern, replacement), con = con),
          translate_sql(paste(c1, c2, c3, sep = "\\ "), con = con),
          translate_sql(paste0(c1, c2, c3), con = con),
          translate_sql(str_join(c1, c2, c3, sep = "\\ "), con = con),
          translate_sql(strsplit(x, split = '\\ ', tokennum = 1L), con = con),
          translate_sql(str_locate(string, pattern, return_opt = 0L), con = con),
          # kable is not showing the whitespace string in the output when str_trim is translated
          translate_sql(RTRIM(LTRIM(string, !!! markdown_whitespace), !!! markdown_whitespace)),
          # "manually" add the str_trim above as a workaround
          translate_sql(str_replace_all(string, pattern, replacement), con = con),
          translate_sql(str_detect(string, pattern), con = con)
          )

char_ops <- data.frame(ops, exps, sqls)
colnames(char_ops) <- c('R operators', 'R expressions', 'SQL')
kable(char_ops, caption = 'Character Functions', escape = TRUE)

## -----------------------------------------------------------------------------
head(landmass)

# base R grep
grep('^A', landmass, value = TRUE) %>% sort

# sql translated grep
df %>% transmute(x = grep('^A', row_names)) %>% pull(x) %>% sort

# to specify the full string, specify it in the pattern
df %>% transmute(x = grep('^A.+', row_names)) %>% pull(x) %>% sort

# or use filter (Using arrange to order the output by the column 'row_names')
df %>% select(row_names) %>% filter(!is.na(grep('^A', row_names))) %>% arrange(row_names)

## ---- echo = FALSE, results='asis'--------------------------------------------
ops <- c('mean',
         'max',
         'min',
         'sum',
         'var',
         'varp',
         'sd',
         'sdp',
         'n', 
         'n_distinct',
         'median')

exps <- c('mean(x, na.rm = FALSE)',
          'max(x, na.rm = FALSE)',
          'min(x, na.rm = FALSE)',
          'sum(x, na.rm = FALSE)',
          'var(x, na.rm = FALSE)',
          'varp(x, na.rm = FALSE)',
          'sd(x, na.rm = FALSE)',
          'sdp(x, na.rm = FALSE)',
          'n(x)',
          'n_distinct(x)',
          'median(x)')

sqls <- c(translate_sql(mean(x, na.rm = TRUE), con = con, window = FALSE),
          translate_sql(max(x, na.rm = TRUE), con = con, window = FALSE), 
          translate_sql(min(x, na.rm = TRUE), con = con, window = FALSE), 
          translate_sql(sum(x, na.rm = TRUE), con = con, window = FALSE),
          translate_sql(var(x, na.rm = TRUE), con = con, window = FALSE),
          translate_sql(varp(x, na.rm = TRUE), con = con, window = FALSE),
          translate_sql(sd(x, na.rm = TRUE), con = con, window = FALSE),
          translate_sql(sdp(x, na.rm = TRUE), con = con, window = FALSE),
          translate_sql(n(x), con = con, window = FALSE),
          translate_sql(n_distinct(x), con = con, window = FALSE),
          translate_sql(median(x), con = con, window = FALSE))

ops <- data.frame(ops, exps, sqls)
colnames(ops) <- c('R operators', 'R expressions', 'SQL')

kable(ops, caption = 'Aggregate functions')

## ---- echo = FALSE, results='asis'--------------------------------------------
ops <- c('row_number',
         'rank',
         'dense_rank',
         'percent_rank',
         'cume_dist',
         'first',
         'last', 
         'lag', 
         'lead',
         'ntile')

exps <- c('row_number(x)',
          'rank(x)',
          'dense_rank(x)',
          'percent_rank(x)',
          'cume_dist(x)',
          'first(x, order_by = NULL, ignore_nulls = FALSE)',
          'last(x, order_by = NULL, ignore_nulls = FALSE)',
          'lag(x, n = 1L, default = NA, order_by = NULL, ignore_nulls = FALSE)',
          'lead(x, n = 1L, default = NA, order_by = NULL, ignore_nulls = FALSE)',
          'ntile(100, order_by = NULL)')

sqls <- c(translate_sql(row_number(x), con = con), 
          translate_sql(rank(x), con = con), 
          translate_sql(dense_rank(x), con = con), 
          translate_sql(percent_rank(x), con = con),
          translate_sql(cume_dist(x), con = con),
          translate_sql(first(x, order_by = x, ignore_nulls = FALSE), con = con),
          translate_sql(last(x, order_by = x, ignore_nulls = FALSE), con = con),
          translate_sql(lag(x, n = 1L, 
                            default = NA, order_by = x, ignore_nulls = FALSE), con = con),
          translate_sql(lead(x, n = 1L, 
                            default = NA, order_by = x, ignore_nulls = FALSE), con = con),
          translate_sql(ntile(100, order_by = x), con = con)
          )

ops <- data.frame(ops, exps, sqls)
colnames(ops) <- c('R operators', 'R expressions', 'SQL')

kable(ops, caption = 'Window functions')

## ---- echo = FALSE, results='asis', warning=FALSE-----------------------------
ops <- c('mean', 
         'sum', 
         'min', 
         'max', 
         'n',
         'var',
         'varp',
         'sd',
         'sdp',
         'cumsum',
         'cummin',
         'cummax',
         'cummean')

exps <- c('mean(x, na.rm = FALSE)',
          'sum(x, na.rm = FALSE)',
          'min(x, na.rm = FALSE)',
          'max(x, na.rm = FALSE)',
          'n(x)',
          'var(x, na.rm = FALSE)',
          'varp(x, na.rm = FALSE)',
          'sd(x, na.rm = FALSE)',
          'sdp(x, na.rm = FALSE)',
          'cumsum(x)',
          'cummin(x)',
          'cummax(x)',
          'cummean(x)')

sqls <- c(translate_sql(mean(x, na.rm = TRUE), con = con), 
          translate_sql(sum(x, na.rm = TRUE), con = con), 
          translate_sql(min(x, na.rm = TRUE), con = con), 
          translate_sql(max(x, na.rm = TRUE), con = con),
          translate_sql(n(x), con = con),
          translate_sql(var(x, na.rm = TRUE), con = con),
          translate_sql(varp(x, na.rm = TRUE), con = con),
          translate_sql(sd(x, na.rm = TRUE), con = con),
          translate_sql(sdp(x, na.rm = TRUE), con = con),
          translate_sql(cumsum(x), con = con),
          translate_sql(cummin(x), con = con),
          translate_sql(cummax(x), con = con),
          translate_sql(cummean(x), con = con)
          )

ops <- data.frame(ops, exps, sqls)
colnames(ops) <- c('R operators', 'R expressions', 'SQL')
kable(ops, caption = 'Window Aggregate functions')

## ---- echo = TRUE, eval = FALSE-----------------------------------------------
#  # equivalent ways of specifying the order by clause in the window specification
#  tbl %>% mutate(order_by(x, first(x)))
#  
#  tbl %>% mutate(first(x, order_by = x))
#  
#  tbl %>% window_order(x) %>% mutate(first(x))

## ---- echo = FALSE, results='asis'--------------------------------------------

ops <- c('bitwNot', 'bitwAnd', 'bitwOr', 
         'bitwXor', 'bitwShiftL', 'bitwShiftR',
         'bitnot' ,'bitand', 'bitor', 
         'bitxor', 'shiftleft', 'shiftright')

exps <- c('bitwNot(a)',
          'bitwAnd(a, b)',
          'bitwOr(a, b)',
          'bitwXor(a, b)',
          'bitwShiftL(a, n)',
          'bitwShiftR(a, n)',
          'bitnot(a)',
          'bitand(a, b)',
          'bitor(a, b)',
          'bitxor(a, b)',
          'shiftleft(a, n)',
          'shiftright(a, n)')

sqls <- c(translate_sql(bitwNot(a), con = con), 
          translate_sql(bitwAnd(a, b), con = con), 
          translate_sql(bitwOr(a, b), con = con),
          translate_sql(bitwXor(a, b), con = con), 
          translate_sql(bitwShiftL(a, n), con = con),
          translate_sql(bitwShiftR(a, n), con = con),
          translate_sql(bitnot(a), con = con), 
          translate_sql(bitand(a, b), con = con), 
          translate_sql(bitor(a, b), con = con),
          translate_sql(bitxor(a, b), con = con), 
          translate_sql(shiftleft(a, n), con = con),
          translate_sql(shiftright(a, n), con = con))

cast <- data.frame(ops, exps, sqls)
colnames(cast) <- c('R operators', 'R expressions', 'SQL')

kable(cast, caption = 'Bit functions')

## ---- echo=FALSE, include=FALSE-----------------------------------------------
db_drop_table(con, 'landmass')
td_remove_context()

