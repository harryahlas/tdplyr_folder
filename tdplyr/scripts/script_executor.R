#!/usr/bin/env Rscript

####################################################################
# Unpublished work.
# Copyright (c) 2021 by Teradata Corporation. All rights reserved.
# TERADATA CORPORATION CONFIDENTIAL AND TRADE SECRET
# 
# Primary Owner: gouri.patwardhan@teradata.com
# Secondary Owner: pavansaikumar.alladi@teradata.com
#
# Script Executor
# ----------
# Script executor executes the user's script in sandbox environment.
####################################################################

library(argparser, quietly = TRUE)
suppressPackageStartupMessages(library(teradatasql))

execute_user_script <- function(user_script_path, script_args, data_file_path) {
  stderr_file <- "stderr.txt"
  cmd_args <- paste(user_script_path, script_args)
  output <- system2("Rscript", args = cmd_args, stdin = data_file_path, stdout = TRUE, stderr = stderr_file)
  stderr <- readLines(stderr_file)
  status <- file.remove(stderr_file)
  if (length(stderr))
    stop(stderr)
  else {
    # Writing to console each element of the output of the command run.
    for (i in 1:length(output)) {
      cat(paste0(output[[i]], "\n"))
    }
  }
}

parser <- arg_parser("Script Executor executes the user's script in sandbox environment.")

parser <- add_argument(parser, "user_script_path", help = "Specifies the path to the user's script.", type = "character")
parser <- add_argument(parser, "--script-args", help = "Specifies the script's arguments required to execute the user's script.", type = "character", default = "")
parser <- add_argument(parser, "--data-file-path", help = "Specifies the path to the data file.", type = "character", default = "")
parser <- add_argument(parser, "--db-host", help = "Specifies the hostname or IP address of Teradata Vantage to establish a connection.", type = "character", default = "")
parser <- add_argument(parser, "--user", help = "Specifies the Vantage username to establish a connection.", type = "character", default = "")
parser <- add_argument(parser, "--passwd", help = "Specifies the password of the user to establish a connection.", type = "character", default = "")
parser <- add_argument(parser, "--table", help = "Specifies the name of the table on Vantage to read from.", type = "character", default = "")
parser <- add_argument(parser, "--db-name", help = "Specifies the name of the schema on Vantage.", type = "character", default = "")
parser <- add_argument(parser, "--delimiter", help="Specifies the delimiter user's script is expecting.", type="character", default = "\t")
parser <- add_argument(parser, "--logmech", help="Specifies the type of logon mechanism to establish a connection to Teradata Vantage.", 
                       type = "character", default = "TD2")
parser <- add_argument(parser, "--logdata", help = "Specifies parameters to the LOGMECH command beyond those needed by the logon mechanism, 
                       such as user ID, password and tokens", type = "character", default = "")
parser <- add_argument(parser, "--data-file-delimiter", help="Specifies the delimiter in data file.", type="character", default = "\t")
parser <- add_argument(parser, "--data-file-header", help="Specifies the boolean flag indicating whether the input data file has header.", type="logical", default = TRUE)

args <- parse_args(parser)

user_script_path <- trimws(args$user_script_path)
if (!file.exists(user_script_path)) {
  stop(paste("File", user_script_path, "does not exist."))
}

data_file_path <- trimws(args$data_file_path)
script_args <- trimws(args$script_args)
db_host <- trimws(args$db_host)
user <- trimws(args$user)
passwd <- trimws(args$passwd)
table <- trimws(args$table)
delimiter <- args$delimiter
logmech <- args$logmech
logdata <- args$logdata
data_file_delimiter <- args$data_file_delimiter
data_file_header <- args$data_file_header

# Raise error if the required inputs are not provided.
if (nchar(data_file_path) <= 0 &&
    !(nchar(db_host) > 0 && nchar(user) > 0 && nchar(passwd) > 0 && nchar(table) > 0)) {
  stop("Either --data-file-path or (--db-host, --user, --passwd and --table) are required.")
}

if (nchar(data_file_path) > 0) {
  if (!file.exists(data_file_path))
    stop(paste("File", data_file_path, "does not exist."))
  res_data_frame <- read.csv(data_file_path, sep = data_file_delimiter, 
                             header = data_file_header,
                             na.strings = c(""), stringsAsFactors = FALSE)
} else if (nchar(db_host) > 0 && nchar(user) > 0 && nchar(passwd) > 0 && nchar(table) > 0) {
  db_name = trimws(args$db_name)
  if (nchar(db_name) == 0) {
    db_name <- user
  }
  
  if (logmech == "JWT") {
    conn <- DBI::dbConnect(teradatasql::TeradataDriver(), host = db_host, user = user,
                           password = passwd, database = db_name, logmech = logmech, logdata = logdata)
  } else {
    conn <- DBI::dbConnect(teradatasql::TeradataDriver(), host = db_host, user = user,
                           password = passwd, database = db_name, logmech = logmech)
  }
  
  res_data_frame <- DBI::dbGetQuery(conn, paste("select * from ", table, " order by 1;"))
  DBI::dbDisconnect(conn)
}

# Create a temporary file out of data.frame, which is an input to the user script.
temp_data_file_path <- "input_data_file.txt"
for (i in 1:nrow(res_data_frame)) {
  # Collapsing each row using the delimiter specified in the argument.
  line <- paste0(res_data_frame[i,], collapse = delimiter)
  cat(line, file = temp_data_file_path, sep = "\n", append = TRUE)
}

# Execute user script.
execute_user_script(user_script_path, script_args, temp_data_file_path)

# Remove the file after completion of execution.
status <- file.remove(temp_data_file_path)