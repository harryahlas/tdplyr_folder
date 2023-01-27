input <- file('stdin', 'r')

# Input comes from STDIN (standard input).
# Reading one line at a time.
line <- readLines(input, 1)
while (length(line) > 0) {
  # Remove leading and trailing whitespace.
  line <- trimws(line, which = "both")

  # Split the line into words.
  words <- strsplit(line, " ")[[1]]

  for (word in words) {
    # Write the results to STDOUT (standard output);
    # whatever we output here will be the input for the
    # Reduce step, i.e., the input for reducer.R.
    #
    # Tab-delimited; the trivial word count is 1.
    cat(paste0(word, "\t", "1", "\n"))
  }

  # Reading one line at a time.
  line <- readLines(input, 1)
}