find_parms_cumulative <- function(x, split = "_",
                                  words_vars = c("private", "cabins", "public", "high", "low", "roads", "trails"),
                                  words_cum = c("cumulative", "nearest"),
                                  words_function = c("Gauss", "exp", "decay", "threshold", "bartlett", "log")) {
  
  # saparate words
  words <- strsplit(as.character(x), split = "_")
  
  # function to grep words and re-merge them
  grep_merge <- function(x) 
    purrr::map(words, ~ grep(pattern = paste(x, collapse = "|"), .x, value = TRUE)) %>%
    purrr::map_chr(~ paste(.x, collapse = "_"))
  
  tibble::tibble(
    # variable
    vars = grep_merge(words_vars),
    # cumulative
    cum = grep_merge(words_cum),
    # function
    func = grep_merge(words_function),
    # zoi
    zoi = purrr::map_dbl(words, ~ as.numeric(last(.x)))
  )
  
}
