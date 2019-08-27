#' @export
xts_to_tibble <- function(x, timestamp_format) {
  
  #x <- debug_data %>% purrr::map(response_to_list)
  #x <<- test2
  #stop()
  #timestamp_format <- "s"
  
  # development options
  performance <- FALSE
  timer <- function(x, txt) {message(paste(Sys.time(), txt));x}
  
  # create divisor for different timestamp format
  div <- get_precision_divisor(timestamp_format)
  
  # set default result
  result_na <- tibble::tibble(statement_id = NA,
                              series_names = NA,
                              series_tags = NA,
                              series_values = NA,
                              series_partial = NA)
  
  # remove hierarchies to get direct access results level
  while (!("results" %in% names(x))) {
    x <- purrr::flatten(x)
  }
  
  # flatten once again to get directly to "statement_id" and "series"
  x <- purrr::flatten(x)
  
  # iterate over result array
  list_of_result <- purrr::map(x, .f = function(series_ele) {
    
    # set dummy result
    result <- result_na
    
    if (!is.null(series_ele$statement_id)) {
      # extract "statement_id"
      statement_id <- series_ele$statement_id
    } else {
      # set NA to statement_id if not found
      statement_id <- NA_integer_
    }
    
    
    
    if (!is.null(series_ele$series)) {
      # extract "measurement names"
      series_names <- purrr::map(series_ele$series, "name") %>%
        unlist() %>%
        `if`(is.null(.), NA, .)
      
      # extract "tags"
      series_tags <- purrr::map(series_ele$series, "tags") %>%
        purrr::map(tibble::as_tibble)
      
      # extract "columns"
      series_columns <- purrr::map(series_ele$series, "columns") %>%
        purrr::map(unlist)
      
      # extract values
      series_values <- purrr::map(series_ele$series, "values") %>%
        # transpose for faster data munging
        `if`(performance, timer(., "transpose data"), .) %>% 
        purrr::map( ~ purrr::transpose(.)) %>% 
        # convert influxdb NULL to NA
        `if`(performance, timer(., "convert influxdb NULL to NA"), .) %>% 
        purrr::map( ~ purrr::map(., ~ purrr::map(., ~ . %||% NA))) %>% 
        # unlist for faster data munging
        `if`(performance, timer(., "unlist data"), .) %>% 
        purrr::map( ~ purrr::map(., base::unlist)) %>%
        # convert int to dbl (required for unnesting)
        `if`(performance, timer(., "unify numerics"), .) %>% 
        purrr::map( ~ purrr::map_if(., is.integer, as.double)) %>%
        # set names 
        `if`(performance, timer(., "setting column names"), .) %>% 
        purrr::map2(., .y  = series_columns, ~ purrr::set_names(., nm = .y)) %>%
        # influxdb ALWAYS stores data in GMT!!
        `if`(performance, timer(., "set POSIX-based time index"), .) %>% 
        purrr::map( ~ purrr::map_at(., .at = "time",
                                    ~ as.POSIXct(. / div, 
                                                 origin = "1970-1-1",
                                                 tz = "GMT")) %>%
                      tibble::as_tibble(., validate = FALSE))
      
      # is partial?
      series_partial <-
        ifelse(is.null(series_ele$partial), FALSE, TRUE)
      
      # RETURN AGGREGATED TIBBLE WITH LIST COLUMNS!
      result <- tibble::tibble(statement_id,
                               series_names,
                               series_tags,
                               series_values,
                               series_partial)
      
    } else {
      # in case of an error...
      if (!is.null(series_ele$error)) {
        stop(series_ele$error, call. = FALSE)
      }
      
      warning("no series returned")
      
    }
    
    return(result)
    
  })
  # return list of tibbles
  return(list_of_result)
  
}
