
#Problem 2 ----
transform_metadata_to_df <- function(stations_metadata) {
  
  df <- stations_metadata[[1]] %>%
    map(as_tibble) %>%
    list_rbind() %>%
    mutate(latestData = map_chr(latestData, 1, .default=NA_character_)) %>% 
    mutate(latestData = as_datetime(latestData, tz = "UTC")) %>% 
    unnest_wider(location) %>% 
    unnest_wider(latLon)
  
  return(df)
}

#Problem 4a ----
to_iso8601 <- function(datetime_var, offset_days) {
  
  datetime_var <- as_datetime(datetime_var)
  
  # Add the offset
  new_datetime <- datetime_var + days(offset_days)
  
  # Convert to ISO8601 format with a "Z" appended
  iso_datetime <- format(new_datetime, format = "%Y-%m-%dT%H:%M:%SZ")
  
  return(iso_datetime)
}

#Test:
to_iso8601(as_datetime("2016-09-01 10:11:12"),0)
to_iso8601(as_datetime("2016-09-01 10:11:12"),-4)

#Function to transform volumes:
transform_volumes <- function(json_data) {
  
  # Extract data into a data frame
  try({
    volumes <- json_data$trafficData$volume$byHour$edges
    
    df <- map_df(volumes, function(x) {
      data.frame(
        from = x$node$from,
        to = x$node$to,
        volume = x$node$total$volumeNumbers$volume,
        stringsAsFactors = FALSE
      )
    })
    
    # Convert time variables to datetime
    df$from <- lubridate::as_datetime(df$from)
    df$to <- lubridate::as_datetime(df$to)
    
    return(df)
    
  }, silent = TRUE)
}
