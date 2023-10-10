# This file contains tests to be applied to 
# the Vegvesen stations-data *after* being transformed
# to a data frame. 
# 
# All tests are packed in a function test_stations_metadata that apples
# all the aforementioned tests

test_stations_metadata_colnames <-
  function(df) {
    
    expected_colnames <- c("id", "name", "latestData", "lat", "lon")
    
    if (all(colnames(df) == expected_colnames) == TRUE) {
      print("PASS: Data has the correct columns")
    } else{
      print("FAIL: Columns do not match the correct specification")
    }
  } # this function test for expected column names in a dataframe and will return 
# "PASS: ..." if the expected column names is the same as in the data frame the function is applied to.
# It will return "FAIL.." indicating that the test has failed and 
# the expected column names is not the same as the vector in this function.

test_stations_metadata_nrows <-
  function(df) {
    
    min_expected_rows <- 5000
    max_expected_rows <- 10000
    
    if (nrow(df) > min_expected_rows & nrow(df) < max_expected_rows) {
      print("PASS: Data has a reasonable number of rows")
    } else if (nrow(df) <= min_expected_rows) {
      print("FAIL: Data has suspiciously few rows")
    } else {
      print("FAIL: Data has suspiciously many rows")
    }
  } #This function test for expected number of rows in a dataframe.
    # It wil return "PASS:.." if the number of rows in the dataframe is between 
    # 5000 and 10 000.
    #It wil return "Fail:..." if the number of rows are less than 5000 or if 
    # the number of rows are more than 10 000

test_stations_metadata_coltypes <-
  function(df) {
    expected_coltypes <-
      c("character", "character", "double", "double", "double")
    
    if (all(df %>%
            map_chr( ~ typeof(.)) == expected_coltypes) == TRUE) {
      print("PASS: All cols have the correct specifications")
    } else{
      print("FAIL: Columns do not have the correct specification")
    }
  } #This function test for the expected column types that can be found in a dataframe.
    # The specified column types in the vector will be matched with the dataframe.
    # If the expected column types is the same as in the dataframe the test will return
    # "PASS:..."
    # And if the expected column types is different it will return "FAIL:...".
  
test_stations_metadata_nmissing <-
  function(df) {
    max_miss_vals <- 200
    
    if (df %>% map_int( ~ sum(is.na((.)))) %>% sum(.) < max_miss_vals) {
      print("PASS: Amount of missing values is reasonable")
    } else {
      print("FAIL: Too many missing values in data set")
    }
  } #This function is used to test for missing values and will return
    # "PASS:..." if missing values is below 200. It will return "FAIL:..." if
    # missing values is above 200.

test_stations_metadata_latestdata_timezone <-
  function(df) {
    
    if (attr(df$latestData,"tzone")=="UTC") {
      print("PASS: latestData has UTC-time zone")
    } else {
      print("FAIL: latestData does not have expected UTC-time zone")
    }
  } # This function is used to test if the latestData column has the right time zone
    # in this example "UCT". It will return PASS if the time zone is correct and 
    # FAIL if the time zone is not the same as the expected time zone(UCT)


test_stations_metadata <- 
  function(df){
    test_stations_metadata_colnames(df)
    test_stations_metadata_coltypes(df)
    test_stations_metadata_nmissing(df)
    test_stations_metadata_nrows(df)
    test_stations_metadata_latestdata_timezone(df)
  }
# This code will use each function and apply it to a dataframe. This is useful
# because we can write one line of code to test all 5 tests at once.




