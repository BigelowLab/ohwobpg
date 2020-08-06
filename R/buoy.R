#' Read in buoy data
#'
#' @export
#' @seealso \href{http://gyre.umeoce.maine.edu/data/gomoos/buoy/html/M01.html}{UMaine Buoy M01 Data}
#' @seealso \href{http://gyre.umeoce.maine.edu/data/gomoos/buoy/html/I01.html}{UMaine Buoy I01 Data}
#' @param buoy the buoy to load - either "I01" or "M01"
#' @return a data frame (tibble) of buoy data
read_buoy <- function(buoy = c("I01","M01")[1]){

  filename <- system.file(file.path("buoy", paste0(buoy[1], "_sbe37_1m.csv.gz")),
                     package = "ohwobpg")
  if(!file.exists(filename)) stop("file not found: ", filename)
  suppressMessages(readr::read_csv(filename))
}


#' Retrieve buoy location information
#'
#' @export
#' @return a data frame (tibble) of ID, lon and lat
buoy_locations <- function(){
  filename <- system.file(file.path("buoy", "locations.csv"), package = "ohwobpg")
  suppressMessages(readr::read_csv(filename))
}
