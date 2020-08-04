#' Read in buoy data
#'
#' @export
#' @seealso \href{http://gyre.umeoce.maine.edu/data/gomoos/buoy/html/M01.html}{UMaine Buoy Data}
#' @param buoy the
read_buoy <- function(buoy = "M01"){

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
