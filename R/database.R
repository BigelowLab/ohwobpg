#' Decompose one or more obpg filenames into the obpg database
#'
#' @export
#' @param x character, vector of filenames to decompose. Path is discarded.
#' @return a database as a data frame (tibble)
#'  \itemize{
#'    \item{date, Date  We only use L3 which is by day, so no POSIXct}
#'    \item{year, character date formatted as YYYY}
#'    \item{mmdd, character date formatted as mmdd}
#'    \item{mit, character mission-instrument-type}
#'    \item{level, character string indicating the level}
#'    \item{period, charcater period indicator for L3}
#'    \item{suite, character suite identifier}
#'    \item{param, character parameter name chlor_a, sst, etc}
#'    \item{per, compositing period such as DAY, MO, 8D}
#'    \item{res, character resolution in km}
#'    \item{nrt, character optional Near Real-Time identifier}
#'    \item{file, basename of the input file less the extension}
#'  }

as_database <- function(x = c("A2004001.L3m_DAY_CHL_chlor_a_4km.nc",
                              "A2004001.L3m_DAY_SST_sst_4km.nc",
                              "AQUA_MODIS.20191130.L3m.DAY.NSST.sst.4km.NRT.nc",
                              "NPP_VIIRS.20190703.L3m.DAY.SST.sst.4km.nc")){

  as_db <- function(f){
    raster::extension(f) <- ""
    ss    <- strsplit(f, ".", fixed = TRUE)
    date  <- as.Date(sapply(ss, function(s) substring(s[2],1,8)), format = "%Y%m%d")
    nrt   <- rep(NA_character_, length(ss))
    len <- lengths(ss)
    ix <- len > 7
    if (any(ix)) nrt[ix] <- sapply(ss[ix], "[[", 8)
    dplyr::tibble(
      date,
      year  = format(date, "%Y"),
      mmdd  = format(date, "%m%d"),
      mit   = sapply(ss, "[[", 1),
      lvl   = sapply(ss, "[[", 3),
      per   = sapply(ss, "[[", 4),
      suite = sapply(ss, "[[", 5),
      param = sapply(ss, "[[", 6),
      res   = sapply(ss, "[[", 7),
      nrt,
      file  = gsub(".NRT", "", sapply(ss, paste, collapse = "."), fixed = TRUE)
    )
  } # to_obpg2

  x <- basename(x)
  ix <- is_obpg1(x)
  # convert obpg old-style to new-style
  x[ix] <- obpg1_to_obpg2(x[ix])
  # then convert
  as_db(x)
}

#' Convert a database entry to a filename
#'
#' @export
#' @param x data frame (tibble), to convert
#' @param path character, the path to prepend to the filename
#' @param ext character, the extension to append to the filename
#' @return one or more filenames
as_filename <- function(x, path = ".", ext = ".tif"){
  f = x$file
  if(nchar(ext)>0) f <- paste0(f, ext)
  file.path(path, x$year, x$mmdd, f)
}



#' Write a database to file as CSV (possible compressed)
#'
#' @export
#' @param x data frame (tibble), the data to store
#' @param path character, the path to the database, by default "."
#' @param filename character, by default 'database.csv.gz' but it can be any other filename
write_database <- function(x,
                          path = ".",
                          filename = "database.csv.gz"){
  fname <- file.path(path, filename)
  readr::write_csv(x, fname)
}

#' Read a database file stored as CSV (possible compressed)
#'
#' @export
#' @param x character, the path to the database, by default "."
#' @param filename character, by default 'database.csv.gz' but it can be any other filename
read_database <- function(x = ".",
                          filename = "database.csv.gz"){
  fname <- file.path(x, filename)
  if (!file.exists(fname)) stop("file not found:", fname)
  suppressMessages(readr::read_csv(fname))
}




