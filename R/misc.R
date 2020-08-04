#' Perform grepl on multiple patterns; it's like  AND-ing or OR-ing successive grepl statements.
#'
#' @param pattern character vector of patterns
#' @param x the character vector to search
#' @param op logical vector operator back quoted, defaults to `|`
#' @param ... further arguments for \code{grepl} like \code{fixed} etc.
#' @return logical vector
mgrepl <- function(pattern, x, op = `|`, ... ){
  Reduce(op, lapply(pattern, grepl, x, ...))
}

#' Exchange a character vector values and names
#'
#' @export
#' @param x a named character vector
#' @return a named character vector
invert_vector <- function(x){
  if (!inherits(x, "character")) stop("input must be character")
  if (is.null(names(x))) stop("input must be named vector")
  nm <- names(x)
  v <- unname(x)
  x <- nm
  names(x) <- v
  x
}

#' Returns a named list of known platform codes
#'
#' @export
#' @param version character either "2" (default) or "1".  Refers to the
#'        filenaming OBPG convention
#' @param invert logical if TRUE invert the name-value relationship
#' @return a named character vector of platform codes
platform_codes <- function(invert = FALSE, version = c("2", "1")[1]){

  x <- switch(as.character(version[1]),
              "2" = c(
                "SEASTAR_SEAWIFS_GAC"      	= "S",
                "AQUA_MODIS"   							= "A",
                "TERRA_MODIS"  							= "T",
                "ADEOS_OCTS_GAC"            = "O",
                "CZCS"                      = "C",
                "OCM2"                      = "O2_",
                "SNPP_VIIRS"                = "V",
                "Aquarius"                  = "Q"),
              "1" = c(
                "SeaWiFS"      = "S",
                "Aqua MODIS"   = "A",
                "Terra MODIS"  = "T",
                "OCTS"         = "O",
                "CZCS"         = "C",
                "OCM2"         = "O2_",
                "VIIRS"        = "V",
                "Aquarius"     = "Q" )
  )
  if (invert) x <- invert_vector(x)
  x
}

#' Determine if a filename is in OPBPG old-style format ("opbg1")
#'
#' Based upon the assumption the old-style uses up to 3 characters to identify
#' the platform before staring with the date.
#'
#' @export
#' @param x character a vector of one or more filenames (with or without path)
#' @return logical TRUE where the naming pattern matches the old-style OBPG naming convention
is_obpg1 <- function(x = c("A2004001.L3m_DAY_CHL_chlor_a_4km.nc",
                           "A2004001.L3m_DAY_SST_sst_4km.nc",
                           "AQUA_MODIS.20191130.L3m.DAY.NSST.sst.4km.NRT.nc",
                           "NPP_VIIRS.20190703.L3m.DAY.SST.sst.4km.nc",
                           "Ocean Hack Week")){

  ix <- grepl("^[[:alpha:]]{1,3}\\d", x)
  return(ix)
}

#' Convert OBPG old-style filenames to OBPG new-style filenames
#'
#' A2004001.L3m_DAY_CHL_chlor_a_4km becomes AQUA_MODIS.20040101.L3m.DAY.CHL.chlor_a.4km
#'
#' @export
#' @param x character, one or more obpg style filenames,
#'          directory paths and extensions are dropped
#' @return character vector of obpg2 style filenames without path and extension
obpg1_to_obpg2 <- function(x = c("A2004001.L3m_DAY_CHL_chlor_a_4km.nc",
                                 "A2004001.L3m_DAY_SST_sst_4km.nc",
                                 "AQUA_MODIS.20191130.L3m.DAY.NSST.sst.4km.NRT.nc",
                                 "NPP_VIIRS.20190703.L3m.DAY.SST.sst.4km.nc",
                                 "Ocean Hack Week")){

  # we only operate on OBPG old-style elements which we guess at
  index <- is_obpg1(x)
  if(!any(index)) return(x)
  f <- basename(x[index])
  ext <- raster::extension(f)
  plut <- platform_codes(invert = TRUE)
  raster::extension(f) <- ""
  segs <- strsplit(f, ".", fixed = TRUE)
  seg1 <- sapply(segs, '[',1)
  seg2 <- sapply(segs, '[',2)
  pp <- plut[substring(f, 1, 1)]
  dd <- sapply(seg1,
               function(x){
                 if (nchar(x) <= 8){
                   d <- format(as.Date(substring(x, 2, 8), format = "%Y%j"), "%Y%m%d")
                 } else {
                   d <- sprintf("%s_%s",
                                format(as.Date(substring(x, 2, 8), format = "%Y%j"), "%Y%m%d"),
                                format(as.Date(substring(x, 9, 15), format = "%Y%j"), "%Y%m%d") )
                 }
                 d
               } )
  ss <- strsplit(seg2, "_", fixed = TRUE)
  len <- lengths(ss)
  ix <- which(len == 6)
  if (length(ix) > 0)
    ss[ix] <- lapply(ss[ix],
                     function(s){
                       s[4] <- paste(s[4:5], collapse = "_")
                       s <- s[-5]
                       s
                     })
  ss <- sapply(ss, paste, collapse = ".")
  r <- sprintf("%s.%s.%s%s", pp, dd, ss, ext)
  x[index] <- r
  x
}


#' Guess the OBPG filenaming style from the parameter name
#'
#' @export
#' @param x one or more parameters
#' @return character vector, one per input, of "new" or "old" indicating filenaming style
obpg_style <- function(x = c("chlor_a", "par", "sst", "sst4")){

  lut <- c(old = FALSE, new = TRUE)
  ix <- x %in% c("sst", "sst4")
  names(lut[ix + 1])
}

#' Given a start date and a period code and style generate a an OBPG date pairing
#'
#' @export
#' @param dates Date-class the dates (always refers to the start if an extended period)
#' @param period character the period of the product (DAY, 8D, MO, etc)
#' @param style character either "new" or "old"
#' @return character vector with either YYYYmmdd or YYYYjjj style format
obpg_period_string <- function(dates, period = "DAY", style = "new"){

  stopifnot(inherits(dates, "Date"))
  per <- toupper(period[1])
  if (per != "DAY"){
    end <- switch(per,
                  "8D" = dates + 7,
                  "32R" = dates + 32,
                  "MO" = as.Date(sapply(seq_along(dates),
                                function(i){
                                  seq(from = dates[i], length = 2, by = "month")[2] - 1
                                }), origin = as.Date("1970-01-01")),
                  "YR" = as.Date(sapply(seq_along(dates),
                                        function(i){
                                          seq(from = dates[i], length = 2, by = "year")[2] - 1
                                        }), origin = as.Date("1970-01-01")),
                  stop("period not known: ", period)
    )
    if (tolower(style[1]) == "new"){
      s <- paste(format(dates, "%Y%m%d"), format(end, "%Y%m%d"), sep = "_")
    } else {
      s <- paste(format(dates, "%Y%j"), format(end, "%Y%j"), sep = "")
    }

  } else {
    if (tolower(style[1]) == "new"){
      s <- format(dates, "%Y%m%d")
    } else {
      s <-format(dates, "%Y%j")
    }
  }
  s
}
