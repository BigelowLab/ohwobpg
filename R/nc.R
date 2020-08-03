#' Read the navigation data structure relative to world
#'
#' @export
#' @param x character, the name of the navigation file to read (default = "world_4km")
#' @return a list of navigation elements
#' \itemize{
#'   \item{bb the requested bounding box}
#'   \item{res the resolution as from \code{\link[raster]{res}}}
#'   \item{start vector of start values \code{[column, row]}}
#'   \item{count vector of count values \code{[columns, rows]}}
#'   \item{ext vector of extent as from \code{\link[raster]{extent}}}
#'   \item{crs character, proj string as from \code{\link[raster]{projection}}}
#'   \item{varname character}
#' }
read_navigation <- function(x = c("world_4km", "world_9km")[1]){
  fname <- system.file("nav", paste0(x[1], ".rds"), package = "obpg2tools")
  readRDS(fname)
}


#' Retrieve the base url for either HTML or OPENDAP
#'
#' @export
#' @param type character, either "html" or "opendap" (default)
#' @return URL
obpg_base_url <- function(type = c("html", "opendap")[2]){

  switch(tolower(type[1]),
         "html" = "https://oceandata.sci.gsfc.nasa.gov/opendap",
         "https://oceandata.sci.gsfc.nasa.gov:443/opendap")
}

#' Build a path for date, level, suite, resolution and parameter
#'
#' @export
#' @param dates Date, one or more dates as character (YYYY-mm-dd), Date-class, POSIXt-class
#'        If requesting a periods other than DAY, then dates refers to the start of the period
#' @param lvl character, the processing level Currently only "L3m" is tested.
#' @param param character, the name of parameter
#' @param suite character, the name of the paramter suite
#' @param period character, the period the imagery covers ala (DAY, 8D, MO, ...). Currently only
#'        DAY has been tested.
#' @param res charcater, either '4km' (default) or '9km'
#' @param platform character, the platform (mission, instrument and platform).  Currently, only
#'        "AQUA_MODIS" has been tested
#' @param baseuri character, the base URL to the resource
#' @param ext character, an optional extension to append to the URL
#' @return character URL
obpg_build_url <- function(dates = as.Date("2018-01-01"),
                           lvl = "L3m",
                           param = "chlor_a",
                           suite = "CHL",
                           period = "DAY",
                           res = "4km",
                           platform = "AQUA_MODIS",
                           baseuri = obpg_base_url(type = "opendap"),
                           ext = ".nc"){
  if(FALSE){
    # for development
    dates = as.Date("2018-01-01")
    lvl = "L3m"
    param = "chlor_a"
    suite = "CHL"
    period = "DAY"
    res = "4km"
    platform = "AQUA_MODIS"
    baseuri = obpg_base_url(type = "opendap")
    ext = ".nc"
  }

  if (period[1] != "DAY") warning("only period DAY has been tested")
  if (platform[1] != "AQUA_MODIS") warning("only platform AQUA_MODIS has been tested")
  if (lvl[1] != "L3m") warning("only level 3 has been tested: ", lvl)
  if (!inherits(dates, "Date")) {
    dates <- try(as.Date(dates))
    if (inherits(dates, "try-error")){
      stop("unable to convert dates to Date-class")
    }
  }

  if (nchar(ext[1]) > 0) ext <- paste0(".", sub(".", "", ext[1], fixed = TRUE))
  param <- tolower(param[1])
  suite <- toupper(suite[1])
  period <- toupper(period[1])

  if (!(res[1] %in% c("4km", "9km"))) stop("resolution not known: ", res[1])

  # old style baseuri
  #   https://oceandata.sci.gsfc.nasa.gov:443/opendap/
  #     MODISA/L3SMI/2018/001/A2018001.L3m_DAY_CHL_chlor_a_4km.nc
  # new style baseuri
  #  https://oceandata.sci.gsfc.nasa.gov:443/opendap
  #     MODISA/L3SMI/2018/0101/AQUA_MODIS.20180101.L3m.DAY.SST.sst.4km.nc

  if (obpg_style(param) == "new"){
    # new-style 2020/0101/AQUA_MODIS.20200101.L3m.DAY.SST.sst.4km.NRT.nc
    # or possibly without NRT
    # new-style 2020/0101/AQUA_MODIS.20200101.L3m.DAY.SST.sst.4km.nc

    per_string <- obpg_period_string(dates, period = period, style = "new")
    fname <- file.path(
                  format(dates, "%Y"),
                  format(dates, "%m%d"),
                  sprintf("%s.%s.%s.%s.%s.%s.%s%s",
                    platform[1],
                    per_string,
                    lvl[1],
                    period,
                    suite,
                    param,
                    res,
                    ext[1]))

  } else {
    # old-style: 2020/001/A2020001.L3m_DAY_CHL_chlor_a_4km.nc
    plat <- platform_codes(version = "2")[[platform]]
    per_string <- obpg_period_string(dates, period = period, style = "old")
    fname <- file.path(
                  format(dates, "%Y"),
                  format(dates, "%j"),
                  sprintf("%s%s.L3m_%s_%s_%s_%s%s",
                    plat,
                    per_string,
                    period,
                    suite,
                    param,
                    res,
                    ext[1]))
  }
  file.path(baseuri,
            switch(platform[1],
                   "AQUA_MODIS" = "MODISA",
                   stop("platform not known: ", platform[1])),
            switch(lvl[1],
                   "L3m"  = "L3SMI",
                   stop("level not known: ", platform[1])),
            fname)
}

#' Retrieve the known OBPG resolution
#'
#' @export
#' @param what character, specify the resultion with either "4km" or "9km"
#' @return a two element [x,y] vector
obpg_res <- function(what = c("4km", "9km")[1]){
  switch(tolower(what[1]),
         "9km" = c(0.0833236929814815, 0.0833140455652778),
         "4km" =c(0.0416666679084301, 0.0416666679084301),
         stop("resolution not known: ", what[1])
  )
}


#' Retrieve OBPG navigation values (start, count, lons, lats).  Create one of these
#' to resuse hwen downloading a suite of images from related NCDF resources (like a series
#' of days for a given product and parameter).
#'
#' @export
#' @param x ncdfd4 object
#' @param bb numeric, 4 element requested bounding box [west, east, south, north]
#' @param res numeric, 2 element resolution [res_x,res_y]
#' @param varname character the name of the variable
#' @return list with
#' \itemize{
#'   \item{bb the requested bounding box}
#'   \item{res the resolution}
#'   \item{start vector of start values}
#'   \item{count vector of count values}
#'   \item{ext vector of extent (for raster::raster)}
#'   \item{crs character, proj string for raster::raster}
#'   \item{varname character}
#' }
obpg_nc_nav <- function(x,
                        bb = c(-77, -42.5, 36.5, 56.7),
                        res = obpg_res(what = "4km"),
                        varname = "sst"){
  stopifnot(inherits(x, 'ncdf4'))
  if (!(varname[1] %in% names(x$var))) stop("varname not known:", varname[1])
  if (length(res) == 1) res <- c(res[1],res[1])
  r2 <- res/2
  # pad bb by res/2 so that we cast a large enough net
  bb2 <- bb + c(-r2[1], r2[1], -r2[2], r2[2])
  ix <- sapply(bb2[1:2],
               function(xbb) which.min(abs(x$dim$lon$vals-xbb)))
  we <- x$dim$lon$vals[ix]
  iy <- sapply(bb2[3:4],
               function(ybb) which.min(abs(x$dim$lat$vals-ybb)))
  sn <- x$dim$lat$vals[iy]

  list(bb = bb,
       res = res,
       start = c(ix[1], iy[2]),
       count = c(ix[2] - ix[1] + 1, iy[1] - iy[2] + 1),
       ext = c(we + (res[1]/2 * c(-1,1)), sn + (res[2]/2 * c(-1,1)) ),
       crs = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0",
       varname = varname)
}

#' Open an ncdf4 connection to an OBPG URL
#'
#' @export
#' @param uri character the URL
#' @param try_nrt logical, if TRUE and the connection fails, then try again
#'   after modifying the url to ".NRT.nc" instead of ".nc"
#' @param verbose logical, if TRUE voice warnings
#' @return either ncdf4 object or NULL
obpg_open <- function(uri, try_nrt = TRUE, verbose = FALSE){

  # first see if the web page is available - cheap and quick
  htmluri <- paste0(gsub(obpg_base_url(),
                         obpg_base_url("html"),
                         uri,
                         fixed = TRUE),
                    ".html")
  ok <- crul::ok(htmluri)
  if (!ok){
    if (verbose) warning("opendap html fail:", htmluri)
    if (try_nrt){
      htmluri2 <- sub(".nc.html", ".NRT.nc.html", htmluri, fixed = TRUE)
      ok2 <- crul::ok(htmluri2)
      if (ok2){
        uri <- sub(".nc", ".NRT.nc", uri, fixed = TRUE)
      } else {
        if (verbose) warning("opendap html with NRT fail:", htmluri2)
        return(NULL)
      }
    } else {
      return(NULL)
    }

  }
  x <- try(ncdf4::nc_open(uri))
  # the follwoing duplicates the error checking above... just because
  if (inherits(x,"try-error")){
    if (verbose) warning("nc_open fail:", uri)
    if (try_nrt){
      uri2 <- sub(".nc", ".NRT.nc", uri, fixed = TRUE)
      x <- try(ncdf4::nc_open(uri2))
      if (inherits(x,"try-error")){
        if (verbose) warning("nc_open fail with NRT:", uri2)
        x <- NULL
      }
    } else {
      x <- NULL
    }
  }
  x
}

#' Close an ncdf4 connection to an OBPG URL
#'
#' @export
#' @param x ncdf4 class object
#' @return NULL invisibly
obpg_close <- function(x){
  if(inherits(x, "ncdf4")) ncdf4::nc_close(x)
  invisible(NULL)
}


#' Fetch a single OBPG raster
#'
#' @export
#' @param x ncdf4 object or URL.  If an ncdf4 object, then it will be left open so be sure to
#'        close it when you are done.
#' @param nav list as returned by \code{obpg_nc_nav}
#' @param outpath character or NA to prevent file writing.  If provided, the
#'    actually path written to will be <path>/YYYY/mmdd/filename
#' @param overwrite logical, see \code{raster}
#' @param fmt character either 'raster' or 'GTiff' (default)
#' @param try_nrt logical, if TRUE and the connection fails, then try again
#'   after modifying the url to ".NRT.nc" instead of ".nc"
#' @return a raster layer or NULL
obpg_fetch <- function(x,
                       nav,
                       outpath = NA,
                       overwrite = TRUE,
                       fmt = "GTiff",
                       try_nrt = TRUE){
  if (!inherits(x, "ncdf4")) {
    close_me <- TRUE
    x <- obpg_open(x, try_nrt = TRUE)
    if (is.null(x)) return(x)
  } else {
    close_me <- FALSE
  }
  m <- ncdf4::ncvar_get(x,
                        varid = nav$varname,
                        start = nav$start,
                        count = nav$count)
  name <- basename(x$filename)
  if (close_me) obpg_close(x)
  r <- raster::raster(t(m),
                      crs = nav$crs,
                      xmn = nav$ext[1], xmx = nav$ext[2],
                      ymn = nav$ext[3], ymx = nav$ext[4])
  names(r) <- name

  if (!is.na(outpath)){
    if (!dir.exists(outpath)) {
      stopifnot(dir.create(outpath, recursive = TRUE, showWarnings = FALSE))
    }
    ext <- c("raster" = ".grd" , "GTiff" = ".tif" )[fmt]
    # the following handles the opbg1 to opbg2 transform
    db <- as_database(name)
    ofile <- as_filename(db, path = outpath, ext = ext)
    if (!dir.exists(dirname(ofile))) ok <- dir.create(dirname(ofile),
                                                      recursive = TRUE,
                                                      showWarnings = FALSE)
    raster::writeRaster(r,
                        filename = ofile,
                        overwrite = TRUE,
                        format = fmt)
  }
  r
}


