# This script shows how we created the small database of OBPG by fetching one year
# of monthly low resolution data for sst, chlor_a and par.  We also include one
# month of daily sst for the same region and resolution.

library(ohwobpg)
library(dplyr)

#' Fetch one or more dates of a particular OBPG product specification
#'
#' @param dates Date-class, vector of one or more dates
#' @param bb numeric, a 4-element bounding box in [west, east, south, north] order
#' @param param character, the name of the parameter
#' @param period character, the name of the period
#' @param res character, the name of the resolution
#' @param path character, the output storage path
#' @return a data frame database
do_fetch <- function(dates,
                     bb = c(-180, 180, -90, 90),
                     param = c("sst", "chlor_a")[1],
                     period = c("MO", "8D", "DAY")[1],
                     res = c("9km", "4km")[1],
                     path = "."){

  # determine which suite the data belongs to
  suite <- switch(param[1],
    "sst" = "SST",
    "chlor_a" = "CHL",
    "par" = "PAR",
    "pic" = "PIC",
    "poc" = "POC",
    toupper(param[1])) # this last one is a guess and might not work

  # build the URLs
  urls <- ohwobpg::obpg_build_url(
    dates = dates,
    param = param[1],
    suite = suite,
    period = period[1],
    res = res[1])

  # create a navigation list
  nc1 <- ohwobpg::obpg_open(urls[1])
  nav <- ohwobpg::obpg_nc_nav(nc1,
                              bb = bb,
                              res = ohwobpg::obpg_res(what = res[1]),
                              varname = param[1])
  ohwobpg::obpg_close(nc1)

  # grab the files
  for (this_url in urls){
    new_data <- ohwobpg::obpg_fetch(this_url, nav, outpath = path)
  }

  return(ohwobpg::as_database(urls))
}

# define an output path
PATH <- "~/gom"
ok <- dir.create(PATH, showWarnings = FALSE, recursive = TRUE)

# and define our bounding box [west, east, south, north]
BB <- c(-72, -63, 39, 46)

# low resolution for space savings
RES <- "9km"

###
# monthlies in 2018 for sst, chlor_a and par
###

dates <- seq(
  from = as.Date("2018-01-01"),
  to = as.Date("2018-12-01"),
  by = "month")

# now we grab the files for each parameter.  The function returns a small database
# (as a data frame) that identifies the files downloaded.

m_sst <- do_fetch(dates,
                  bb = BB,
                  path = PATH,
                  param = "sst",
                  res = RES,
                  period = "MO")

m_chl <- do_fetch(dates,
                  bb = BB,
                  path = PATH,
                  param = "chlor_a",
                  res = RES,
                  period = "MO")

m_par <-  do_fetch(dates,
                   bb = BB,
                   path = PATH,
                   param = "par",
                   res = RES,
                   period = "MO")


###
# dailies in August 2018 for sst
###
dates <- seq(from = as.Date("2018-08-01"),
             to = as.Date("2018-08-31"),
             by = "day")
d_sst <-  do_fetch(dates,
                   bb = BB,
                   path = PATH,
                   param = "sst",
                   res = RES,
                   period = "DAY")


###
#  Create the database by binding each of the little databases together, and save it.
#  The pipe operator "%>%" passes the output of one function to the input of the next;
#  the pipe is similar to the unix/linux pipe "|".
###

db <- dplyr::bind_rows(m_sst, m_chl, m_par, d_sst) %>%
  ohwobpg::write_database(PATH)

