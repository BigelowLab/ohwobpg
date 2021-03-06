% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/nc.R
\name{obpg_fetch}
\alias{obpg_fetch}
\title{Fetch a single OBPG raster}
\usage{
obpg_fetch(
  x,
  nav,
  outpath = NA,
  overwrite = TRUE,
  fmt = "GTiff",
  try_nrt = TRUE
)
}
\arguments{
\item{x}{ncdf4 object or URL.  If an ncdf4 object, then it will be left open so be sure to
close it when you are done.}

\item{nav}{list as returned by \code{obpg_nc_nav}}

\item{outpath}{character or NA to prevent file writing.  If provided, the
actually path written to will be <path>/YYYY/mmdd/filename}

\item{overwrite}{logical, see \code{raster}}

\item{fmt}{character either 'raster' or 'GTiff' (default)}

\item{try_nrt}{logical, if TRUE and the connection fails, then try again
after modifying the url to ".NRT.nc" instead of ".nc"}
}
\value{
a raster layer or NULL
}
\description{
Fetch a single OBPG raster
}
