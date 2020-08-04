# [OBPG](https://oceancolor.gsfc.nasa.gov/) for [Ocean Hack Week 2020](https://oceanhackweek.github.io/)

Ocean Color Processing Group (OPBG) serves satellite data via [OPeNDAP](https://www.opendap.org/).  This packages provides simple tools in [R language](https://www.r-project.org/) for downloading subsets of global data files, and proposes a simple method for storing and managing the datasets.

This package is deminstrates working with [Level 3](https://oceancolor.gsfc.nasa.gov/products/) (simple global grids) specifically from the [AQUA_MODIS](https://oceancolor.gsfc.nasa.gov/data/aqua/) instrumentation.  The package may be adaptable for other products/instruments, but we haven't tried anything other than AQUA_MODIS Level 3 mapped images.

OBPG managers are migrating from an old-style [naming convention](https://oceancolor.gsfc.nasa.gov/docs/filenaming-convention/) to a new-style. Currently, only recently reprocessed data (SST) are served in the new-style. That means this code attemtps to handle either convention seamlessly while navigating the system. Eventually, all products will be served in the new-style filenaming format, so we have kept that in mind when proposing a local storage and management system.

## Requirements

  + [R v3+](https://www.r-project.org/) 
  
  + [raster](https://CRAN.R-project.org/package=raster)
  
  + [ncdf4](https://CRAN.R-project.org/package=ncdf4)
  
  + [dplyr](https://CRAN.R-project.org/package=dplyr)
  
  + [readr](https://CRAN.R-project.org/package=readr)
  
  + [leaflet](https://CRAN.R-project.org/package=leaflet)
  
  + [ggplot2](https://CRAN.R-project.org/package=ggplot2)
  
  + [crul](https://CRAN.R-project.org/package=crul)
  
## Installation

Use the [remotes](https://CRAN.R-project.org/package=remotes) package to install with ease.

```
remotes::install_github("BigelowLab/ohwobpg")
```

## Storing data

OBPG data naturally organize under a simple heirarchy `<root>/region/yyyy/mmdd/files`. We find that allowing the end user to specify the `<root>/region` while autmatically enforcing the remainder `yyyy/mmdd/files` works really well. For example, suppose you are going to download daily SST and CHLOR_A from AQUA_MODIS covering the Gulf of Maine in 2018.  We suggest that you create the root path like the following shows - a simple directory in you home directory (but whatever works for you works for us.)

```
path <- "~/gom"
dir.create(path, recursive = TRUE)
```

Any data you subsequently download using this package will automatically create subdirectories that are required. Below is an example from our own lab where `<root>/region` is `/mnt/ecocast/coredata/obpg2/nwa/AQUA_MODIS/L3m` and the automatically generated subdirectories, `yyyy/mmdd`, are `2018/0101`.

```
/mnt/ecocast/coredata/obpg2/nwa/AQUA_MODIS/L3m/2018/0101
├── AQUA_MODIS.20180101.L3m.16DR.CHL.chlor_a.4km.tif
├── AQUA_MODIS.20180101.L3m.32DR.CHL.chlor_a.4km.tif
├── AQUA_MODIS.20180101.L3m.8DR.CHL.chlor_a.4km.tif
├── AQUA_MODIS.20180101.L3m.8DR.PAR.par.4km.tif
├── AQUA_MODIS.20180101.L3m.8DR.PIC.pic.4km.tif
├── AQUA_MODIS.20180101.L3m.8DR.POC.poc.4km.tif
├── AQUA_MODIS.20180101.L3m.8DR.SST.sst.4km.tif
├── AQUA_MODIS.20180101.L3m.8DR.SST.sst_slope.4km.tif
├── AQUA_MODIS.20180101.L3m.DAY.CHL.chlor_a.4km.tif
├── AQUA_MODIS.20180101.L3m.DAY.CHL.chlor_a_cum.4km.tif
├── AQUA_MODIS.20180101.L3m.DAY.CHL.chlor_a_fill.4km.tif
├── AQUA_MODIS.20180101.L3m.DAY.PAR.par.4km.tif
├── AQUA_MODIS.20180101.L3m.DAY.PIC.pic.4km.tif
├── AQUA_MODIS.20180101.L3m.DAY.POC.poc.4km.tif
└── AQUA_MODIS.20180101.L3m.DAY.SST.sst.4km.tif
```

## Downloading example


Let's download 2018 monthly CHLOR_A data at 9km resoltion just for the Gulf of Maine region. First we build a series of URLs for the data using `obpg_build_url()`.  The function has a number of arguments, but we'll just focus on what we need and accept the default values for the others.  Complete documentation is available by typing at the console, `?obpg_build_url`.

```
library(ohwobpg)

# first we need a sequence of dates
dates <- seq(
  from = as.Date("2018-01-01"), 
  to = as.Date("2018-12-01"), 
  by = "month")

urls <- obpg_build_url(
  dates = dates,
  param = "chlor_a",
  suite = "CHL",
  period = "MO",
  res = "9km")
  
# and define our bounding box [west, east, south, north]
BB <- c(-72, -63, 39, 46)
```

Now we'll open just the first NCDF resource.  From that we'll build a simple list of items we need to successfully navigate the remainder of the URLs. Then we can close the NCDF resource.

```
nc1 <- obpg_open(urls[1])
nav <- obpg_nc_nav(nc1,
  bb = BB,
  res = obpg_res(what = "9km"),
  varname = "chlor_a")
obpg_close(nc1)
```

Now we simply need to iterate through the dates - downloading the subset data and storing in our path.

```
for (this_url in urls){
  cat("fetching", basename(this_url), "\n")
  new_data <- obpg_fetch(this_url, nav, outpath = path)
}
```

> ### More Data
> 
> We have downloaded a daily dataset of sst and chlor_a for the Gulf of Maine which we will
> work with later.  The script we used can be found [here]().

## Make a database

It is easy to create a database by first creating a list of files, then parsing to the database format. 

```
files <- list.files(path, pattern = glob2rx("*.tif"), full.names = TRUE)
db <- as_database(files)
write_database(db, path)
db
```

The nrt column refers to "near real time" data. OBPG group first publishes it data flagged as "nrt".  Some time later (weeks? months?), after quality review and adjustments, the data is republished without the "nrt" flag. For this tutorial we'll ignore it, but one could use that to identify local files suitable for updating when OBPG updates. 


The database can be easily filtered to select just the images needed; for this task we leverage the tools in the [dplyr](https://CRAN.R-project.org/package=dplyr) package.

```
library(dplyr, warn.conflicts = FALSE)
sub_db <- db %>% 
  dplyr::filter(param == "sst" & 
                per == "MO" &
                dplyr::between(date, as.Date("2018-05-15"), as.Date("2018-09-26")))
```

Using the filtered database we then read in a subset of records into a raster stack of images.

```
library(raster)
files <- as_filename(sub_db, path)
x <- raster::stack(files)
```


## TODO

Navigating a stack (just simple - there are plenty of online tutorials)
Extracting from a stack (just simple - there are plenty of online tutorials)... a point, a patch of points, a polygon

Plotting rasters (just simple - there are plenty of online tutorials)... base graphics, ggplot2 and leaflet (for HTML)

Creating a derived stack (like CHLOR_A cumulative and adding to the database)

