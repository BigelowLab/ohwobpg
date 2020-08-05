# [OBPG](https://oceancolor.gsfc.nasa.gov/) for [Ocean Hack Week 2020](https://oceanhackweek.github.io/)

Ocean Color Processing Group (OPBG) serves satellite data via [OPeNDAP](https://www.opendap.org/).  This packages provides simple tools in [R language](https://www.r-project.org/) for downloading subsets of global data files, and proposes a simple method for storing and managing the datasets.

This package is demonstrates working with [Level 3](https://oceancolor.gsfc.nasa.gov/products/) (simple global grids) specifically from the [AQUA_MODIS](https://oceancolor.gsfc.nasa.gov/data/aqua/) instrumentation.  The package may be adaptable for other products/instruments, but we haven't tried anything other than AQUA_MODIS Level 3 mapped images.

OBPG managers are migrating from an old-style [naming convention](https://oceancolor.gsfc.nasa.gov/docs/filenaming-convention/) to a new-style. Currently, only recently reprocessed data (SST) are served in the new-style. That means this code attempts to handle either convention seamlessly while navigating the system. Eventually, all products will be served in the new-style filenaming format, so we have kept that in mind when proposing a local storage and management system.

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


Below is a brief tutorial/overview which is also available as an [Rmarkdown document](inst/tutorials/intro_to_OBPG.Rmd).

## Storing data

OBPG data naturally organize under a simple heirarchy `<root>/region/yyyy/mmdd/files`. We find that allowing the end user to specify the `<root>/region` while automatically enforcing the remainder `yyyy/mmdd/files` works really well. For example, suppose you are going to download daily SST and CHLOR_A from AQUA_MODIS covering the Gulf of Maine in 2018.  We suggest that you create the root path like the following shows - a simple directory in you home directory (but whatever works for you works for us.)

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

Let's download 2018 monthly CHLOR_A data at 9km resolution just for the Gulf of Maine region. First we build a series of URLs for the data using `obpg_build_url()`.  The function has a number of arguments, but we'll just focus on what we need and accept the default values for the others.  Complete documentation is available by typing at the console, `?obpg_build_url`.

```
library(ohwobpg)

# first we need a sequence of dates
dates <- seq(
  from = as.Date("2018-01-01"), 
  to = as.Date("2018-12-01"), 
  by = "month")

# then we build the URLs
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


> **Note** We have downloaded a larger dataset of sst, par and chlor_a for the Gulf of Maine which we will work with later.  The script we used for downloading can be found [here](https://github.com/BigelowLab/ohwobpg/blob/master/inst/scripts/fetch_ohw_obpg.R).

## Make a database and save it

It is easy to create a database by first creating a list of files, then parsing to the database format.  We actually have that list of files in hand already in our URLs, but for the sake of example, let's do a listing by file search instead.  Note that we use the pipe operator `%>%`, provided to us by the [dplyr](https://CRAN.R-project.org/package=dplyr) package, to pipe the output of one function to the the input of the next. There are [boat loads](https://rseek.org/?q=dplyr+tutorial) of tutorials on using dplyr available to you.

```
library(dplyr, warn.conflicts = FALSE)

db <- list.files(path, pattern = glob2rx("*.tif"), full.names = TRUE, recursive = TRUE) %>%
  as_database() %>%
  write_database(path)
```

## The database contents

The database is a very simple table (data frame) build from various elements of a filename. All of the parts could be compute as-needed which would make the file smaller to store on disk, but the ease of parsing and saving is worth the extra bit of disk required.  The OBPG filenames have all of the necessary information to uniquely identify each file - details can found in the documentation `?as_database`.  For now, let's just print it out and look at it.

```
db
# # A tibble: 12 x 11
#    date       year  mmdd  mit        lvl   per   suite param   res   nrt   file                                               
#    <chr>      <chr> <chr> <chr>      <chr> <chr> <chr> <chr>   <chr> <chr> <chr>                                              
#  1 2018-01-01 2018  0101  AQUA_MODIS L3m   MO    CHL   chlor_a 9km   NA    AQUA_MODIS.20180101_20180131.L3m.MO.CHL.chlor_a.9km
#  2 2018-02-01 2018  0201  AQUA_MODIS L3m   MO    CHL   chlor_a 9km   NA    AQUA_MODIS.20180201_20180228.L3m.MO.CHL.chlor_a.9km
#  3 2018-03-01 2018  0301  AQUA_MODIS L3m   MO    CHL   chlor_a 9km   NA    AQUA_MODIS.20180301_20180331.L3m.MO.CHL.chlor_a.9km
#  4 2018-04-01 2018  0401  AQUA_MODIS L3m   MO    CHL   chlor_a 9km   NA    AQUA_MODIS.20180401_20180430.L3m.MO.CHL.chlor_a.9km
#  5 2018-05-01 2018  0501  AQUA_MODIS L3m   MO    CHL   chlor_a 9km   NA    AQUA_MODIS.20180501_20180531.L3m.MO.CHL.chlor_a.9km
#  6 2018-06-01 2018  0601  AQUA_MODIS L3m   MO    CHL   chlor_a 9km   NA    AQUA_MODIS.20180601_20180630.L3m.MO.CHL.chlor_a.9km
#  7 2018-07-01 2018  0701  AQUA_MODIS L3m   MO    CHL   chlor_a 9km   NA    AQUA_MODIS.20180701_20180731.L3m.MO.CHL.chlor_a.9km
#  8 2018-08-01 2018  0801  AQUA_MODIS L3m   MO    CHL   chlor_a 9km   NA    AQUA_MODIS.20180801_20180831.L3m.MO.CHL.chlor_a.9km
#  9 2018-09-01 2018  0901  AQUA_MODIS L3m   MO    CHL   chlor_a 9km   NA    AQUA_MODIS.20180901_20180930.L3m.MO.CHL.chlor_a.9km
# 10 2018-10-01 2018  1001  AQUA_MODIS L3m   MO    CHL   chlor_a 9km   NA    AQUA_MODIS.20181001_20181031.L3m.MO.CHL.chlor_a.9km
# 11 2018-11-01 2018  1101  AQUA_MODIS L3m   MO    CHL   chlor_a 9km   NA    AQUA_MODIS.20181101_20181130.L3m.MO.CHL.chlor_a.9km
# 12 2018-12-01 2018  1201  AQUA_MODIS L3m   MO    CHL   chlor_a 9km   NA    AQUA_MODIS.20181201_20181231.L3m.MO.CHL.chlor_a.9km
```

> **Note** The nrt column refers to "near real time" data. OBPG group first publishes it data flagged as "nrt".  Some time later (weeks? months?), after quality review and adjustments, the data is republished without the "nrt" flag. For this tutorial we'll ignore it, but one could use that to identify local files suitable for updating when OBPG updates. 


Use the `read_database()` and `write_database()` functions for input and output.


## A larger dataset

We provide with the [ohwobpg](https://github.com/BigelowLab/ohwobpg) package a slightly larger and more complex dataset. This will save the need for each participant to download from the OBPG servers.  The larger dataset includes ...

  + monthly sst, chlor_a and par data from 2018 in the Gulf of Maine  
  
  + daily sst data from August 2018 in the Gulf of Maine
  
The path must now be redefined, and then we can read in the new associated database.

```
path <- system.file("gom", package = "ohwobpg")
db <- read_database(path)
```

We can do a quick summary by counting the records by period and parameter. 

```
db %>%                       # start with the database
  dplyr::count(per, param)   # count instance first by period then by parameter 
  
# # A tibble: 4 x 3
#   per   param       n
#   <chr> <chr>   <int>
# 1 DAY   sst        31
# 2 MO    chlor_a    12
# 3 MO    par        12
# 4 MO    sst        12
```

> **Note** almost every function in R comes from a package - it can be hard to remember where each comes from.  To help jog one's memory it can be helpful to prepend the package name to the function - for instance, instead of writing `count(...)` note that we wrotew `dplyr::count(...)`.  In this case, there is no difference between the two other than it is easy to recall to which package `count()` belongs. 

## Using the database to select files to read

The database can be easily filtered to chose just the images needed; for this task we continue leveraging the tools in the [dplyr](https://CRAN.R-project.org/package=dplyr) package.  Let's grab par monthly data between May and September of 2018. First we filter the database to a smaller subset, then convert it to a set of filenames, and finally load it into a raster stack. 

```
library(raster)

par_db <- db %>% 
  dplyr::filter(param == "sst" & 
                per == "MO" &
                dplyr::between(date, as.Date("2018-05-15"), as.Date("2018-09-26")))
                
# # A tibble: 4 x 11
#   date        year mmdd  mit        lvl   per   suite param res   nrt   file                                           
#   <date>     <dbl> <chr> <chr>      <chr> <chr> <chr> <chr> <chr> <lgl> <chr>                                          
# 1 2018-06-01  2018 0601  AQUA_MODIS L3m   MO    SST   sst   9km   NA    AQUA_MODIS.20180601_20180630.L3m.MO.SST.sst.9km
# 2 2018-07-01  2018 0701  AQUA_MODIS L3m   MO    SST   sst   9km   NA    AQUA_MODIS.20180701_20180731.L3m.MO.SST.sst.9km
# 3 2018-08-01  2018 0801  AQUA_MODIS L3m   MO    SST   sst   9km   NA    AQUA_MODIS.20180801_20180831.L3m.MO.SST.sst.9km
# 4 2018-09-01  2018 0901  AQUA_MODIS L3m   MO    SST   sst   9km   NA    AQUA_MODIS.20180901_20180930.L3m.MO.SST.sst.9km
```

Using the filtered database we then read in a subset of records into a raster stack of images.  By default each layer's name is assigned the filename from which it came, but that can make for really long names.  We know that each layer is one month, so we will assign each a new name: "Jun", "Jul", "Aug", "Sep".  You can lean more about formatting dates here `?strftime`. The are many [raster tutorials](https://rseek.org/?q=raster+tutorial) available and a handy [cheatsheet](https://rpubs.com/etiennebr/visualraster).

```
library(raster)

par <- par_db %>%                # start with the subset database
  as_filename(path = path) %>%   # build filenames and append to the path
  raster::stack()                # read them into a stack of images
  
names(par) <- format(par_db$date, "%b")

par

# class      : RasterStack 
# dimensions : 86, 110, 9460, 4  (nrow, ncol, ncell, nlayers)
# resolution : 0.08333323, 0.08333309  (x, y)
# extent     : -72.08333, -62.91667, 38.91668, 46.08332  (xmin, xmax, ymin, ymax)
# crs        : +proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0 
# names      :    Jun,    Jul,    Aug,    Sep 
# min values :  5.945, 10.140, 12.525, 12.950 
# max values : 27.505, 29.780, 30.230, 30.150 
```

## Drawing rasters

There are lots of ways to draw a raster.  We show three simple ones in a [brief tutorial](https://github.com/BigelowLab/ohwobpg/blob/master/inst/tutorials/plotting_rasters.Rmd).  

> **Note** If you [clone](https://github.com/git-guides/git-clone) the package to your lcoal computer you can easily view these tutorials from within an RStudio session.

## Extracting data from a stack of rasters

Extracting from a stack at a point, a patch of points, or a polygon is very staright forward. See this [tutorial for an example](https://github.com/BigelowLab/ohwobpg/blob/master/inst/tutorials/extracting_rasters.Rmd).

## Deriving new rasters - raster math!

Creating a derived stack is easy with raster math - see this [page for an example](https://github.com/BigelowLab/ohwobpg/blob/master/inst/tutorials/deriving_rasters.md).

