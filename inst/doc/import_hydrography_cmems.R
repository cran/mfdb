## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)

## ---- message=FALSE, echo=FALSE-----------------------------------------------
library(unittest)
# Redirect ok() output to stderr
ok <- function(...) capture.output(unittest::ok(...), file = stderr())

## ---- message=FALSE-----------------------------------------------------------
library(mfdb)

## ---- message=FALSE, echo=FALSE-----------------------------------------------
# Don't show chatty MFDB messages
logging::setLevel('WARN')

## ----eval=FALSE---------------------------------------------------------------
#  library(RCMEMS)
#  
#  # point to the data product
#  cfg <- CMEMS.config(motu="http://my.cmems-du.eu/motu-web/Motu",
#                           service.id = "BALTICSEA_REANALYSIS_PHY_003_011-TDS",
#                           product.id = "dataset-reanalysis-nemo-monthlymeans",
#                           variable = c("bottomT"))
#  
#  # add user and psw
#  CMEMS.config.usr <- function(x){
#      print("username")
#      scan("", what="character", nmax=1, quiet=T)
#      }
#  CMEMS.config.pwd <- function(x){
#      print("password")
#      scan("", what="character", nmax=1, quiet=T)
#      }
#  cfg@user <- CMEMS.config.usr()
#  cfg@pwd  <- CMEMS.config.pwd()
#  
#  # select year and download
#  y <- 2018 # year of interest
#  CMEMS.download(cfg,
#                     ROI = c(17,20,56,58.5),
#                     date.range = c(ISOdate(y,01,01), ISOdate(y,12,31)),
#                     depth.range= c(10,500), # max depth Baltic 459 m
#                     out.path=paste("data_provided/CMEMS_BAL_PHY_reanalysis_monthlymeans_",y,".nc",sep=""),
#                 debug=FALSE)

## ----eval=FALSE---------------------------------------------------------------
#  temp <- tempfile()
#  download.file(url="https://gis.ices.dk/shapefiles/ICES_StatRec_mapto_ICES_Areas.zip", temp)
#  unzip(temp, exdir="data_provided/ices_rect")
#  unlink(temp)

## ----eval=FALSE---------------------------------------------------------------
#  library(sf)
#  library(raster)
#  library(tidyverse)
#  library(rnaturalearth)
#  library(lwgeom)
#  library(ggplot2)
#  
#  y <- 2018 # year of interest
#  
#  # load ICES rect
#  ices.rect <- st_read("data_provided/ices_rect/StatRec_map_Areas_Full_20170124.dbf", quiet = T)
#  
#  # load bottom temperature (bottomT)
#  rst <- brick(paste("data_provided/CMEMS_BAL_PHY_reanalysis_monthlymeans_",y,".nc",sep=""), varname="bottomT", lvar=4)
#  
#  # calculate mean sob (raster) by ices rect (polygons)
#  ov <- raster::extract(rst,ices.rect, fun=mean, na.rm=T, df=T)
#  hydroVar <- data.frame(ID=1:length(ices.rect$ICESNAME),
#                         rect=ices.rect$ICESNAME,
#  					   areaFull_km2=ices.rect$AREA_KM2,
#  					   year=y) %>%
#      right_join(ov) %>%
#      select(-ID) %>%
#      gather("month","sob",4:15) %>%
#      mutate(month = as.numeric(substring(month,7,8))) %>%
#      subset(!is.na(sob)) # NA are on land or outside the raster area
#  
#  land <- rnaturalearth::ne_countries(returnclass = "sf", continent="Europe", scale="large") %>%
#    st_union()
#  ices.rect.sea <- ices.rect %>% filter(ICESNAME %in% hydroVar$rect) %>%
#           st_difference(land)
#  ices.rect.sea$areaSea_km2 <- st_area(ices.rect.sea) %>% units::set_units(km^2)
#  
#  hydroVar <- ices.rect.sea %>%
#      rename(rect=ICESNAME) %>%
#      select(rect, areaSea_km2) %>%
#      right_join(hydroVar)
#  
#  ggplot(hydroVar) +
#      geom_sf() +
#      geom_sf(data=st_geometry(ices.rect.sea), fill="lightblue") +
#      xlim(xmin(extent(hydroVar)), xmax(extent(hydroVar))) +
#      ylim(ymin(extent(hydroVar)), ymax(extent(hydroVar))) +
#      theme_bw()	
#  
#  # Write the results to temporary files so we can read them back in the next step
#  
#  write.table(sf::st_drop_geometry(ices.rect.sea), file = 'ices.rect.sea.txt')
#  write.table(sf::st_drop_geometry(hydroVar), file = 'hydroVar.txt')

## -----------------------------------------------------------------------------
mdb <- mfdb(tempfile(fileext = '.duckdb'))

ices.rect.sea <- read.table('ices.rect.sea.txt')
hydroVar <- read.table('hydroVar.txt')

# import ICES rectangles
mfdb_import_area(mdb, data.frame(
    name = as.character(ices.rect.sea$ICESNAME),
    size = ices.rect.sea$areaSea_km2))

# import area size as index for later use
mfdb_import_cs_taxonomy(mdb, "index_type", data.frame(name=c("ices_rect", "bottom_temp")))
mfdb_import_survey_index(mdb,
                         data_source="area_ices_rect",
                         data.frame(index_type = "ices_rect",
                                        year       = hydroVar$year,
                                        month      = hydroVar$month,
                                        areacell   = as.character(hydroVar$rect),
                                        value      = as.numeric(hydroVar$areaSea_km2)))

# import avg bottom temperature by month and ICES rect (under the 'length' field)
mfdb_import_survey_index(mdb,
                   data_source="bottom_temp_CMEMS",
                   data.frame(year       = hydroVar$year,
                              month      = hydroVar$month,
                              areacell   = as.character(hydroVar$rect),
                              value     = hydroVar$sob,
                              index_type = "bottom_temp",
                              stringsAsFactors = TRUE))

# extract mean bottom temperature by quarter and area (weighted mean using rectangle area)
dat <- mfdb_survey_index_mean(mdb, c(), list(
                                       ## area = mfdb_unaggregated(),
                                       area = mfdb_group('a1'=c('42G7','42G8','42G9','43G7','43G8','43G9'),
                                                         'a2'=c('44G7','44G8','44G9','45G7','45G8','45G9')),
                                       timestep = mfdb_timestep_quarterly,
                                       year = 2018,
                                       index_type = "bottom_temp",
                                       data_source = 'bottom_temp_CMEMS'),
                              scale_index = 'area_size')[[1]]
dat <- dat[,c('year', 'step', 'area', 'mean')]
dat

## ---- echo=FALSE--------------------------------------------------------------
# NB: Calculated with:
# raw <- hydroVar[hydroVar$year == 2018 & hydroVar$rect %in% c('44G7','44G8','44G9','45G7','45G8','45G9') & hydroVar$month %in% c(1,2,3),]
# sum(raw$sob * raw$areaSea_km2) / sum(raw$areaSea_km2)
ok(ut_cmp_equal(dat, read.table(blank.lines.skip = TRUE, header = TRUE, stringsAsFactors = FALSE, text = '
year step area     mean
2018    1   a1 5.131353
2018    1   a2 5.286023
2018    2   a1 5.772875
2018    2   a2 5.787779
2018    3   a1 7.259383
2018    3   a2 6.609905
2018    4   a1 6.144196
2018    4   a2 6.020029
', colClasses = c(NA, 'character', NA, NA)), tolerance = 1e-5), "extract mean bottom temperature by quarter and area")

## -----------------------------------------------------------------------------
mfdb_disconnect(mdb)

