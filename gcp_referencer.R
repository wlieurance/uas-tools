library(imager)
library(zoom)
library(sf)
library(nngeo)

source("flight_autoextract.R")

scan_dir <- "/mnt/windows/Users/wlieurance/Documents/odm/datasets/2015040826001-2/images/"
dsn = "/mnt/windows/Users/wlieurance/Documents/odm/jl_flight2/trimble/Export/gcp.gpkg"
layer = "flight1"

gcps <- st_read(dsn = dsn, layer = layer)

pattern = "(?i)_1\\.(?:(?:JPE?G)|(?:TIFF?))$"
# files <- list.files(path = scan_dir, pattern = pattern, 
#                     recursive = FALSE, full.names= FALSE)

dat <- import(path = scan_dir, pattern = pattern)
df.s <- populate_space_time(dat = dat, break_minutes = NULL, tz = "UTC",
                                      tz_new = "America/Los_Angeles")

near <- st_nn(x = gcps, y = df.s, k = 10, returnDist = T)

gcps[1,]
near$dist[[1]]
near.df <- df.s[near$nn[[1]],] 
near.df$SourceFile




