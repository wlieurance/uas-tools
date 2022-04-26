#!/usr/bin/env Rscript 

# This script is meant to be run from the command line using the Rscript binary 
# which is distributed with R. The R/bin folder should be on $PATH in order to 
# be called from the command line. Some installations do this automatically and 
# some do not. For Windows environments, see: 
# https://stackoverflow.com/questions/9546324/adding-a-directory-to-the-path-environment-variable-in-windows


#' A script to auto-parse drone flight photos into spatial features using photo
#' EXIF metadata.
#'  
#' License: \tab GNU GENERAL PUBLIC LICENSE version 3 (GPLv3), dependencies have more 
#'     restrictive licensing.\cr
#' @author Wade Lieurance \email{wlieurance@@gmail.com}

suppressPackageStartupMessages(library(exifr))
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(sf))
suppressPackageStartupMessages(library(lubridate))
suppressPackageStartupMessages(library(tools))
suppressPackageStartupMessages(library(optparse))
suppressPackageStartupMessages(library(readr))
suppressPackageStartupMessages(library(ggplot2))
suppressPackageStartupMessages(library(viridis))
# used these in an attempt to get a static base map, with no luck thus far...
# library(maptiles)
# library(raster)
# library(rasterVis)
# library(ggmap)
# library(ggmaptile)

#' Determines the md5 hash of each photo.
#'
#' @param files string vector. A vector of full file paths to JPEGs which will 
#'     be hashed.
#'
#' @return tibble. Contains file names and md5 hashes.
#' @export
remove_dup_photos <-  function(files){
  md5hash <- md5sum(files) 
  file.df <- tibble(path = files, md5 = md5hash)
  file.clean <- file.df %>% group_by(md5) %>% arrange(path) %>%
    mutate(cnt = row_number()) %>% ungroup() %>%
    filter(cnt == 1) %>%
    select(-cnt)
  return(file.clean)
}

#' Takes raw metadata info, converts to a simple feature and identifies 
#' contiguous flights using provided parameters.
#'
#' @param dat tibble. Produced from the iomport function.
#' @param break_minutes double. The time in minutes between photos with the same 
#'     camera serial number that will constitue a break in flights.
#' @param crs integer. The SRID to use when creating geometry.
#' @param tz string. The timezone used in creating the date/time fields.
#'
#' @return sf POINTZ containing identified flight groupings of points.
#' @export
populate_space_time <- function(dat, break_minutes = NULL,
                                crs = 4326, tz = "UTC", tz_new = NULL){
  df <- dat %>%
    mutate(dt = as_datetime(DateTimeOriginal, tz = tz)) %>%
    rename(x = GPSLongitude, y = GPSLatitude, z = GPSAltitude) %>%
    filter(!is.na(x) & !is.na(y) & !is.na(z)) %>%
    st_as_sf(coords=c("x", "y", "z"), 
             crs = crs, agr = "constant", remove = FALSE)

  if (!is.null(tz_new)){
    df <- df %>%
      mutate(dt_orig = dt) %>%
      mutate(dt = lubridate::with_tz(dt, tz_new))
  }
  
  df.p <- df %>% 
    filter(x != 0 & y != 0 & z != 0) %>%
    group_by(SerialNumber) %>%
    mutate(prev_dt = lag(dt, order_by = dt)) %>%
    mutate(prev_delta = dt - prev_dt, grp = NA) %>% ungroup() %>%
    arrange(SerialNumber, dt)
  
  # iterates through rows, assigning a new group if it finds a new serial #
  # or a time break > time_break
  if (!is.null(break_minutes)){
    g <-  1
    df.p[1, "grp"] <-  g
    for (row in 2:nrow(df.p)){
      delta <- df.p[[row, "prev_delta"]]
      prev.id <- df.p[[row - 1, "SerialNumber"]]
      id <- prev.id <- df.p[[row, "SerialNumber"]]
      if (id != prev.id | is.na(delta)){
        g <- 1
      } else if (delta > minutes(break_minutes) & id == prev.id){
        g <- g + 1
      }
      df.p[row, "grp"] <- g
    }
  } else {
    df.p <- df.p %>%
      mutate(grp = 1)
  }
  return(df.p)
}

#' Converts identified flight points into lines.
#'
#' @param df sf POINTZ. Produced by the populate_space_time function.
#' @param min.points integer. The minimum number of consecutive points 
#'     constituting a flight.
#'
#' @return list of simple features.
#' @export
points_to_lines <-  function(df, min.points = 10){
  grps <- df %>% 
    group_by(SerialNumber, grp) %>%
    summarise(n = n(), do_union = FALSE, start_dt = min(dt), 
              .groups = "drop") %>%
    st_cast("LINESTRING") %>% 
    mutate(break_no = row_number(),
           flight_date = as_date(start_dt))
  
  lins <- grps %>% filter(n >= min.points) %>%
    arrange(start_dt) %>%
    mutate(flight_no = row_number())
  
  return(list(line = lins, full = grps))
}

#' Joins flight numbers and details back to the original points tibble.
#'
#' @param lines sf LINESTRINGZ. Produced by the points_to_lines function.
#' @param df sf POINTZ. Produced by the populate_space_time function.
#'
#' @return sf POINTZ with flight info.
#' @export
join_id_point <- function(lines, df){
  lines.ng <- lines %>%
    st_drop_geometry() %>%
    select(SerialNumber, grp, n, flight_no, start_dt, flight_date) %>%
    as_tibble()
  df2 <- df %>%
    left_join(lines.ng, 
              by = c("SerialNumber" = "SerialNumber", "grp" = "grp"))
  return(df2)
}

#' The main import function for scanning and reading metadata for photos.
#'
#' @param path string. Path to a folder to scan for photos or a csv 
#'     metadata file previously generated with the --raw_meta_path option.
#' @param rm boolean. If TRUE, the md5 hashes will be calculated for each 
#'     photo and used to identify duplicates. This increases script run time 
#'     significantly for larger photo sets so it is best to scan in a folder 
#'     where duplicates do not exist if their presence is not desired.
#' @param rec boolean. Should the photo scanning be done recursively.
#' @param raw string. A file path (e.g. my/file/path.csv) which will 
#'     be used to write the intermediate EXIF data and (optionally) md5 hash 
#'     data which can be passed into the 'scan_path' argument for quicker script
#'     runs with the same photo set.
#'
#' @return A tibble with selected metadata and (optionally) md5 hashes.
#' @export
import <-  function(path, rm = F, rec = F, raw = NULL, 
                    pattern = "(?i)\\.(?:JPE?G)|(?:TIFF?)"){
  if (dir.exists(path)){
    files <- list.files(path = path, pattern = pattern, 
                        recursive = rec, full.names= TRUE)
    if (length(files) > 0){
      cat(paste0(length(files), " files found.\n"))
    } else {
      cat(paste0("Could not find any files with regex pattern: ", 
                 pattern, "\n"))
      cat("Quitting...\n")
      quit()
    }
    # detect and remove duplicates
    if (rm == TRUE){
      print("Removing duplicate photos...")
      df <- remove_dup_photos(files)
    } else {
      df <- tibble(path = files)
    }
    print("Reading EXIF metadata from photos...")
    dat <- as_tibble(read_exif(df$path))
    
    # join md5hash for later use if we have it
    if (rm == TRUE){
      dat <-  dat %>%
        left_join(df, by=c("SourceFile" = "path"))
    } else {
      dat <-  dat %>% mutate(md5 = NA_character_)
    }
    
    if (!is.null(raw)){
      print(paste("Writing raw metadata to", raw))
      write_csv(dat, file = raw)
    }
  } else if (file.exists(path)){
    print("Reading csv...")
    dat <- read_csv(path, show_col_types = FALSE)
  } else {
    print("scan_path is neither a directory or a file. Quitting...")
    quit()
  }
  
  # clean up our data frame
  dat2 <- dat %>% 
    select(SourceFile,Directory, DateTimeOriginal, GPSLongitude, GPSLatitude, 
           GPSAltitude,GPSPosition, SerialNumber, md5)
  return(dat2)
}


#' Writes flight line features as well as point features for each photo,
#' whether in a flight or not. 
#'
#' @param path string. A path to a folder to write ESRI shapefiles for 
#'     processed point and line features. A path to a file (e.g. file.gkpg) is
#'     also allowed, where st_write() will guess the dsn driver.
#' @param line_feat sf LINESTRINGZ. Produced by the points_to_lines function.
#' @param point_feat sf POINTZ. Produced by the join_id_point function.
#'
#' @return None
#' @export
write_geo <- function(path, line_feat, point_feat){
  if (dir.exists(path)){
    suppressWarnings(
    st_write(line_feat, path, layer = "lines", 
             delete_layer = TRUE, driver = "ESRI Shapefile", 
             layer_options = "SHPT=ARCZ", quiet = TRUE, delete_dsn = TRUE))
    suppressWarnings(
    st_write(point_feat, path, layer = "points", 
             delete_layer = TRUE, driver = "ESRI Shapefile", 
             layer_options = "SHPT=POINTZ", quiet = TRUE, delete_dsn = TRUE))
  } else {
    suppressWarnings(
    st_write(line_feat, path, layer = "lines", delete_layer = TRUE, 
             quiet = TRUE))
    suppressWarnings(
    st_write(point_feat, path, layer = "points", delete_layer = TRUE, 
             quiet = TRUE))
  }
}

#' Function to create a shell script for each flight which will allow the user
#' to run the script to copy all photos associated with a flight to a new 
#' folder.
#'
#' @param path string. The directory path to write a copy script for each 
#'     flight to make photo copying for desired flights easier.
#' @param copy string. The directory path that photos should be copied to if
#'     using the script_dir option.
#' @param df sf POINTZ. Produced by the join_id_point function.
#' @param lines sf LINESTRINGZ. Produced by the points_to_lines function.
#'
#' @return None
#' @export
create_copy_scripts <- function(path, copy, df, lines){
  df.ng <- df %>% st_drop_geometry() %>% as_tibble()
  lines.ng <- lines %>% st_drop_geometry() %>% as_tibble() %>%
    select(-n)
  flight.df <- df.ng %>% 
    group_by(flight_no) %>%
    summarize(n = n(), .groups = "drop") %>%
    inner_join(lines.ng, by=c("flight_no" = "flight_no"))
  for (i in rownames(flight.df)){
    id = flight.df[i,]$flight_no
    new.df <- filter(df.ng, flight_no == id)
    fpath <- file.path(path, paste0("flight_", id, ".sh"))
    fcon <- file(fpath, open = "w")
    writeLines("#!/usr/bin/env bash", fcon)
    for (j in rownames(new.df)){
      # print(text)
      ts <- format(new.df[j,]$dt, format="%Y%m%d_%H%M%S")
      source <- new.df[j,]$SourceFile
      base <- basename(source)
      ext <- file_ext(base)
      name <- file_path_sans_ext(base)
      base_new <- paste0(ts, "_", name, ".", ext) 
      text <-  paste0("cp ", new.df[j,]$SourceFile, " ", copy, "/", base_new)
      writeLines(text, fcon)
    }
    close(fcon)
  }
}

#' Function which plots the flights processed from the photo EXIF data.
#'
#' @param lines sf LINESTRINGZ. Produced by the points_to_lines function.
#' @param out string. A file path (e.g. /my/picture/path.png) to which a plot
#'    of the flights will be written.
#'
#' @return None
#' @export
plot_flights <- function(lines, out){
  # bbox <- st_bbox(lines)
  # names(bbox) <- c("left", "bottom", "right", "top")
  # center <- c(long = mean(bbox[1],bbox[3]), lat = mean(bbox[2],bbox[4]))
  # base <- get_tiles(lines, crop = TRUE, zoom = 17, provider = "OpenTopoMap")
  if (dir.exists(out)){
    out <- file.path(out, "plot.png")
  }
  plt <- ggplot() + 
    geom_sf(data = lines, aes(color = flight_no)) +
    scale_color_viridis_c() +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
    # mapview() +
    facet_wrap(~flight_no)
  ggsave(filename = out, plot = plt, width = 10, height = 10, units = "in")
}

#' Main processing function for the script.
#'
#' @param scan_path string. Path to a folder to scan for photos or a csv 
#'     metadata file previously generated with the --raw_meta_path option.
#' @param geo_path string. A path to a folder to write ESRI shapefiles for 
#'     processed point and line features. A path to a file (e.g. file.gkpg) is
#'     also allowed, where st_write() will guess the dsn driver. 
#' @param time_break  double. The time in minutes between photos with the same 
#'     camera serial number that will constitue a break in flights.
#' @param min_pts integer. The minimum number of consecutive points 
#'     constituting a flight.
#' @param rm_dups boolean. If TRUE, the md5 hashes will be calculated for each 
#'     photo and used to identify duplicates. This increases script run time 
#'     significantly for larger photo sets so it is best to scan in a folder 
#'     where duplicates do not exist if their presence is not desired.
#' @param recursive boolean. Should the photo scanning be done recursively.
#' @param script_dir string. The directory path to write a copy script for each 
#'     flight to make photo copying for desired flights easier.
#' @param copy_dir string. The directory path that photos should be copied to if
#'     using the script_dir option.
#' @param raw_meta_path string. A file path (e.g. my/file/path.csv) which will 
#'     be used to write the intermediate EXIF data and (optionally) md5 hash 
#'     data which can be passed into the 'scan_path' argument for quicker script
#'     runs with the same photo set.
#' @param plot string. A file path (e.g. /my/picture/path.png) to which a plot
#'    of the flights will be written.
#'
#' @return None.
#' @export
main <- function(scan_path, geo_path, time_break = NULL, min_pts = 3, 
                 rm_dups = FALSE, recursive = TRUE, script_dir = NULL, 
                 copy_dir = NULL, raw_meta_path = NULL, plot = NULL, 
                 tz = "UTC", tz_new = NULL){
  print("Reading data...")
  dat <- import(path = scan_path, rm = rm_dups, rec = recursive, 
                raw = raw_meta_path)
  print("Populating space and time attributes...")  
  df.s <- populate_space_time(dat = dat, break_minutes = time_break, tz = tz,
                              tz_new = tz_new)
  feat.list <- points_to_lines(df = df.s)
  df.j <- join_id_point(lines = feat.list$line, df = df.s)
  print("Writing spatial features...")
  write_geo(path = geo_path, line_feat = feat.list$line, point_feat = df.j)
  if(!is.null(script_dir) & !is.null(copy_dir)){
    print("Writing copy shell scripts...")
    create_copy_scripts(path = script_dir, copy = copy_dir, df = df.j, 
                        lines = feat.list$line)
  }
  if(!is.null(plot)){
    print(paste0("Plotting flights to ", plot))
    plot_flights(lines = feat.list$line, out = plot)
  }
  print("Script finished.")
}

# runs if called from Rscript on command line
if (sys.nframe() == 0){
  args = commandArgs(trailingOnly = TRUE)
  
  parser <- OptionParser(formatter = IndentedHelpFormatter,
                         usage = "usage: %prog [options] scan_path out_path",
                         description = "Searches 'scan_path' for JPEG files and
                         constructs flight features based on date, time, and 
                         camera serial number, and outputs spatial features to
                         out_path (directory for shapefiles).")
  parser <- add_option(parser, c("-b", "--time_break"), type = "double", 
                       help = "number of minutes that must occur between photos 
                       in order to break them up into separate flights.")
  parser <- add_option(parser, c("-n", "--min_point_no"), type = "integer",
                       help = "the minimum number of concurrent photos to be 
                       considered a flight.", default = 3)
  parser <- add_option(parser, c("-d", "--rm_dups"), action = "store_true",
                       default = FALSE,
                       help = "remove duplicate photos (can take some time).")
  parser <- add_option(parser, c("-r", "--recursive"), action = "store_true",
                       default = FALSE,
                       help = "scan recursively for photos in scan_path.")
  parser <- add_option(parser, c("-s", "--script_dir"), 
                       help = "The directory to store copy shell scripts for each 
                       flight (for easy copying for individual flights).")
  parser <- add_option(parser, c("-l", "--script_copy_dir"), 
                       help = "The directory to which scripts will copy photos.")
  parser <- add_option(parser, c("-m", "--raw_meta_path"), 
                       help = "The (.csv) file path to write the raw photo metadata 
                       before processing. This file can be passed to 'scan_path'
                       as an alternative to scanning a directory for photos.")
  parser <- add_option(parser, c("-p", "--plot_path"), 
                       help = "path to save a plot of the flights")
  parser <- add_option(parser, c("-t", "--tz"), default = "UTC",
                       help = paste0("Time zone of the date time data captured",
                                     " in the EXIF metadata (See R ",
                                     "OlsonNames() function for valid options."))
  parser <- add_option(parser, c("-T", "--tz_new"),
                       help = paste0("Timezone used in writing new filenames ",
                                     "for copying. See R OlsonNames() function",
                                     " for valid options."))
  
  opt <- parse_args(object = parser, args = args, positional_arguments = 2)
  
  # checking argument sanity
  if (!dir.exists(opt$args[1]) & !file.exists(opt$args[1])){
    print(opt$args[1], "does not exist. Quitting...")
    quit()
  }
  if (!is.null(opt$options$script_dir)){
    if (!dir.exists(opt$options$script_dir)){
      print(opt$options$script_dir, "directory does not exist. Quitting...")
      quit()
    }
  }
  if (!is.null(opt$options$raw_meta_path)){
    if (opt$options$raw_meta_path == opt$args[1]){
      print("raw_meta_path and scan_path cannot be the same. Quitting..." )
      quit()
    }
  }

  main(scan_path = opt$args[1], 
       geo_path = opt$args[2], 
       time_break = opt$options$time_break,
       min_pts = opt$options$min_point_no,
       rm_dups = opt$options$rm_dups, 
       recursive = opt$options$recursive,
       script_dir = opt$options$script_dir, 
       copy_dir = opt$options$script_copy_dir,
       raw_meta_path = opt$options$raw_meta_path,
       plot = opt$options$plot_path,
       tz = opt$options$tz,
       tz_new = opt$options$tz_new)
}

