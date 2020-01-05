#This script grabs ACS variables from ACS website. Notes:
#i)  Since census api calls are error prone (and randomly
#    it seems), I wrap everything in while and try syntax.
#    For this reason too, I grab years individually
#
#ii) The ACS variables are only first available in 2009.
#
#iii) In the variable searches, it is critical to identify
#    the "universe (specified via "u") that the variable
#    is drawn from, so that we can estimate the true fraction
#    of the population in that universe
#
#iv) zips not available prior to 2011

#0) Prepare workspace
    #a) load libraries
        library(tidycensus)
        library(data.table)
        library(fst)
        library(dplyr,  warn.conflicts = FALSE)
        library(tigris)
        library(stringr)
        library(sf)
        options(tigris_use_cache = TRUE)


    #b) set directories
        proj.dir <- file.path("/N", "project", "suicide_study", "data")
        raw.v2.dir <- file.path(proj.dir, "RawData_V2_fst")

    #c) set API key
        mykey <- "hidden"

    #d) find local directories (if doing on my PC)
        user <- ifelse(grepl("briarons",getwd()), "briarons",
                ifelse(grepl("bda13",getwd()), "bda13",
                "admin"))
        main.dir <- file.path("C:","Users",user,"Desktop","Analysis - Data","Postdoc")
        geo.dir <- file.path(main.dir, "ACS GEOS"); dir.create(geo.dir, showWarnings = F)
        raw.dir <- file.path(main.dir, "ACS raw vars"); dir.create(raw.dir, showWarnings = F)
        der.dir <- file.path(main.dir, "ACS derived vars"); dir.create(der.dir, showWarnings = F)


#1) load data
    l.geos <- readRDS(file.path(geo.dir, "l.df.geo.rds"))
    zips <- fread(file.path(geo.dir, "US Zip Codes from 2013 Government Data"))
    # df.geos2 <- read_sf(dsn = file.path(geo.dir, "ipums_puma_2010"), as_tibble = F)
        #raw data; does not seem properly formatted and cannot figur out how to change bounding box


#2) append/format each puma to a single dataframe
    l.geos <- lapply(l.geos, st_as_sf)
        #sf does not play nice with data.table, so can't use rbindlist
    for(i in 1:length(l.geos)){
        if(i == 1) df.geos <- l.geos[[i]]
        if(i > 1) df.geos <- rbind(df.geos, l.geos[[i]])
        print(i)
    }

    
#3) Format zip codes for doing geo lookups
    zip.pts <- data.frame(y = zips$LAT[1:1000], x = zips$LNG[1:1000])
    zip.pts <- st_as_sf(zip.pts, coords = c("x", "y"), crs = st_crs(df.geos))


#4) Determine which zips are in which PUMAs
    matches <- as.numeric(st_within(zip.pts, df.geos)) 
