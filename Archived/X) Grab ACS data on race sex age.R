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
        

#2) Define geos to search by, states to look up, and years to search
    geos <- c("county", "zcta")
    years <- c(2009:2017)
    l.county <- list()
    l.zip <- list()
    
#3) Grab all census data
    for(i in 1:length(geos)){
      for(k in 1:length(years)){
        #a) assign geo and year to pull
            t.geo <- geos[i]  
            t.year <- years[k]
            
        #b) skip if geo is ZCTAs and year is prior to 2011
            if(t.geo == "zcta" & t.year < 2011){ print(paste(i,k)); next() }
          
        #c) determine variables to pull and new names for variables
            census.vars <-c(
              white.male.0to5 = "B01001H_003",
              white.male.5to9 = "B01001H_004",
              white.male.10to14 = "B01001H_005",
              white.male.15to17 = "B01001H_006",
              white.male.18to19 = "B01001H_007",
              white.male.20to24 = "B01001H_008",
              white.male.25to29 = "B01001H_009",
              white.male.30to34 = "B01001H_010",
              white.male.35to44 = "B01001H_011",
              white.male.45to54 = "B01001H_012",
              white.male.55to64 = "B01001H_013",
              white.male.65to74 = "B01001H_014",
              white.male.75to84 = "B01001H_015",
              white.male.85P = "B01001H_016",
              white.female.0to5 = "B01001H_018",
              white.female.5to9 = "B01001H_019",
              white.female.10to14 = "B01001H_020",
              white.female.15to17 = "B01001H_021",
              white.female.18to19 = "B01001H_022",
              white.female.20to24 = "B01001H_023",
              white.female.25to29 = "B01001H_024",
              white.female.30to34 = "B01001H_025",
              white.female.35to44 = "B01001H_026",
              white.female.45to54 = "B01001H_027",
              white.female.55to64 = "B01001H_028",
              white.female.65to74 = "B01001H_029",
              white.female.75to84 = "B01001H_030",
              white.female.85P = "B01001H_031",

              black.male.0to5 = "B01001B_003",
              black.male.5to9 = "B01001B_004",
              black.male.10to14 = "B01001B_005",
              black.male.15to17 = "B01001B_006",
              black.male.18to19 = "B01001B_007",
              black.male.20to24 = "B01001B_008",
              black.male.25to29 = "B01001B_009",
              black.male.30to34 = "B01001B_010",
              black.male.35to44 = "B01001B_011",
              black.male.45to54 = "B01001B_012",
              black.male.55to64 = "B01001B_013",
              black.male.65to74 = "B01001B_014",
              black.male.75to84 = "B01001B_015",
              black.male.85P = "B01001B_016",
              black.female.0to5 = "B01001B_018",
              black.female.5to9 = "B01001B_019",
              black.female.10to14 = "B01001B_020",
              black.female.15to17 = "B01001B_021",
              black.female.18to19 = "B01001B_022",
              black.female.20to24 = "B01001B_023",
              black.female.25to29 = "B01001B_024",
              black.female.30to34 = "B01001B_025",
              black.female.35to44 = "B01001B_026",
              black.female.45to54 = "B01001B_027",
              black.female.55to64 = "B01001B_028",
              black.female.65to74 = "B01001B_029",
              black.female.75to84 = "B01001B_030",
              black.female.85P = "B01001B_031",
              
              native.male.0to5 = "B01001C_003",
              native.male.5to9 = "B01001C_004",
              native.male.10to14 = "B01001C_005",
              native.male.15to17 = "B01001C_006",
              native.male.18to19 = "B01001C_007",
              native.male.20to24 = "B01001C_008",
              native.male.25to29 = "B01001C_009",
              native.male.30to34 = "B01001C_010",
              native.male.35to44 = "B01001C_011",
              native.male.45to54 = "B01001C_012",
              native.male.55to64 = "B01001C_013",
              native.male.65to74 = "B01001C_014",
              native.male.75to84 = "B01001C_015",
              native.male.85P = "B01001C_016",
              native.female.0to5 = "B01001C_018",
              native.female.5to9 = "B01001C_019",
              native.female.10to14 = "B01001C_020",
              native.female.15to17 = "B01001C_021",
              native.female.18to19 = "B01001C_022",
              native.female.20to24 = "B01001C_023",
              native.female.25to29 = "B01001C_024",
              native.female.30to34 = "B01001C_025",
              native.female.35to44 = "B01001C_026",
              native.female.45to54 = "B01001C_027",
              native.female.55to64 = "B01001C_028",
              native.female.65to74 = "B01001C_029",
              native.female.75to84 = "B01001C_030",
              native.female.85P = "B01001C_031",
              
              asian.male.0to5 = "B01001D_003",
              asian.male.5to9 = "B01001D_004",
              asian.male.10to14 = "B01001D_005",
              asian.male.15to17 = "B01001D_006",
              asian.male.18to19 = "B01001D_007",
              asian.male.20to24 = "B01001D_008",
              asian.male.25to29 = "B01001D_009",
              asian.male.30to34 = "B01001D_010",
              asian.male.35to44 = "B01001D_011",
              asian.male.45to54 = "B01001D_012",
              asian.male.55to64 = "B01001D_013",
              asian.male.65to74 = "B01001D_014",
              asian.male.75to84 = "B01001D_015",
              asian.male.85P = "B01001D_016",
              asian.female.0to5 = "B01001D_018",
              asian.female.5to9 = "B01001D_019",
              asian.female.10to14 = "B01001D_020",
              asian.female.15to17 = "B01001D_021",
              asian.female.18to19 = "B01001D_022",
              asian.female.20to24 = "B01001D_023",
              asian.female.25to29 = "B01001D_024",
              asian.female.30to34 = "B01001D_025",
              asian.female.35to44 = "B01001D_026",
              asian.female.45to54 = "B01001D_027",
              asian.female.55to64 = "B01001D_028",
              asian.female.65to74 = "B01001D_029",
              asian.female.75to84 = "B01001D_030",
              asian.female.85P = "B01001D_031",
              
              other1.male.0to5 = "B01001E_003",
              other1.male.5to9 = "B01001E_004",
              other1.male.10to14 = "B01001E_005",
              other1.male.15to17 = "B01001E_006",
              other1.male.18to19 = "B01001E_007",
              other1.male.20to24 = "B01001E_008",
              other1.male.25to29 = "B01001E_009",
              other1.male.30to34 = "B01001E_010",
              other1.male.35to44 = "B01001E_011",
              other1.male.45to54 = "B01001E_012",
              other1.male.55to64 = "B01001E_013",
              other1.male.65to74 = "B01001E_014",
              other1.male.75to84 = "B01001E_015",
              other1.male.85P = "B01001E_016",
              other1.female.0to5 = "B01001E_018",
              other1.female.5to9 = "B01001E_019",
              other1.female.10to14 = "B01001E_020",
              other1.female.15to17 = "B01001E_021",
              other1.female.18to19 = "B01001E_022",
              other1.female.20to24 = "B01001E_023",
              other1.female.25to29 = "B01001E_024",
              other1.female.30to34 = "B01001E_025",
              other1.female.35to44 = "B01001E_026",
              other1.female.45to54 = "B01001E_027",
              other1.female.55to64 = "B01001E_028",
              other1.female.65to74 = "B01001E_029",
              other1.female.75to84 = "B01001E_030",
              other1.female.85P = "B01001E_031",
              
              other2.male.0to5 = "B01001F_003",
              other2.male.5to9 = "B01001F_004",
              other2.male.10to14 = "B01001F_005",
              other2.male.15to17 = "B01001F_006",
              other2.male.18to19 = "B01001F_007",
              other2.male.20to24 = "B01001F_008",
              other2.male.25to29 = "B01001F_009",
              other2.male.30to34 = "B01001F_010",
              other2.male.35to44 = "B01001F_011",
              other2.male.45to54 = "B01001F_012",
              other2.male.55to64 = "B01001F_013",
              other2.male.65to74 = "B01001F_014",
              other2.male.75to84 = "B01001F_015",
              other2.male.85P = "B01001F_016",
              other2.female.0to5 = "B01001F_018",
              other2.female.5to9 = "B01001F_019",
              other2.female.10to14 = "B01001F_020",
              other2.female.15to17 = "B01001F_021",
              other2.female.18to19 = "B01001F_022",
              other2.female.20to24 = "B01001F_023",
              other2.female.25to29 = "B01001F_024",
              other2.female.30to34 = "B01001F_025",
              other2.female.35to44 = "B01001F_026",
              other2.female.45to54 = "B01001F_027",
              other2.female.55to64 = "B01001F_028",
              other2.female.65to74 = "B01001F_029",
              other2.female.75to84 = "B01001F_030",
              other2.female.85P = "B01001F_031",
              
              other3.male.0to5 = "B01001G_003",
              other3.male.5to9 = "B01001G_004",
              other3.male.10to14 = "B01001G_005",
              other3.male.15to17 = "B01001G_006",
              other3.male.18to19 = "B01001G_007",
              other3.male.20to24 = "B01001G_008",
              other3.male.25to29 = "B01001G_009",
              other3.male.30to34 = "B01001G_010",
              other3.male.35to44 = "B01001G_011",
              other3.male.45to54 = "B01001G_012",
              other3.male.55to64 = "B01001G_013",
              other3.male.65to74 = "B01001G_014",
              other3.male.75to84 = "B01001G_015",
              other3.male.85P = "B01001G_016",
              other3.female.0to5 = "B01001G_018",
              other3.female.5to9 = "B01001G_019",
              other3.female.10to14 = "B01001G_020",
              other3.female.15to17 = "B01001G_021",
              other3.female.18to19 = "B01001G_022",
              other3.female.20to24 = "B01001G_023",
              other3.female.25to29 = "B01001G_024",
              other3.female.30to34 = "B01001G_025",
              other3.female.35to44 = "B01001G_026",
              other3.female.45to54 = "B01001G_027",
              other3.female.55to64 = "B01001G_028",
              other3.female.65to74 = "B01001G_029",
              other3.female.75to84 = "B01001G_030",
              other3.female.85P = "B01001G_031",
              
              hispanic.male.0to5 = "B01001I_003",
              hispanic.male.5to9 = "B01001I_004",
              hispanic.male.10to14 = "B01001I_005",
              hispanic.male.15to17 = "B01001I_006",
              hispanic.male.18to19 = "B01001I_007",
              hispanic.male.20to24 = "B01001I_008",
              hispanic.male.25to29 = "B01001I_009",
              hispanic.male.30to34 = "B01001I_010",
              hispanic.male.35to44 = "B01001I_011",
              hispanic.male.45to54 = "B01001I_012",
              hispanic.male.55to64 = "B01001I_013",
              hispanic.male.65to74 = "B01001I_014",
              hispanic.male.75to84 = "B01001I_015",
              hispanic.male.85P = "B01001I_016",
              hispanic.female.0to5 = "B01001I_018",
              hispanic.female.5to9 = "B01001I_019",
              hispanic.female.10to14 = "B01001I_020",
              hispanic.female.15to17 = "B01001I_021",
              hispanic.female.18to19 = "B01001I_022",
              hispanic.female.20to24 = "B01001I_023",
              hispanic.female.25to29 = "B01001I_024",
              hispanic.female.30to34 = "B01001I_025",
              hispanic.female.35to44 = "B01001I_026",
              hispanic.female.45to54 = "B01001I_027",
              hispanic.female.55to64 = "B01001I_028",
              hispanic.female.65to74 = "B01001I_029",
              hispanic.female.75to84 = "B01001I_030",
              hispanic.female.85P = "B01001I_031"
            )
            
        #d) pull variables from census for geo and year 
            census_info <- get_acs(
              geography = t.geo,
              year = t.year,
              keep_geo_vars = F,
              geometry = F,
              key = mykey,
              output = "wide",
              cache_table = T,
              variables = census.vars
            )
            
        #e) remove useless info and format
            #i) remove margins of error
                tname <- names(census_info)
                badvars <- substr(tname, nchar(tname), nchar(tname)) == "M"
                census_info <- census_info[, !badvars]
            #ii) kill name
                census_info$NAME <- NULL
            #iii) remove E from end of all variable names
                names(census_info) <- gsub("E$", "", names(census_info))
            #iv) append other races
                tname <- names(census_info)
                other1 <- grepl("other1", tname)
                other2 <- grepl("other2", tname)
                other3 <- grepl("other3", tname)
                df.other <- census_info[, other1] + census_info[, other2] + census_info[, other3]
                names(df.other) <- gsub("other1", "other", names(df.other))
                census_info <- census_info[, !other1 & !other2 & !other3]
                census_info <- cbind(census_info, df.other)
            #v) convert to four columns
                census_info <- tidyr::gather(census_info, key = "varname", value = "pop", white.male.0to5:other.female.85P)
            #vi) convert to data.table
                census_info <- data.table(census_info)
            #vii) break out var name to three columns
                census_info[, c("race", "sex", "age") := tstrsplit(varname, "\\.")]
            #viii) add year
                census_info[, year := t.year]

        #f) append to list
            if(i == 1) l.county[[k]] <- census_info
            if(i == 2) l.zip[[k]] <- census_info

        #g) report progress
            print(paste(i,round(k/length(years), 2)))
      }
    }
     

#5) bind to dataframe
    df.county <- rbindlist(l.county)
    df.zip <- rbindlist(l.zip)
    
    
#5.5) remove varname    
    df.county[, varname := NULL]
    df.zip[, varname := NULL]

        
#6) save data
    write_fst(df.county, file.path(der.dir, "ACS.county.race.sex.age.fst"))
    write_fst(df.zip, file.path(der.dir, "ACS.zip.race.sex.age.fst"))

