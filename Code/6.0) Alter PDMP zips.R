#0) load libraries
    library(docshop)
    library(geosphere)
    #a) find main directory
        if(grepl("briarons",getwd())){
          main.dir<-"C:/Users/briarons/Desktop/Analysis - Data/Postdoc"
        }
        if(grepl("bda13",getwd())){
          main.dir<-"C:/Users/bda13/Desktop/Analysis - Data/Postdoc"
        }
    #b) find file paths
        pdmppath<-file.path(main.dir,"PDMP","pdmp.fst")
        ziphistpath<-file.path(main.dir,"Zip Hist","ziphist.fst")

#1) read data
    pdmp<-read.fst(pdmppath)
    ziphist<-read.fst(ziphistpath)
    zipzcta<-s3read_csv("hidden")
    
#2) deal with historical changes in zip codes for each dataset
    #a) match names of relevant variables
        names(ziphist)[1]<-"zipcode"
    #b) merge historical zips into data (use year 2012 for now)
        pdmp<-merge(pdmp,by=c("zipcode","year"),ziphist[,c("zipcode","year","Zip2017")],all.x=T)

#3) merge zctas into pdmp and dea
    #a) match names in zcta
        zipzcta<-zipzcta[,c("ZIP","ZCTA")]
        names(zipzcta)<-c("Zip2012","zcta")
    #b) merge zctas into data
        pdmp<-merge(pdmp,zipzcta,by="Zip2012",all.x=T)

#4) overwrite old files (new = old + 2 vars)
    write.fst(pdmp,pdmppath,compress = 100)

