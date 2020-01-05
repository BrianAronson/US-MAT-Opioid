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
    #b) set directory to zip folder
        ziphist.dir<-file.path(main.dir,"Zip Hist")
        state.dir<-file.path(ziphist.dir,"States")
    #c) create directory
        dir.create(ziphist.dir, showWarnings = FALSE)
        dir.create(state.dir, showWarnings = FALSE)

#1) load data
    tdir<-"hidden"
    allzipdf<-s3read_csv(paste(tdir,"SAS_zipcodes.csv",sep=""))
    
#2) remove quarters, non zipcodes, and split data by state (zips can't cross states)
    allzipdf<-allzipdf[(allzipdf$quarter==1) | (allzipdf$year==2008 & allzipdf$quarter==4),]
    allzipdf<-allzipdf[allzipdf$file=="zipcode"]
    allzipdf<-allzipdf[!is.na(allzipdf$Y)]
    #kill PW because it causes errors
    allzipdf<-allzipdf[allzipdf$STATECODE!="PW",]
    
    lallzipdf<-split(allzipdf,by="STATECODE")
    
    
#2.5) For every state in lallzipdf, do the following:
    for(k in 1:length(lallzipdf)){    
        #a) assign tallzipdf to element of list
            tallzipdf<-lallzipdf[[k]]
    #3) determine distance between each zip code in dataset
        #a) grab geocoordinates
            geoall<-tallzipdf[,c("X","Y")]
        #b) calculate distance between all coordinates
            system.time(temp<-c(distm(geoall,fun=distCosine)))
        #c) convert to miles
            Dist<-temp*0.000621371
            rm(geoall)
            rm(temp)
    
    #4) optional: weight zip codes by shared city (to account for error) and same zip (to break ties)
        # samecity<-rep(tallzipdf$CITY,each=length(tallzipdf$CITY))==rep(tallzipdf$CITY,length(tallzipdf$CITY))
        # samezip<-rep(tallzipdf$ZIP,each=length(tallzipdf$ZIP))==rep(tallzipdf$ZIP,length(tallzipdf$ZIP))
        # Dist<-ifelse(!samecity,Dist+1,Dist)
        # Dist<-ifelse(!samezip,Dist+.1,Dist)
    
    #5) for each year of sender city and each year of receiver city, pull out closest city
        #a) create matrix of distances
            Distmat<-matrix(Dist,nrow=sqrt(length(Dist)))
            year<-tallzipdf$year
            zip<-tallzipdf$ZIP
            uyear<-unique(year)
            zipindex<-data.table(matrix(nrow=nrow(tallzipdf),ncol=length(uyear)))
        #b) slice matrix by year
            for(i in 1:length(uyear)){
                tDistmat<-Distmat[,year==uyear[i]]
                tclose<-unlist(apply(tDistmat,1, function(x) which(x==min(x))[1]))
                zipindex[,i]<-zip[year==uyear[i]][tclose]
            }
            rm(Distmat)
            rm(Dist)
        #c) name columns and bring in zip and year indices
            names(zipindex)<-paste("Zip",uyear[1:length(uyear)],sep="")
            zipindex<-cbind(tallzipdf[,c("ZIP","year")], zipindex)
        #d) check how many zips never change
            # table(apply(zipindex[,3:14],1,function(x) length(unique(x))==1))
    
    #6) fill in years where zips don't exist but probably should
        a<-data.table(table(zipindex$ZIP))
        probs<-a$V1[a$N!=max(a$N)]
        for(i in 1:length(probs)){
          tzipindex<-zipindex[zipindex$ZIP==probs[i],]
          missyear<-!(uyear %in% tzipindex$year)
          fillyears<-uyear[missyear]
          for(j in 1:length(fillyears)){
            dif<-abs(fillyears[j]-uyear[!missyear])
            ind<-which(dif==min(dif))[1]
            trow<-tzipindex[ind,]
            trow$year<-fillyears[j]
            zipindex<-rbind(zipindex,trow)
          }
        }
        zipindex<-zipindex[order(zipindex$year,zipindex$ZIP),]
        zipindex<-zipindex[!duplicated(paste(zipindex$ZIP,zipindex$year)),]
        zipindex<-zipindex[!is.na(zipindex$ZIP),]
    
    #7) Save resulting table
        write.fst(zipindex,file.path(state.dir,paste("zipindex",tallzipdf$STATECODE[1],".fst",sep="")),compress = 100)
        print(k)
        gc()
}
 
#8) append all zipindex files
    indices<-list.files(state.dir)
    ldf<-list()
    for(i in 1:length(indices)){
      ldf[[i]]<-read.fst(file.path(state.dir,indices[i]))
    }
    #kick out two small fake states with fewer than 13 columns
        ldf<-ldf[!sapply(ldf,ncol)!=max(sapply(ldf,ncol))]
        df<-do.call(rbind,ldf)

#9) save full index
    write.fst(df,file.path(ziphist.dir,"ziphist.fst"),compress = 100)

