#0 - prep work
    library(docshop)
    #a) find main directory
        if(grepl("briarons",getwd())){
          main.dir<-"C:/Users/briarons/Desktop/Analysis - Data/Postdoc"
        }
        if(grepl("bda13",getwd())){
          main.dir<-"C:/Users/bda13/Desktop/Analysis - Data/Postdoc"
        }
    #b) set directory to DEA files
        DEA.dir<-file.path(main.dir,"DEA")
    #c) set directories for below functions
        base.dir<-file.path(DEA.dir,"0 - basedata")
        chunk.dir<-file.path(DEA.dir,"1 - chunked files")
        state.dir<-file.path(DEA.dir,"2 - state files")
        zip.dir<-file.path(DEA.dir,"3 - zip files")
        county.dir<-file.path(DEA.dir,"4 - county files")
    #d) create all directories
        dir.create(DEA.dir, showWarnings = FALSE)
        dir.create(base.dir, showWarnings = FALSE)
        dir.create(chunk.dir, showWarnings = FALSE)
        dir.create(state.dir, showWarnings = FALSE)
        dir.create(zip.dir, showWarnings = FALSE)
        dir.create(county.dir, showWarnings = FALSE)
        
#1 - save info into chunks
    #a) connext to data
        con<-gzfile(file.path(base.dir,"arcos_all_washpost.tsv.gz"),open = "rt")
    #b) prep variables
        ind.df<-vector()
        j<-0
        chunk<-1000000
        tlines<-chunk
    #c) prep save info
        dir.create("fstfiles", showWarnings = FALSE)
        savenum<-1
    #d) prep variable indices
        rnames<-readLines(con,1)
        rnames<-unlist(strsplit(rnames,"\t"))
    #e) pull data
        while(tlines==chunk){
            #read data
                ind.df<-fread(text=readLines(con,chunk))
                tlines<-nrow(ind.df)
            #keep useful stuff
                names(ind.df)<-rnames
                keeps<-!grepl("REPORTER",rnames) & !rnames %in% c("BUYER_DEA_NO","Combined_Labeler_Name","Revised_Company_Name","TRANSACTION_ID","CORRECTION_NO","UNIT","ACTION_INDICATOR","ORDER_FORM_NO")
                ind.df<-ind.df[,keeps,with=FALSE]
            #split by state
                l.ind.df<-split(ind.df,by="BUYER_STATE")
            #save each state into separate files
                for(i in 1:length(l.ind.df)){
                    filename<-file.path(chunk.dir,paste(names(l.ind.df)[i],"-",savenum,".fst",sep=""))
                    tdf<-l.ind.df[[i]]
                    write.fst(tdf,filename,compress=100)        
                }
                savenum<-1+savenum
                j<-j+1
                print(j)
                gc()
        }
        
#2) aggregate chunks by state
    setwd(chunk.dir)
    #a) find file names
        files<-sort(list.files())
        unfileabr<-unique(substr(files,1,2))
    #b) for each state
        for(i in 1:length(unfileabr)){
          #find files within state
              tfiles<-list.files(pattern = unfileabr[i])
          #prep list
              tlist<-list()
          #for each file in state
              for(j in 1:length(tfiles)){
                  #read each file
                      tlist[[j]]<-read.fst(tfiles[j])
                  #once all files are read
                      if(j==length(tfiles)){
                          #bind them
                              tdf<-do.call(rbind,tlist)
                          #save
                              getwd()
                              filename<-file.path(state.dir,paste(unfileabr[i],"-all",".fst",sep=""))
                              write.fst(tdf,filename,compress=100)        
                      }
          }
        }
        
#3) aggregate states into pills by year by zip code
    setwd(state.dir)
    tfiles<-sort(list.files())
    ldf<-list()
    for(i in 1:length(tfiles)){
      tdf<-read.fst(tfiles[i],as.data.table = T)
      tdf$year<-as.numeric(substr(tdf$TRANSACTION_DATE,nchar(tdf$TRANSACTION_DATE)-3,nchar(tdf$TRANSACTION_DATE)))
      tdf2<-tdf[,.(base::sum(as.numeric(DOSAGE_UNIT))),by=c("BUYER_ZIP","year")]
      tdf2$state<-substr(tfiles[i],1,2)
      ldf[[i]]<-tdf2
      print(i)
    }
    df<-do.call(rbind,ldf)
    names(df)<-c("zipcode","year","pills","state")
    write.fst(df,file.path(zip.dir,"dea.fst"),compress=100)

#4) aggregate states into pills by year by county
    #a) single core
        setwd(state.dir)
        tfiles<-sort(list.files())
        ldf<-list()
        for(i in 1:length(tfiles)){
          tdf<-read.fst(tfiles[i],as.data.table = T)
          tdf$year<-as.numeric(substr(tdf$TRANSACTION_DATE,nchar(tdf$TRANSACTION_DATE)-3,nchar(tdf$TRANSACTION_DATE)))
          tdf2<-tdf[,.(base::sum(as.numeric(DOSAGE_UNIT))),by=c("BUYER_COUNTY","year")]
          tdf2$state<-substr(tfiles[i],1,2)
          ldf[[i]]<-tdf2
          print(i)
        }
    #b) parallel cores
        library(parallel)
        library(doParallel)
        library(doSNOW)
        a<-names(sessionInfo()$otherPkgs)
        pb <- txtProgressBar(max = length(tfiles), style = 3)
        progress <- function(n) setTxtProgressBar(pb, n)
        opts <- list(progress = progress)
        cl <- makeCluster(detectCores())
        registerDoSNOW(cl)
        ldf<-foreach(i=1:length(tfiles),
                     .options.snow = opts,
                     .packages=a) %dopar%
        {
          tdf<-read.fst(tfiles[i],as.data.table = T)
          tdf$year<-as.numeric(substr(tdf$TRANSACTION_DATE,nchar(tdf$TRANSACTION_DATE)-3,nchar(tdf$TRANSACTION_DATE)))
          tdf<-tdf[,.(base::sum(as.numeric(DOSAGE_UNIT))),by=c("BUYER_COUNTY","year")]
          tdf$state<-substr(tfiles[i],1,2)
          gc()
          return(tdf)
        }
        closeAllConnections()
        df<-do.call(rbind,ldf)
        names(df)<-c("county","year","pills","state")
        write.fst(df,file.path(county.dir,"dea.fst"),compress=100)
    

        
    # cl <- makeCluster(detectCores())
    # registerDoSNOW(cl)
    # setwd(state.dir)
    # tfiles<-sort(list.files())
    # ldf<-list()
    # t3<-system.time({
    #     par_for(
    #       vars="ldf",
    #       forloop=
    #       'for(i in 1:length(tfiles)){
    #         tdf<-read.fst(tfiles[i],as.data.table = T)
    #         tdf$year<-as.numeric(substr(tdf$TRANSACTION_DATE,nchar(tdf$TRANSACTION_DATE)-3,nchar(tdf$TRANSACTION_DATE)))
    #         tdf2<-tdf[,.(base::sum(as.numeric(DOSAGE_UNIT))),by=c("BUYER_COUNTY","year")]
    #         tdf2$state<-substr(tfiles[i],1,2)
    #         ldf[[i]]<-tdf2
    #         print(i)
    #       }'
    #     )
    # })
    # closeAllConnections()
    # df<-do.call(rbind,ldf)
    # names(df)<-c("county","year","pills","state")
    # write.fst(df,file.path(county.dir,"dea.fst"),compress=100)
    

  
    #     library(parallel)
    #     library(doParallel)
    #     pb <- txtProgressBar(max = length(tfiles), style = 3)
    #     progress <- function(n) setTxtProgressBar(pb, n)
    #     opts <- list(progress = progress)
    #     cl <- makeCluster(detectCores())
    #     registerDoSNOW(cl)
    #     ldf2<-foreach(i=1:length(tfiles),
    #                  .options.snow = opts,
    #                  .packages=a) %dopar%
    #     { 
    #       tdf<-read.fst(tfiles[i],as.data.table = T)
    #       tdf$year<-as.numeric(substr(tdf$TRANSACTION_DATE,nchar(tdf$TRANSACTION_DATE)-3,nchar(tdf$TRANSACTION_DATE)))
    #       tdf<-tdf[,.(base::sum(as.numeric(DOSAGE_UNIT))),by=c("BUYER_COUNTY","year")]
    #       tdf$state<-substr(tfiles[i],1,2)
    #       gc()
    #       return(tdf)
    #     }
    #     closeAllConnections()
    # })