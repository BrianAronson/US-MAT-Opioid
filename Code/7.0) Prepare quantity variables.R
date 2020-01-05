#0) prep workspace
    #a) load packages
        library(docshop)
    #b) find main directory
        if(grepl("briarons",getwd())){
          main.dir<-"C:/Users/briarons/Desktop/Analysis - Data/Postdoc"
        }
        if(grepl("bda13",getwd())){
          main.dir<-"C:/Users/bda13/Desktop/Analysis - Data/Postdoc"
        }
    #c) find file paths
        quantdir<-file.path(main.dir,"Pill quantities")
        quantpath<-file.path(quantdir,"Pill quantities (county).fst")
        quantpath2<-file.path(quantdir,"Pill quantities (county yearly).fst")
        dir.create(quantdir,showWarnings = F)
    #d - load necessary data
        drugs_all = s3read_csv(file.path("hidden")) # use NDC for matching
        drugs_all[,NDC:=create_leading_zeros(NDC,11)]
        drugs_opioid = drugs_all[Class=='Opioid',]
        drugs_MAT = s3read_csv(file.path("hidden"))
        drugs_MAT[,NDC:=create_leading_zeros(NDC,11)]

#1) Prepare loop
    dfl<-list()
    dates<-paste(rep(2007:2018,each=4),c("q1","q2","q3","q4"),sep="")
    for(i in 1:length(dates)){
        dfname<-paste("hidden",dates[i],".fst",sep="")
        dfname2<-paste("s3hidden",dates[i],".csv.gz",sep="")

#2) load data iteratively
    df<-s3read_any(dfname,columns = c("PATID","NDC","QUANTITY"))
    df2<-s3read_any(dfname2,select=c("PATID","county"))

#3) estimate pills sold
    #a) merge datasets
        df3<-merge(df,df2,by="PATID",all.x = T)
    #b) identify whether drug is opioid or MAT
        df3$opioid<-df3$NDC %in% drugs_opioid$NDC
        df3$MAT<-df3$NDC %in% drugs_MAT$NDC
    #c) count pills per county
        df4<-df3[,.(MAT_pill_total=sum(QUANTITY[MAT]),opioid_pill_total=sum(QUANTITY[opioid])),by="county"]
    #d) append to list
        df4$date<-dates[i]
        dfl[[i]]<-df4
        print(i)
    }

#4) save pill quantities
    dff<-do.call(rbind,dfl)
    write.fst(dff,quantpath)
    
#5) aggregate by year and save again
    dff2<-dff
    dff2$date<-substr(dff2$date,1,4)
    dff2<-dff2[,.(MAT_pill_total=sum(MAT_pill_total),opioid_pill_total=sum(opioid_pill_total)),by=c("county","date")]
    write.fst(dff2,quantpath2)
    

    