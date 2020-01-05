#0) load libraries
    library(docshop)
    library(geosphere)
    library(ggplot2)
    library(reshape2)
    library(stringr)
    library(sp)
    library(rgeos)
    library(leaflet)
    library(tidycensus)
    library(ggplot2)
    library(scales)
    library(tigris)
    library(sf)
    options(tigris_class = "sf")
    options(tigris_use_cache = TRUE)
    
    #a) find main directory
        if(grepl("briarons",getwd())){
          main.dir<-"C:/Users/briarons/Desktop/Analysis - Data/Postdoc"
          fig.dir<-"C:/Users/briarons/Desktop/Temp - Figures/"
        }
        if(grepl("bda13",getwd())){
          main.dir<-"C:/Users/bda13/Desktop/Analysis - Data/Postdoc"
          fig.dir<-"C:/Users/bda13/Desktop/Temp - Figures/"
        }
        
    #b) find file paths
        mergeddir<-file.path(main.dir,"Merged files")
        mergedpath<-file.path(mergeddir,"DEA PDMP ACS (county).fst")
        shape.dir<-file.path(main.dir,"Shape files")
        dir.create(mergeddir,showWarnings = F)
        dir.create(shape.dir, showWarnings = FALSE)
        
#1) read data
    df<-read.fst(mergedpath,as.data.table = T)
    # tracts<-readRDS(file.path(shape.dir,"county shapes 2018.rds"))
    tracts <- counties(cb=F,year=2017,state="IN",resolution = "20m")

#2) create vars of interest
    df$pMAT<-df$n_MAT_patients/df$n_people
    df$pillsperperson<-df$pills/df$ind.pop
    df$pillsperperson[is.infinite(df$pillsperperson)]<-NA
    df$inchome<-df$income.median/df$homevalue.median
    df$ins.pillsperperson<-df$opioid_pill_total/df$ind.pop
    df<-df[df$year!=2006,]
    
#3) create weights based on all info prior to 2012
    df2<-df[df$year %in% 2010:2012,c("pills","ind.pop","opioid_pill_total","county","ins.pillsperperson","pillsperperson")]
    df2$opioid_pill_total[is.na(df2$opioid_pill_total)]<-0
    df2$ins.pillsperperson[is.na(df2$ins.pillsperperson)]<-0

    df3<-df2[,.(pillsperperson=mean(pillsperperson,na.rm=T),ins.pillsperperson=mean(ins.pillsperperson,na.rm=T),pills=mean(pills,na.rm=T),ind.pop=mean(ind.pop,na.rm=T),opioid_pill_total=mean(opioid_pill_total,na.rm=T)),by=c("county")]
    # df3$weight<-df3$pills/df3$opioid_pill_total
    df3$weight<-df3$pillsperperson/df3$ins.pillsperperson
    #a) don't allow weights to be greater than 1 (a place can't be over-represented) or less than .005 (in case certain places get overweighted)
        # df3$weight[df3$weight>1]<-1
        # df3$weight[df3$weight<.005]<-.005
        df3$weight[df3$weight>quantile(df3$weight,.95,na.rm=T)]<-quantile(df3$weight,.95,na.rm=T)
        df3$weight[df3$weight<quantile(df3$weight,.05,na.rm=T)]<-quantile(df3$weight,.05,na.rm=T)
    #b) merge weights into df
        df4<-df3[,c("county","weight")]
        df<-merge(df,df4,by="county",all=T)
    #c) make weighted vars
        df$w.MAT<-df$MAT_pill_total*df$weight
        df$w.MAT.perperson<-df$w.MAT/df$ind.pop
        df$no_has_MAT
        df$w.opioid_pill_total<-df$opioid_pill_total*df$weight
        df$w.pill.perperson<-df$w.opioid_pill_total/df$ind.pop
    
#4) subset to indiana
    df<-df[df$state.name=="indiana",]

#5) aggregate variablesacross years of interest
    # df.2<-df[df$year %in% 2010:2012, lapply(.SD, base::mean, na.rm=TRUE), by=c("county") ]
    df.4<-df[df$year %in% 2016:2018, lapply(.SD, base::mean, na.rm=TRUE), by=c("county") ]

#6) Merge relevant county level info into shape files
    tracts$GEOID2<-as.numeric(tracts$GEOID)
    df.4$GEOID2<-df.4$county
    df.42<-df.4[,c("w.MAT.perperson","w.pill.perperson","no_has_MAT","GEOID2")]
    countydf<-merge(tracts,df.42,by="GEOID2",all.x=T)
    #a) cap outliers
          countydf$w.pill.perperson[countydf$w.pill.perperson>200]<-200
          countydf$no_has_MAT[countydf$no_has_MAT>.015]<-.015
          countydf$w.MAT.perperson[countydf$w.MAT.perperson>3]<-3
          
#2) plot pillsperpop
    p<-ggplot()+
        geom_sf(
            data = countydf,
            aes(fill = w.pill.perperson),
            size = 0.5,
            color="grey50"
        )+
            coord_sf(crs = sf::st_crs(countydf), datum = NA)+
            # scale_fill_gradient(low = "white",high="red",na.value="white")+
            scale_fill_gradient2(low = "white",mid="#FFF6F6",high="#cc0000",na.value="white")+#,midpoint=20,limits=c(0,125)
            theme_void() +
            theme(axis.line = element_line(colour = "black"),
                  panel.grid = element_blank(),
                  panel.border = element_blank(),
                  panel.background = element_blank(),
                  # legend.position = "none",
                  plot.title = element_text(hjust = 0.5),
                  text=element_text(family="serif",face="bold",size=20),
                  axis.title=element_blank())+
        labs(fill="Pills per person",title="         Opioids Sold in Indiana (2016-2018)")
        ggsave(tsave1<-paste(fig.dir,"Opioids Sold in Indiana (2016-2018).png",sep=""),p,width=8.5,height=8,units="in")
        browseURL(tsave1)

# 
# #3b) Plot MAT by percent users
#     (p<-ggplot()+
#         geom_sf(
#             data = countydf,
#             aes(fill = no_has_MAT),
#             size = 0.5,
#             color="grey50"
#         )+
#             coord_sf(crs = sf::st_crs(countydf), datum = NA)+
#             scale_fill_gradient2(low = "white",high="#00cccc",na.value="white",labels = percent)+
#             theme_void() +
#             theme(axis.line = element_line(colour = "black"),
#                   panel.grid = element_blank(),
#                   panel.border = element_blank(),
#                   panel.background = element_blank(),
#                   plot.title = element_text(hjust = 0.5),
#                   text=element_text(family="serif",face="bold",size=20),
#                   axis.title=element_blank())+
#         labs(fill="Patients on MAT",title="         MAT Use in Indiana (2016-2018)"))
#         ggsave(tsave1<-paste(fig.dir,"MAT User in Indiana (2016-2018).png",sep=""),p,width=8.5,height=8,units="in")
#         browseURL(tsave1)
        
#3b) Plot MAT by number pills
    (p<-ggplot()+
        geom_sf(
            data = countydf,
            aes(fill = w.MAT.perperson),
            size = 0.5,
            color="grey50"
        )+
            coord_sf(crs = sf::st_crs(countydf), datum = NA)+
            scale_fill_gradient2(low = "white",high="#00cccc",na.value="white",labels = scales::number_format(accuracy = 0.01, decimal.mark = '.'))+
            theme_void() +
            theme(axis.line = element_line(colour = "black"),
                  panel.grid = element_blank(),
                  panel.border = element_blank(),
                  panel.background = element_blank(),
                  plot.title = element_text(hjust = 0.5),
                  text=element_text(family="serif",face="bold",size=20),
                  axis.title=element_blank())+
        labs(fill="Pills per person",title="         MAT Sold in Indiana (2016-2018)"))
        ggsave(tsave1<-paste(fig.dir,"MAT Sold in Indiana (2016-2018).png",sep=""),p,width=8.5,height=8,units="in")
        browseURL(tsave1)
        

#4) plot median splits
    #a) create plot function
        plotfun<-function(var,label,savename,title,color1){
            p1<-ggplot()+
              geom_sf(
                data = countydf,
                eval(parse(text=paste("aes(fill=",var,")",sep=""))),
                alpha=.4,
                size = 0.5,
                color="grey20"
              )+
              coord_sf(crs = sf::st_crs(countydf), datum = NA)+
              theme_void() +
              theme(axis.line = element_line(colour = "black"),
                    panel.grid = element_blank(),
                    panel.border = element_blank(),
                    panel.background = element_blank(),
                    plot.title = element_text(hjust = 0.5),
                    text=element_text(family="serif",face="bold",size=20),
                    axis.title=element_blank())+
              labs(title=paste("         ",title,sep=""),fill=label)+
              scale_fill_manual(values = c(color1,"white"))
              ggsave(tsave1<-paste(fig.dir,savename,sep=""),p1,width=8.5,height=8,units="in")
              browseURL(tsave1)
        }
        countydf$w.MAT.perperson
        countydf$w.pill.perperson
        
    #b) create median splits
        #ii) pills median split
            pillsovermedian2<-countydf$w.pill.perperson>median(countydf$w.pill.perperson,na.rm=T)
            pillsovermedian2[is.na(pillsovermedian2)]<-0
            countydf$Count<-ifelse(pillsovermedian2,paste("Opioid Pill Sales > Median (",round(median(countydf$w.pill.perperson,na.rm=T),2),")",sep=""), "Opioid Pill Sales < Median")
            countydf$Count2<-factor(countydf$Count,levels = unique(countydf$Count)[c(2,1)])
            plotfun(var="Count2",title="Opioid Pill Sales in Indiana",label="Opioid Pills per person",savename="Split-Median Opioid Sales in Indiana.png",color1="#cc0000")

        #iii) MAT median split
            matovermedian2<-countydf$w.MAT.perperson>median(countydf$w.MAT.perperson,na.rm=T)
            matovermedian2[is.na(matovermedian2)]<-0
            countydf$Count<-ifelse(matovermedian2,paste("MAT Pill Sales > Median (",round(median(countydf$w.MAT.perperson,na.rm=T),2),")",sep=""), "MAT Pill Sales < Median")
            countydf$Count2<-factor(countydf$Count,levels = unique(countydf$Count)[c(2,1)])
            plotfun(var="Count2",title="MAT Pill Sales in Indiana",label="MAT Pills per person",savename="Split-Median MAT Sales in Indiana.png",color1="#00cccc")

        #iv) mixed
            bothhigh<-matovermedian2 & pillsovermedian2
            mathigh<-!bothhigh & matovermedian2
            pillshigh<-!bothhigh & pillsovermedian2
            countydf$Count<-ifelse(
              pillshigh,paste("Opioid Pill Sales > Median (",round(median(countydf$w.pill.perperson,na.rm=T),2),")",sep=""), ifelse(
              mathigh,paste("MAT Pill Sales > Median (",round(median(countydf$w.MAT.perperson,na.rm=T),2),")",sep=""), ifelse(
              bothhigh,"Both > Median", "Both < Median"
            )))
            countydf$Count2<-factor(countydf$Count,levels = unique(countydf$Count)[c(4,2,1,3)][c(1,4,2,3)])
            plotfun(var="Count2",title="Opioid and MAT Pill Sales in Indiana",label="MAT Use",savename="Split-Median Opioid and MAT Sales in Indiana.png",color1=c("yellow","#cc0000","#00cccc"))
