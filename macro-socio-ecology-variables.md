-   REFERENCES

{% include JB/setup %}

    library(ggplot2);library(reshape)
    wd<-"G:/Documents/PostDocKVA/Labbook/projects/macro-socio-ecology"
    QoG.wd<-"G:\\Documents\\PostDocKVA\\Data\\QOG"
    WB.poverty.wd<-"G:\\Documents\\PostDocKVA\\Data\\WorldBank\\PovertyEquityDB"

    year.min<-1990
    year.max<-2009

    setwd(wd)

    var.df<-read.csv("social_variables.csv",header=T,stringsAsFactors = FALSE,na.strings="")

    var.df<-var.df[which(is.na(var.df[,"varcode"])==FALSE),]

    data.dirs<-unique(var.df$dir)
    data.files<-unique(var.df$file)

    ## reading in QoG data

    if(QoG.wd %in% data.dirs){
      
    setwd(QoG.wd)  
    QoG.basts.df<-read.csv("qog_bas_ts_jan15.csv",header=T,stringsAsFactors = FALSE,na.strings="")  

    QoG.sel.vars<-unique(var.df[which(var.df$dataset == "QoG"),"varcode"])
      
    QoG.sel.df<-QoG.basts.df[,c("ccode","cname","year","ccodealp","cname_year","ccodealp_year","ccodecow","ccodewb","version",QoG.sel.vars)]

    #c("ccode","cname","year","ccodealp","cname_year","ccodealp_year","ccodecow","ccodewb","version",QoG.sel.vars)[c("ccode","cname","year","ccodealp","cname_year","ccodealp_year","ccodecow","ccodewb","version",QoG.sel.vars)%in%names(QoG.basts.df)==FALSE]

    QoG.sel.df<-QoG.sel.df[which(QoG.sel.df$year %in% c(year.min:year.max)),]


    }

    ### CALCULATING WITHIN COUNTRY STANDARD DEVIATION

    country.df<-data.frame("country"=sort(unique(QoG.sel.df$ccodealp)))
    #country.df[,paste(QoG.sel.vars,"mean",sep=".")]<-NA
    #country.df[,paste(QoG.sel.vars,"median",sep=".")]<-NA


    QoG.sel.df<-QoG.sel.df[order(QoG.sel.df$ccodealp),]

    QoG.sd<-as.data.frame(apply(QoG.sel.df[,QoG.sel.vars],2,function(x,y=QoG.sel.df$ccodealp) tapply(x,y,function(z) abs(sd(z,na.rm=TRUE)/mean(z,na.rm=TRUE)))))

    #names(QoG.sd)<-paste(names(QoG.sd),"sd",sep=".")

    country.df<-as.data.frame(cbind(country.df,QoG.sd))
    #country.df[,paste(QoG.sel.vars,"sd",sep=".")]<-QoG.sd

    country.melt.sd.df<-melt(country.df[,c("country",names(QoG.sd))])

    #country.melt.sd.df[,"variable"]<-gsub(".sd","")
    country.melt.sd.df<-merge(country.melt.sd.df,var.df,by.x="variable",by.y="varcode",all.x=TRUE)

    #x11()
    ggplot(country.melt.sd.df[-which(country.melt.sd.df[,"variable"]%in%c("cam_inclusive","wbgi_cce")),],aes(x=variable,y=value))+geom_violin()+facet_wrap(~social,scales="free",ncol=2)

![](G:\Documents\PostDocKVA\Labbook\projects\macro-socio-ecology\macro-socio-ecology-variables_files/figure-markdown_strict/unnamed-chunk-1-1.png)

    ####

    ### CALCULATING WITHIN COUNTRY UNIQUE VALUES

    country.df<-data.frame("country"=sort(unique(QoG.sel.df$ccodealp)))
    #country.df[,paste(QoG.sel.vars,"mean",sep=".")]<-NA
    #country.df[,paste(QoG.sel.vars,"median",sep=".")]<-NA


    QoG.sel.df<-QoG.sel.df[order(QoG.sel.df$ccodealp),]

    QoG.unique<-as.data.frame(apply(QoG.sel.df[,QoG.sel.vars],2,function(x,y=QoG.sel.df$ccodealp) tapply(x,y,function(z) length(which(is.na(unique(z,na.rm=TRUE))==FALSE)))))

    #names(QoG.sd)<-paste(names(QoG.sd),"sd",sep=".")

    country.df<-as.data.frame(cbind(country.df,QoG.unique))
    #country.df[,paste(QoG.sel.vars,"sd",sep=".")]<-QoG.sd

    country.melt.unique.df<-melt(country.df[,c("country",names(QoG.unique))])

    #country.melt.sd.df[,"variable"]<-gsub(".sd","")
    country.melt.unique.df<-merge(country.melt.unique.df,var.df,by.x="variable",by.y="varcode",all.x=TRUE)

    #x11()
    ggplot(country.melt.unique.df[-which(country.melt.unique.df[,"variable"]%in%c("cam_inclusive","wbgi_cce")),],aes(x=variable,y=value))+geom_violin()+facet_wrap(~social,scales="free",ncol=2)

![](G:\Documents\PostDocKVA\Labbook\projects\macro-socio-ecology\macro-socio-ecology-variables_files/figure-markdown_strict/unnamed-chunk-1-2.png)

    ### CALCULATING WITHIN COUNTRY UNIQUE VALUES PER TIME SERIES LENGTH

    country.df<-data.frame("country"=sort(unique(QoG.sel.df$ccodealp)))

    QoG.sel.df<-QoG.sel.df[order(QoG.sel.df$ccodealp),]

    QoG.relunique<-as.data.frame(apply(QoG.sel.df[,QoG.sel.vars],2,function(x,y=QoG.sel.df$ccodealp) tapply(x,y,function(z) length(which(is.na(unique(z,na.rm=TRUE))==FALSE))/
                                                                                                           length(which(is.na(z)==FALSE)))))

    #names(QoG.sd)<-paste(names(QoG.sd),"sd",sep=".")

    country.df<-as.data.frame(cbind(country.df,QoG.relunique))
    #country.df[,paste(QoG.sel.vars,"sd",sep=".")]<-QoG.sd

    country.melt.relunique.df<-melt(country.df[,c("country",names(QoG.relunique))])

    #country.melt.sd.df[,"variable"]<-gsub(".sd","")
    country.melt.relunique.df<-merge(country.melt.relunique.df,var.df,by.x="variable",by.y="varcode",all.x=TRUE)

    #x11()
    ggplot(country.melt.relunique.df[-which(country.melt.relunique.df[,"variable"]%in%c("cam_inclusive","wbgi_cce")),],aes(x=variable,y=value))+geom_violin()+facet_wrap(~social,scales="free",ncol=2)

![](G:\Documents\PostDocKVA\Labbook\projects\macro-socio-ecology\macro-socio-ecology-variables_files/figure-markdown_strict/unnamed-chunk-1-3.png)

<br><br>

------------------------------------------------------------------------

<br><br>

<br><br>

------------------------------------------------------------------------

------------------------------------------------------------------------

<br><br>

REFERENCES
==========

The following literature was cited
