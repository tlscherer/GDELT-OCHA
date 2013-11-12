#' Download and manipulate GDELT data for visualization of events around disasters
#'
#' Download the GDELT files necessary for a data set, import them,
#' filter on various crieteria, and return a data.frame. 
#' 
#' @aliases GDELTdisaster
#' @param disaster.name character, name of disaster used in naming saved files, cannot have spaces
#' @param time.start character, in the form "yyyy-mm-dd", date to start tracking events.
#' @param disaster.start character, in the form "yyyy-mm-dd", time.start to disaster.start is called 'before'
#' @param disaster.end character, in the form "yyyy-mm-dd", disaster.start to disaster.end is called 'during'
#' @param time.end character, in the form "yyyy-mm-dd", disaster.end to time.end is called 'after'.
#' @param local.folder character, if specified, where downloaded GDELT files will be saved or loaded from
#' @param data.destination character, folder where GDELT subsets will be saved for easier analysis
#' @param plot.destination character, folder where created graphs will be saved
#' @param GeoCountryCode,
#' @param getdata logical, if TRUE then will capture GDELT data using GetGdelt
#' @param analyzedata logical, if TRUE then will run analysis on save output of getdata section
#' @param verbose logical, if TRUE then indications of progress will be displayed.
#' @return NA
#' @export
#' @details
#'
#'
#' If a needed file has already been downloaded to \code{local.folder} then this file is used instead of being downloaded. This can greatly speed up future
#' 
#' 
#' @references
#' GDELT: Global Data on Events, Location and Tone, 1979-2012. Presented at the 2013 meeting of the International Studies Association in San Francisco, CA.
#' \url{http://gdelt.utdallas.edu/}
#' 
#' instant_pkgs from Kat Cichini - https://github.com/gimoya/theBioBucket-Archives/blob/master/R/Functions/instant_pkgs.R
#' 
#' GDELTtools from Stephen Haptonstahl, Thomas Scherer, Oscar Thoms - http://cran.r-project.org/web/packages/GDELTtools/
#' 
#' @author 
#' \tabular{ll}{
#'   Thomas Scherer \tab \email{tscherer@@princeton.edu}\cr
#' }
#' @examples
#' \dontrun{   
#' 2013 Hurrican Phailin             
#' GDELTdisaster(disaster.name="Phailin",
#'              time.start="2013-09-27",
#'              disaster.start="2013-10-04",
#'              disaster.end="2013-10-14",
#'              time.end="2013-10-21",
#'              local.folder="C:/Users/TLScherer/Documents/GDELT",
#'              data.destination="H:/Work/SideProjects/GDELT/OCHA/OCHA/testfolder/",
#'              plot.destination="x",
#'              GeoCountryCode="IN",
#'              getdata=TRUE,
#'              analyzedata=TRUE,
#'              verbose=TRUE)                        
#'                          
#' 2013 North India Floods
#' GDELTdisaster(disaster.name="NorthIndiaFloods",
#'               time.start="2013-07-07",
#'               disaster.start="2013-07-14", 
#'               disaster.end="2013-07-17",
#'               time.end="2013-07-24",
#'               local.folder="C:/Users/TLScherer/Documents/GDELT",
#'               data.destination="H:/Work/SideProjects/GDELT/OCHA/OCHA/testfolder/",
#'               plot.destination="x",
#'               GeoCountryCode="IN",
#'               getdata=TRUE,
#'               analyzedata=TRUE,
#'               verbose=TRUE)
#'               
#' 1999 Hurrican Odisha
#' GDELTdisaster(disaster.name="Odisha",
#'               time.start="1999-10-18",
#'               disaster.start="1999-10-25", 
#'               disaster.end="1999-11-03",
#'               time.end="1999-11-10",
#'               local.folder="C:/Users/TLScherer/Documents/GDELT",
#'               data.destination="H:/Work/SideProjects/GDELT/OCHA/OCHA/testfolder/",
#'               plot.destination="x",
#'               GeoCountryCode="IN",
#'               getdata=TRUE,
#'               analyzedata=TRUE,
#'               verbose=TRUE)
  
GDELTdisaster <- function(disaster.name,
                          time.start=NULL, 
                          disaster.start=NULL,
                          disaster.end=NULL,
                          time.end=NULL,
                          local.folder,
                          data.destination,
                          plot.destination,
                          GeoCountryCode,
                          getdata=TRUE,
                          analyzedata=TRUE,
                          verbose=TRUE){
    

  ##########
  memory.limit(size=100000000000)
  
  # Load subfunctions to be used during function
  if(verbose) cat("\n loading functions... \n\n")   
  
  # instant_pkgs from Kat Cichini - https://github.com/gimoya/theBioBucket-Archives/blob/master/R/Functions/instant_pkgs.R
  instant_pkgs <- function(pkgs) { 
    pkgs_miss <- pkgs[which(!pkgs %in% installed.packages()[, 1])]
    if (length(pkgs_miss) > 0) {
      install.packages(pkgs_miss)
    }
    
    if (length(pkgs_miss) == 0) {
      message("\n ...Packages were already installed!\n")
    }
    
    # install packages not already loaded:
    pkgs_miss <- pkgs[which(!pkgs %in% installed.packages()[, 1])]
    if (length(pkgs_miss) > 0) {
      install.packages(pkgs_miss)
    }
    
    # load packages not already loaded:
    attached <- search()
    attached_pkgs <- attached[grepl("package", attached)]
    need_to_attach <- pkgs[which(!pkgs %in% gsub("package:", "", attached_pkgs))]
    
    if (length(need_to_attach) > 0) {
      for (i in 1:length(need_to_attach)) require(need_to_attach[i], character.only = TRUE)
    }
    
    if (length(need_to_attach) == 0) {
      message("\n ...Packages were already loaded!\n")
    }
  }
  
  # create function to take a date a specific point between two given dates
  middate <- function(date1, date2, format="%Y-%m-%d", point=.5){
    outdate <- as.Date(date1, format)+(as.Date(date2, format) - as.Date(date1, format)) * point
    return(outdate)
  }
  
  
  ##########
  ## Load Datasets if getdata=TRUE
  ##########
  
  if(getdata){

    # Get data from online GDELT websites and save to 
    
    instant_pkgs("GDELTtools") 
    
    # check time constraints 
    
    if(!is.null(time.start)){
      start.date<-time.start      
    } else if (!is.null(disaster.start)){
      start.date<-disaster.start
    } else {
      stop("need time.start or disaster.start")
    }
    
    if(!is.null(time.end)){
      end.date<-time.end      
    } else if (!is.null(disaster.end)){
      end.date<-disaster.end
    } else {
      stop("need time.end or disaster.end")
    }
    
    if(verbose) cat("Saving location data as ", data.destination, disaster.name, "loc... \n\n", sep="") 
    assign(paste(disaster.name, "loc", sep=""), GetGDELT(start.date, end.date, local.folder=local.folder, filter=list(ActionGeo_CountryCode=c(GeoCountryCode))))
    eval(parse(text=paste("save(", disaster.name, "loc",", file='", data.destination, disaster.name, "loc')", sep="")))
    
    if(verbose) cat("Saving actor1 data as ", data.destination, disaster.name, "act1... \n\n", sep="")
    assign(paste(disaster.name, "act1", sep=""), GetGDELT(start.date, end.date, local.folder=local.folder, filter=list(Actor1Geo_CountryCode=c(GeoCountryCode))))
    eval(parse(text=paste("save(", disaster.name, "act1",", file='", data.destination, disaster.name, "act1')", sep="")))
    
    if(verbose) cat("Saving actor2 data as ", data.destination, disaster.name, "act2... \n\n", sep="") 
    assign(paste(disaster.name, "act2", sep=""), GetGDELT(start.date, end.date, local.folder=local.folder, filter=list(Actor2Geo_CountryCode=c(GeoCountryCode))))
    eval(parse(text=paste("save(", disaster.name, "act2",", file='", data.destination, disaster.name, "act2')", sep="")))
    
  } else {
    if(verbose) cat("Assuming relevant subsets exist... \n\n")
  } # close getdata
  
  
  
  ##########
  ## Manipulate the dataset and output the tables and charts of interest if analyze=TRUE
  ##########
  
  if(analyzedata){
    
    # check time constraints 
    
    if(!is.null(time.start)){    
    } else if (!is.null(disaster.start)){
    } else {
      stop("need time.start or disaster.start")
    }
    
    if(!is.null(time.end)){   
    } else if (!is.null(disaster.end)){
    } else {
      stop("need time.end or disaster.end")
    }
    
    
    if(verbose) cat("Loading relevant subsets from ", data.destination, "... \n\n", sep="")
    
    actorrole<-c("loc", "act1", "act2")
    
    # start actor loop a1
    for(a in actorrole){
      
      if(verbose) cat("Running ", disaster.name, a, "... \n\n", sep="")
      
      # load the datasets
      eval(parse(text=paste("load('", data.destination, disaster.name, a, "')", sep="")))

      # make a column with proper formatted date
      eval(parse(text=paste(disaster.name, a, "$date<-as.Date(as.character(",disaster.name, a, "$SQLDATE), '%Y%m%d')", sep="")))
      
      # aggregate by day"
      eval(parse(text=paste(disaster.name, a, ".dateagg<-aggregate(",disaster.name, a, "$date, list(", disaster.name, a, "$date), length)", sep="")))
      
      # TO MAKE GRAPHS OF DIFFERENT CODES, HERE IS WHERE I WANT TO MAKE THE SUBSAMPLE
      
      # appeals for aid (023)
      # provide aid (07)
      # pessimistic comment (012)
      # optimistic comment (013)

      if(a=="loc"){
        
        eval(parse(text=paste("pess<-",disaster.name, a, "$EventBaseCode=='012'", sep="")))
        eval(parse(text=paste("opt<-",disaster.name, a, "$EventBaseCode=='013'", sep="")))
        eval(parse(text=paste("req_aid<-",disaster.name, a, "$EventBaseCode=='023'", sep="")))
        eval(parse(text=paste("prov_aid<-",disaster.name, a, "$EventRootCode=='07'", sep="")))
        eval(parse(text=paste("codedummies<-cbind(",disaster.name, a, "$date, pess, opt, req_aid, prov_aid)", sep="")))
        eval(parse(text=paste("codetrends<-aggregate(codedummies, list(", disaster.name, a, "$date), sum)", sep="")))
        codetrends[,3:6]<-(codetrends[,3:6]/codetrends[,2]*1000000)
      }
      
      #### Determine Temporal Format based on inputs
      
      timing=vector()
      
      if(!is.null(time.start) & !is.null(disaster.start)){
        if(time.start!=disaster.start){
          timing=c(timing, "before")
          eval(parse(text=paste(disaster.name, a, ".before<-",disaster.name, a, "[", disaster.name, a, "$date<as.Date('", disaster.start, "','%Y-%m-%d'),]", sep="")))
        }
      }
      if(!is.null(disaster.start) | !is.null(disaster.end)){
        timing=c(timing, "during")        
        eval(parse(text=paste(disaster.name, a, ".during<-",disaster.name, a, "[", disaster.name, a, "$date>=as.Date('", ifelse(is.null(disaster.start),time.start,disaster.start) , "','%Y-%m-%d') & ", disaster.name, a, "$date<=as.Date('", ifelse(is.null(disaster.end),time.end,disaster.end) , "','%Y-%m-%d'),]", sep="")))
      } else {
        timing=c(timing, "none")
        eval(parse(text=paste(disaster.name, a, ".none<-",disaster.name, a, "[", disaster.name, a, "$date>=as.Date('", time.start, "','%Y-%m-%d') & ", disaster.name, a, "$date<=as.Date('", time.end, "','%Y-%m-%d'),]", sep="")))
      }
      if(!is.null(disaster.end) & !is.null(time.end)){
        if(time.end!=disaster.end){
          timing=c(timing, "after")
          eval(parse(text=paste(disaster.name, a, ".after<-",disaster.name, a, "[", disaster.name, a, "$date>as.Date('", disaster.end, "','%Y-%m-%d'),]", sep="")))
        }
      }

      ##########
      ## Loop over timing and event level to format subsets for graphing
      ##########
      
      eventlevel<-c("Root", "Base")
      # start event loop e1
      for(e in eventlevel){
        # start timing loop t1
        for(t in timing){ 
          if(verbose) cat("Formatting ", t ," data for event specific bar plots... \n\n", sep="")
          
          eval(parse(text=paste(disaster.name, a, ".", t, ".Event", e, "agg<-aggregate(", disaster.name, a, ".", t, "$Event", e, "Code, list(", disaster.name, a, ".", t, "$Event", e, "Code), length)", sep="")))
          eval(parse(text=paste("names(", disaster.name, a, ".", t, ".Event", e, "agg)<-c('", e, "Code', '", t, "count')", sep="")))
          eval(parse(text=paste(disaster.name, a, ".", t, ".Event", e, "agg.norm<-", disaster.name, a, ".", t, ".Event", e, "agg", sep="")))
          eval(parse(text=paste(disaster.name, a, ".", t, ".Event", e, "agg.norm[,2]<-", disaster.name, a, ".", t, ".Event", e, "agg.norm[,2]/sum(", disaster.name, a, ".", t, ".Event", e, "agg.norm[,2])", sep="")))
          eval(parse(text=paste("names(", disaster.name, a, ".", t, ".Event", e, "agg.norm)<-c('", e, "Code', '", t, "count.norm')", sep="")))
          
          } # close timing loop t1
        
        names<-paste(disaster.name, a, ".", timing[1], ".Event", e, "agg.norm", sep="")
        
        if(length(timing)>1){
          for(i in 2:length(timing)){
            names<-paste(names, ", ", disaster.name, a, ".", timing[i], ".Event", e, "agg.norm", sep="")
          }
        }
        eval(parse(text=paste("list.", a, ".Event", e, "agg.norm = list(", names, ")", sep="")))
        
        eval(parse(text=paste("merged.", a, ".Event",e,"agg.norm = Reduce(function(...) merge(..., all=T), list.", a, ".Event",e,"agg.norm)", sep="")))
        
      } # close event loop e1
    } # close actor loop a1
    
    
    ##########
    # Create plot of total events over time
    ##########
    
    if(verbose) cat("Saving plot of all events over time as ", data.destination, disaster.name, "_dailycount.pdf  ...\n\n", sep="")
    eval(parse(text=paste("pdf('", data.destination, disaster.name, "_dailycount.pdf',height=6, w=11)", sep="")))
    eval(parse(text=paste("plot(", disaster.name, "loc.dateagg[,1], ", disaster.name, "loc.dateagg[,2], xlab='date', ylab='event count', type='b', ylim=c(0, 1.4*max(", disaster.name, "loc.dateagg[,2])), main='GDELT events concerning ", disaster.name, "')", sep="")))
    eval(parse(text=paste("points(", disaster.name, "act1.dateagg[,1], ", disaster.name, "act1.dateagg[,2], type='b',col='blue')", sep="")))
    eval(parse(text=paste("points(", disaster.name, "act2.dateagg[,1], ", disaster.name, "act2.dateagg[,2], type='b',col='red')", sep="")))
    # only highlight disaster time if a disaster is recorded
    if(!is.null(disaster.start) | !is.null(disaster.end)){
    eval(parse(text=paste("rect(as.Date('", ifelse(is.null(disaster.start),time.start,disaster.start), "','%Y-%m-%d'),-1000, as.Date('", ifelse(is.null(disaster.end),time.end,disaster.end), "','%Y-%m-%d'), 100000,col = rgb(202/255,255/255,112/255,.5))", sep="")))    
    }
    # add legend
    eval(parse(text=paste("legend('topright', legend=c('Location','Actor1','Actor2'), col=c('black', 'blue', 'red'), pch=21, title='Country role:')", sep="")))
    eval(parse(text=paste("text(x=as.Date('", middate(ifelse(is.null(disaster.start),time.start,disaster.start), ifelse(is.null(disaster.end),time.end,disaster.end)), "','%Y-%m-%d'), y=1.35*max(", disaster.name, "loc.dateagg[,2]), labels = '", disaster.name, "', col = rgb(34/255,139/255,34/255,1))", sep="")))
    dev.off()
    
    ##########
    # Create plot of subsets of interest over time
    ##########
    
    codetrends
    
    if(verbose) cat("Saving plot of specific event codes over time as ", data.destination, disaster.name, "_dailycountbycode.pdf  ...\n\n", sep="")
    eval(parse(text=paste("pdf('", data.destination, disaster.name, "_dailycountbycode.pdf',height=6, w=11)", sep="")))
    
    eval(parse(text=paste("plot(codetrends[,1], codetrends[,3], xlab='date', ylab='event count', type='l', ylim=c(0, 1.4*max(codetrends[,3:6])), col='pink', main='GDELT events concerning ", disaster.name, "')", sep="")))
    eval(parse(text=paste("points(codetrends[,1], codetrends[,4], type='l',col='lightblue')", sep="")))
    eval(parse(text=paste("points(codetrends[,1], codetrends[,5], type='l',col='darkred')", sep="")))
    eval(parse(text=paste("points(codetrends[,1], codetrends[,6], type='l',col='blue')", sep="")))
    
    # pessimistic comment (012)
    # optimistic comment (013)
    # appeals for aid (023)
    # provide aid (07)

    
    # only highlight disaster time if a disaster is recorded
    if(!is.null(disaster.start) | !is.null(disaster.end)){
      eval(parse(text=paste("rect(as.Date('", ifelse(is.null(disaster.start),time.start,disaster.start), "','%Y-%m-%d'),-1000, as.Date('", ifelse(is.null(disaster.end),time.end,disaster.end), "','%Y-%m-%d'), 100000,col = rgb(202/255,255/255,112/255,.5))", sep="")))    
    }
    # add legend
    eval(parse(text=paste("legend('topright', legend=c('aid appeal','pessimistic', 'optimistic','aid provision'), col=c('darkred', 'pink', 'lightblue', 'darkblue'), title='Event type', pch=15)", sep="")))
    eval(parse(text=paste("text(x=as.Date('", middate(ifelse(is.null(disaster.start),time.start,disaster.start), ifelse(is.null(disaster.end),time.end,disaster.end)), "','%Y-%m-%d'), y=1.35*max(codetrends[,3:6]), labels = '", disaster.name, "', col = rgb(34/255,139/255,34/255,1))", sep="")))
    dev.off()
    

    if(verbose) cat("Creating event specific bar plots in ", data.destination, " ... \n\n", sep="")
    
    # start actor loop a1
    
    for(a in actorrole){
      

      allt<-paste(timing[1], sep="")
      allcol<-paste("gray88")
      if(length(timing)>1){
        for(i in 2:length(timing)){
          allt<-paste(allt, "' ,'", timing[i], sep="")
          allcol<-paste(allcol, "', 'gray", 88-28*i, sep="")
        }
      }
      # Create event specific histogram using 2 digit root codes 
      
      # pdf('H:/Work/SideProjects/GDELT/RWork/NorthIndiaFloodsloc_norm_rootcode.pdf', height=5, w=8)
      eval(parse(text=paste("pdf('", data.destination, disaster.name, a, "_norm_rootcode.pdf', height=5, w=8)", sep="")))

      # barplot(t(as.matrix(merged.locEventRootagg.norm[,2:4])), names.arg=merged.locEventRootagg.norm[,1], beside=TRUE, col=c('gray88', 'gray60', 'gray31'), main='GDELT events around NorthIndiaFloods using loc', xlab='GDELT Root Code', ylab='Event Count Normalized by Total Events')
      eval(parse(text=paste("barplot(t(as.matrix(merged.", a, ".EventRootagg.norm[,2:",1+length(timing) ,"])), names.arg=merged.", a, ".EventRootagg.norm[,1], beside=TRUE, col=c('", allcol ,"'), main='GDELT events around ", disaster.name, " using ", a, "', xlab='GDELT Root Code', ylab='Event Count Normalized by Total Events')", sep="")))
    
      # legend("topright", legend=c("Before", "During", "After"), pch=15, title="Timing vs. Hurricane", col=c("gray88", "gray60", "gray31"))
      eval(parse(text=paste("legend('topright', legend=c('", allt, "'), pch=15, title='Timing vs. ", disaster.name, "', col=c('", allcol ,"'))", sep="")))
      
      dev.off()

        
      # Create event specific histogram using 3 base root codes 
            
      # pdf('H:/Work/SideProjects/GDELT/RWork/NorthIndiaFloodsloc_norm_basecode.pdf', height=5, w=14)
      eval(parse(text=paste("pdf('", data.destination, disaster.name, a, "_norm_basecode.pdf', height=5, w=14)", sep="")))
    
      # barplot(t(as.matrix(merged.locEventBaseagg.norm[,2:4])), names.arg=merged.locEventBaseagg.norm[,1], beside=TRUE, las=2, cex.names=.6, col=c('gray88', 'gray60', 'gray31'), main='GDELT Events around NorthIndiaFloods using loc', xlab='GDELT Base Code', ylab='Event Count Normalized by Total Events')
      eval(parse(text=paste("barplot(t(as.matrix(merged.", a, ".EventBaseagg.norm[,2:",1+length(timing) ,"])), names.arg=merged.", a, ".EventBaseagg.norm[,1], beside=TRUE, las=2, cex.names=.6, col=c('", allcol ,"'), main='GDELT Events around ", disaster.name, " using ", a, "', xlab='GDELT Base Code', ylab='Event Count Normalized by Total Events')", sep="")))
      # legend("topright", legend=c("Before", "During", "After"), pch=15, title="Timing vs. Hurricane", col=c("gray88", "gray60", "gray31"))
      eval(parse(text=paste("legend('topright', legend=c('", allt, "'), pch=15, title='Timing vs. ", disaster.name, "', col=c('", allcol ,"'))", sep="")))
      
      dev.off()
  
    } # for close actor loop a1
    
  }else if(verbose) {
    cat("I ain't got time for analysis... \n\n", sep="")
  } # close analyzedata (the else if)
} # end GDELTdisaster


#' 2013 Hurrican Phailin             
 GDELTdisaster(disaster.name="Phailin",
              time.start="2013-09-27",
              disaster.start="2013-10-04",
              disaster.end="2013-10-14",
              time.end="2013-10-21",
              local.folder="C:/Users/TLScherer/Documents/GDELT",
              data.destination="H:/Work/SideProjects/GDELT/OCHA/OCHA/testfolder/",
              plot.destination="x",
              GeoCountryCode="IN",
              getdata=FALSE,
            analyzedata=TRUE,
              verbose=TRUE)                        
#'                          
#' 2013 North India Floods
 GDELTdisaster(disaster.name="NorthIndiaFloods",
               time.start="2013-07-07",
               disaster.start="2013-07-14", 
               disaster.end="2013-07-17",
               time.end="2013-07-24",
               local.folder="C:/Users/TLScherer/Documents/GDELT",
               data.destination="H:/Work/SideProjects/GDELT/OCHA/OCHA/testfolder/",
               plot.destination="x",
               GeoCountryCode="IN",
               getdata=FALSE,
               analyzedata=TRUE,
               verbose=TRUE)


# 1999 Hurrican Odisha
GDELTdisaster(disaster.name="Odisha",
              time.start="1999-10-18",
              disaster.start="1999-10-25", 
              disaster.end="1999-11-03",
              time.end="1999-11-10",
              local.folder="C:/Users/TLScherer/Documents/GDELT",
              data.destination="H:/Work/SideProjects/GDELT/OCHA/OCHA/testfolder/",
              plot.destination="x",
              GeoCountryCode="IN",
              getdata=FALSE,
              analyzedata=TRUE,
              verbose=TRUE)


# Phillipines Typhoon Haiyan
GDELTdisaster(disaster.name="Typhoon_Haiyan",
              time.start="2013-11-05",
              disaster.start="2013-11-08",
              time.end="2013-11-09",
              local.folder="C:/Users/TLScherer/Documents/GDELT",
              data.destination="H:/Work/SideProjects/GDELT/OCHA/OCHA/testfolder/",
              plot.destination="x",
              GeoCountryCode="RP",
              getdata=FALSE,
              analyzedata=TRUE,
              verbose=TRUE)


