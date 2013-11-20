#' Take in a dataframe from GDELT and breakdown counts of cameo codes by temporal relation to disaster
#' 
#' CAMEO Codes found at http://gdelt.utdallas.edu/data/lookups/CAMEO.eventcodes.txt
#' 
#' @aliases CameoBreakdown
#' @param gdelt_df dataframe, subset of GDELT data
#' @param disaster.name character, name of disaster used in naming saved files, cannot have spaces
#' @param time.start character, in the form "yyyy-mm-dd", date to start tracking events.
#' @param disaster.start character, in the form "yyyy-mm-dd", time.start to disaster.start is called 'before'
#' @param disaster.end character, in the form "yyyy-mm-dd", disaster.start to disaster.end is called 'during'
#' @param time.end character, in the form "yyyy-mm-dd", disaster.end to time.end is called 'after'.
#' @param data.destination character, folder where GDELT subsets will be saved for easier analysis
#' @param verbose logical, if TRUE then indications of progress will be displayed.
#' @return NA
#' @export
#' @details
#'
#'



CameoBreakdown<-function(
  gdelt_df_name,
  disaster.name,
  time.start=NULL,
  disaster.start=NULL,
  disaster.end=NULL,
  time.end=NULL,
  data.destination, 
  verbose=TRUE){

  #### Create a funciton that takes in a subset of GDELT data and creates the before, during, after ######

  # make a column with proper formatted date, dis'a
  eval(parse(text=paste(gdelt_df_name, "$date<-as.Date(as.character(", gdelt_df_name, "$SQLDATE), '%Y%m%d')", sep="")))
  
  timing=vector()
  if(!is.null(time.start) & !is.null(disaster.start)){
    if(time.start!=disaster.start){
      timing=c(timing, "before")
      eval(parse(text=paste(gdelt_df_name, ".before<-",gdelt_df_name, "[", gdelt_df_name, "$date<as.Date('", disaster.start, "','%Y-%m-%d'),]", sep="")))
    
    }
  }
  if(!is.null(disaster.start) | !is.null(disaster.end)){
    timing=c(timing, "during")        
    eval(parse(text=paste(gdelt_df_name, ".during<-", gdelt_df_name, "[", gdelt_df_name, "$date>=as.Date('", ifelse(is.null(disaster.start),time.start,disaster.start) , "','%Y-%m-%d') & ", gdelt_df_name, "$date<=as.Date('", ifelse(is.null(disaster.end),time.end,disaster.end) , "','%Y-%m-%d'),]", sep="")))
    
    } else {
      timing=c(timing, "none")
      eval(parse(text=paste(gdelt_df_name, ".none<-",gdelt_df_name, "[", gdelt_df_name, "$date>=as.Date('", time.start, "','%Y-%m-%d') & ", gdelt_df_name, "$date<=as.Date('", time.end, "','%Y-%m-%d'),]", sep="")))
    }
  if(!is.null(disaster.end) & !is.null(time.end)){
    if(time.end!=disaster.end){
      timing=c(timing, "after")
      eval(parse(text=paste(gdelt_df_name, ".after<-", gdelt_df_name, "[", gdelt_df_name, "$date>as.Date('", disaster.end, "','%Y-%m-%d'),]", sep="")))
    }
  }
  
  
  eventlevel<-c("Root", "Base")
  # should only need to loop over t and e . . .
  for(t in timing){
    for(e in eventlevel){
    
      # looped across e, and t
      ## dis.t --> dis.t.Event'e'agg
      eval(parse(text=paste(gdelt_df_name, ".", t, ".Event", e, "agg<-aggregate(", gdelt_df_name, ".", t, "$Event", e, "Code, list(", gdelt_df_name, ".", t, "$Event", e, "Code), length)", sep="")))
      eval(parse(text=paste("names(", gdelt_df_name, ".", t, ".Event", e, "agg)<-c('", e, "Code', '", t, "count')", sep="")))

      ## dis.t.Event'e'agg --> dis.t.Event'e'agg.norm
      eval(parse(text=paste(gdelt_df_name, ".", t, ".Event", e, "agg.norm<-", gdelt_df_name, ".", t, ".Event", e, "agg", sep="")))
      eval(parse(text=paste(gdelt_df_name, ".", t, ".Event", e, "agg.norm[,2]<-", gdelt_df_name, ".", t, ".Event", e, "agg.norm[,2]/sum(", gdelt_df_name, ".", t, ".Event", e, "agg.norm[,2])", sep="")))
      eval(parse(text=paste("names(", gdelt_df_name, ".", t, ".Event", e, "agg.norm)<-c('", e, "Code', '", t, "count.norm')", sep="")))
    }
  }
  
  # looped across e, and timing[i]
  # for all t, dis.t.Event'e'agg.norm --> merged.list.a.Event'e'agg.norm 
  for(e in eventlevel){
    names<-paste(gdelt_df_name, ".", timing[1], ".Event", e, "agg.norm", sep="")
    if(length(timing)>1){
      for(i in 2:length(timing)){
        names<-paste(names, ", ", gdelt_df_name, ".", timing[i], ".Event", e, "agg.norm", sep="")
      }
    }
    eval(parse(text=paste("list.Event", e, "agg.norm = list(", names, ")", sep="")))
    eval(parse(text=paste("merged.Event", e,"agg.norm = Reduce(function(...) merge(..., all=T), list.Event",e,"agg.norm)", sep="")))
    eval(parse(text=paste("merged.Event", e,"agg.norm[is.na(merged.Event", e,"agg.norm)]<-min(na.omit(as.vector(as.matrix(merged.Event", e,"agg.norm[,-1]))))/100", sep=""))) 
  }
  
  # looped across timing[i]
  # Set legend and colors for histogram of cameo
  allt<-paste(timing[1], sep="")
  allcol<-paste("gray88")
  if(length(timing)>1){
    for(i in 2:length(timing)){
      allt<-paste(allt, "' ,'", timing[i], sep="")
      allcol<-paste(allcol, "', 'gray", 88-28*i, sep="")
    }
  }

  for(e in eventlevel){
    # looped across e
    # merged.Event'e'agg.norm --> 'name'_norm_'e'code.pdf
    # Create event specific histogram using 2 (root) or 3 (base) digit root codes 
    eval(parse(text=paste("pdf('", data.destination, gdelt_df_name, "_norm_", e, "code.pdf', height=5, w=", ifelse(e=="Root", "8", "14"), ")", sep="")))
    eval(parse(text=paste("barplot(t(as.matrix(merged.Event", e, "agg.norm[,2:",1+length(timing) ,"])), names.arg=merged.Event", e, "agg.norm[,1], beside=TRUE,", ifelse(e=="Base", " las=2, cex.names=.6, ", ""), "col=c('", allcol ,"'), main='GDELT events around ", disaster.name, "', xlab='GDELT Root Code', ylab='Event Count Normalized by Total Events')", sep="")))
    eval(parse(text=paste("legend('topright', legend=c('", allt, "'), pch=15, title='Timing vs. ", disaster.name, "', col=c('", allcol ,"'))", sep="")))
    dev.off()
  }
}

  
    
    
    