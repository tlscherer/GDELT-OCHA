#' Take in a dataframe from GDELT and plot counts of individual codes over time
#'
#' 
#' @aliases DailyCountbyCode
#' @param gdelt_df dataframe, subset of GDELT data
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

DailyCountbyCode<-function(
  gdelt_df_name,
  disaster.name,
  time.start,
  disaster.start,
  disaster.end,
  time.end,
  data.destination,
  plotname=gdelt_df_name,
  subcodes,
  subnames=subcodes,
  col=NA,
  lty=NA)
{
  
  # create function to take a date a specific point between two given dates
  middate <- function(date1, date2, format="%Y-%m-%d", point=.5){
    outdate <- as.Date(date1, format)+(as.Date(date2, format) - as.Date(date1, format)) * point
    return(outdate)
  }
  
  
  # make a column with proper formatted date, dis'a
  eval(parse(text=paste(gdelt_df_name, "$date<-as.Date(as.character(", gdelt_df_name, "$SQLDATE), '%Y%m%d')", sep="")))
  
  eval(parse(text=paste("codedummies<-rep(1, nrow(", gdelt_df_name, "))", sep="")))  
  
  for(c in subcodes){
    eval(parse(text=paste("is", c, "<-", gdelt_df_name, "$Event", ifelse(nchar(c)==3,"Base","Root" ), "Code=='", c, "'", sep="")))
    eval(parse(text=paste("codedummies<-cbind(codedummies, is", c, ")", sep="")))
  }
  
  eval(parse(text=paste("codetrends<-aggregate(codedummies, list(", gdelt_df_name, "$date), sum)", sep="")))
  colnames(codetrends)[1]<-"date"
  
  if(is.na(col[1])){
    col<-rep("black", length(subcodes))
  }
  
  
  if(is.na(lty[1])){
    lty<-c(1:length(subcodes))
  }
  
  eval(parse(text=paste("pdf('", data.destination, plotname, "_dailycountbycode.pdf',height=6, w=11)", sep="")))
  eval(parse(text=paste("plot(codetrends$date, codetrends[,3], xlab='date', ylab='event count', type='l', ylim=c(0, 1.4*max(codetrends[,-1:-2])), col=col[1], lty=lty[1], main='GDELT events concerning ", disaster.name, "')", sep="")))
  
  for(row in 2:length(subcodes)){
    eval(parse(text=paste("points(codetrends$date, codetrends[,(row+2)], type='l',col=col[row], lty=lty[row])", sep="")))
  }
  
  # only highlight disaster time if a disaster is recorded
  if(!is.null(disaster.start) | !is.null(disaster.end)){
    eval(parse(text=paste("rect(as.Date('", ifelse(is.null(disaster.start),time.start,disaster.start), "','%Y-%m-%d'),-1000, as.Date('", ifelse(is.null(disaster.end),time.end,disaster.end), "','%Y-%m-%d'), 2*max(codetrends[,-1:-2]), col = rgb(202/255,255/255,112/255,.5))", sep="")))    
  }
  
  # add legend  
  allsubs<-as.character(subnames[1])
  allcol<-as.character(col[1])
  alllty<-as.character(lty[1])
  for(i in 2:length(subnames)){
    allsubs<-paste(allsubs, subnames[i], sep="','")
    allcol<-paste(allcol, col[i], sep="','")
    alllty<-paste(alllty, lty[i], sep=",")
  }
  
  eval(parse(text=paste("legend('topright', legend=c('", allsubs, "'), col=c('", allcol, "'), lty=c(", alllty, "), title='Events')", sep="")))
  
  eval(parse(text=paste("text(x=as.Date('", middate(ifelse(is.null(disaster.start),time.start,disaster.start), ifelse(is.null(disaster.end),time.end,disaster.end)), "','%Y-%m-%d'), y=1.35*max(codetrends[,-1:-2]), labels = '", disaster.name, "', col = rgb(34/255,139/255,34/255,1))", sep="")))
  dev.off()
}


