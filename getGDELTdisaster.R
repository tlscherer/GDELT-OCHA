#' Download GDELT data based around a target country using GetGDELT using GDELTtools
#'
#' Download the GDELT files necessary for a data set, import them,
#' filter on various criteria, and return a data.frame. 
#' 
#' @aliases GDELTdisaster
#' @param disaster.name character, name of disaster used in naming saved files, cannot have spaces
#' @param time.start character, in the form "yyyy-mm-dd", date to start tracking events.
#' @param disaster.start character, in the form "yyyy-mm-dd", time.start to disaster.start is called 'before'
#' @param disaster.end character, in the form "yyyy-mm-dd", disaster.start to disaster.end is called 'during'
#' @param time.end character, in the form "yyyy-mm-dd", disaster.end to time.end is called 'after'.
#' @param local.folder character, if specified, where downloaded GDELT files will be saved or loaded from
#' @param data.destination character, folder where GDELT subsets will be saved for easier analysis
#' @param targetcountry character, name of country of interest
#' @param getdata logical, if TRUE then will capture GDELT data using GetGdelt
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

getGDELTdisaster <- function(disaster.name,
                          time.start=NULL, 
                          disaster.start=NULL,
                          disaster.end=NULL,
                          time.end=NULL,
                          local.folder,
                          data.destination,
                          targetcountry,
                          verbose=TRUE,
                          lite=FALSE){

  memory.limit(size=100000000000)
  
  #### Load subfunctions to be used during function ######
  
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
  
  # load necessary packages
  instant_pkgs(c("GDELTtools","countrycode")) 
  
  #### Identify country using the countrycode package
  iso3c<-countrycode(targetcountry, "country.name", "iso3c")
  fips104<-countrycode_data$fips104[na.omit(countrycode_data$iso3c==iso3c)]
  
  #### Load Datasets ################################################

    # Get data from online GDELT websites and save to 
  
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
    
  if(lite==FALSE){

  assign(paste(disaster.name, "all", sep=""), GetGDELT(start.date, end.date, local.folder=local.folder))
  if(verbose) cat("Saving location data as ", data.destination, disaster.name, "loc... \n\n", sep="") 
  eval(parse(text=paste(disaster.name, "loc<-", disaster.name, "all[ which(", disaster.name, "all$ActionGeo_CountryCode=='", fips104, "'),]", sep="")))
  eval(parse(text=paste("save(", disaster.name, "loc",", file='", data.destination, disaster.name, "loc')", sep="")))
  if(verbose) cat("Saving actor1 data as ", data.destination, disaster.name, "act1... \n\n", sep="")
  eval(parse(text=paste(disaster.name, "act1<-", disaster.name, "all[ which(", disaster.name, "all$Actor1CountryCode=='", iso3c, "'),]", sep="")))
  eval(parse(text=paste("save(", disaster.name, "act1",", file='", data.destination, disaster.name, "act1')", sep="")))
  if(verbose) cat("Saving actor2 data as ", data.destination, disaster.name, "act2... \n\n", sep="") 
  eval(parse(text=paste(disaster.name, "act2<-", disaster.name, "all[ which(", disaster.name, "all$Actor2CountryCode=='", iso3c, "'),]", sep="")))
  eval(parse(text=paste("save(", disaster.name, "act2",", file='", data.destination, disaster.name, "act2')", sep="")))
  
  }
  
  if(lite==TRUE){  
  
  
      if(verbose) cat("Saving location data as ", data.destination, disaster.name, "loc... \n\n", sep="") 
      assign(paste(disaster.name, "loc", sep=""), GetGDELT(start.date, end.date, local.folder=local.folder, filter=list(ActionGeo_CountryCode=c(fips104))))
      eval(parse(text=paste("save(", disaster.name, "loc",", file='", data.destination, disaster.name, "loc')", sep="")))
      
      if(verbose) cat("Saving actor1 data as ", data.destination, disaster.name, "act1... \n\n", sep="")
      assign(paste(disaster.name, "act1", sep=""), GetGDELT(start.date, end.date, local.folder=local.folder, filter=list(Actor1CountryCode=c(iso3c))))
      eval(parse(text=paste("save(", disaster.name, "act1",", file='", data.destination, disaster.name, "act1')", sep="")))
      
      if(verbose) cat("Saving actor2 data as ", data.destination, disaster.name, "act2... \n\n", sep="") 
      assign(paste(disaster.name, "act2", sep=""), GetGDELT(start.date, end.date, local.folder=local.folder, filter=list(Actor2CountryCode=c(iso3c))))
      eval(parse(text=paste("save(", disaster.name, "act2",", file='", data.destination, disaster.name, "act2')", sep="")))
  
  }
  
} # end function
######






