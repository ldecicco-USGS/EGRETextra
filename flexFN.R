#' Flexible Flow Normalization
#' 
#' @param eList named list with at least the Daily, Sample, and INFO dataframes
#' @param nSegments integer number of flow normalized segments to create
#' @param segStart integer vector of start years (water) for each FN conc/flux segment
#' @param segEnd integer vector of end years (water) for each FN conc/flux segment
#' @param dStart Date vector of start days for each flow segment
#' @param dEnd Date vector of end days for each flow segment
#' 
#' @export
#' @importFrom EGRET as.egret
#' @importFrom EGRET estDailyFromSurfaces
#' @examples
#' library(EGRET)
#' eList <- Choptank_eList
#' eList <- setUpEstimation(eList)
#' nSegments <- 2
#' segStart <- c(1985,2000)
#' segEnd <- c(2001,2010)
#' dStart <- as.Date(c("1988-10-01","2002-05-15"))
#' dEnd <- as.Date(c("1998-09-30","2009-09-30"))
#' eList <- flexFN(eList,nSegments,segStart,segEnd,dStart,dEnd)
#' plotFluxHist(eList)
flexFN <- function(eList, nSegments, segStart, segEnd,dStart,dEnd){
  
  dateInfo <- data.frame(segStart,segEnd,dStart,dEnd)
  
  if(nrow(dateInfo) != nSegments){
    stop("Length of segStart and segEnd must equal nSegments")
  }
  
  Daily <- eList$Daily
  Sample <- eList$Sample
  
  Sample$WaterYear <- as.integer(Sample$DecYear)
  Sample$WaterYear[Sample$Month >= 10] <- Sample$WaterYear[Sample$Month >= 10] +1
  
  Daily$WaterYear <- as.integer(Daily$DecYear)
  Daily$WaterYear[Daily$Month >= 10] <- Daily$WaterYear[Daily$Month >= 10] +1
  
  yStartSample <- min(Sample$WaterYear)
  yEndSample <- max(Sample$WaterYear)
  
  yStartDaily <- min(Daily$WaterYear)
  yEndDaily <- max(Daily$WaterYear)
  
  checkWY <- function(dates, yStart, yEnd){
    segWY <- as.integer(format(dates,"%Y"))
    segMonth <- as.integer(format(dates,"%m"))
    segWY[segMonth > 9] <- segWY[segMonth > 9]+1
    
    if(segWY[1] < yStart | segWY[length(segWY)] > yEnd){
      stop("Flow segment dates outside range")
    }
    return(segWY)
  }
  
  if(segStart[1] < yStartSample | segEnd[length(segEnd)] > yEndSample){
    stop("Sample segment years outside range")
  }
  
  if(nSegments>1){
    for(i in (seq(nSegments-1)+1)){
      if(segStart[i] != segEnd[i-1] -1){
        stop("segStart and segEnd need to be continuous")
      }
    }
  }
  
  dStartWY <- checkWY(dStart, yStartDaily, yEndDaily)  
  dEndWY <- checkWY(dEnd, yStartDaily, yEndDaily)
  
  if(any(as.integer(dEnd - dStart) < 365)){
    stop("Each flow normaliziation section must span at least 365 days")
  }
  
  DailyFN <- estDailyFromSurfaces(eList)
  DailyFN$FNConc <- NULL
  DailyFN$FNFlux <- NULL
  
  newList <- as.egret(eList$INFO,DailyFN,Sample,eList$surfaces)
  
  for(i in seq(nSegments)){
    dailyReturn <- estFNsegs(newList,segStart[i],segEnd[i],dStart[i],dEnd[i])
    DailyFN <- merge(DailyFN, dailyReturn[,c("Date","FNConc","FNFlux")],by=c("Date"),all.x=TRUE)
  }
  FNConcCols <- grep("FNConc",names(DailyFN))
  FNConcCols <- names(DailyFN)[FNConcCols]
  
  FNFluxCols <- grep("FNFlux",names(DailyFN))
  FNFluxCols <- names(DailyFN)[FNFluxCols]
  
  DailyFN$FNConc <- rowSums(DailyFN[,FNConcCols],na.rm = TRUE)
  DailyFN$FNFlux <- rowSums(DailyFN[,FNFluxCols],na.rm = TRUE)
  
  DailyFN[rowSums(is.na(DailyFN[,FNConcCols]))==length(FNConcCols),"FNConc"] <- NA
  DailyFN[rowSums(is.na(DailyFN[,FNFluxCols]))==length(FNFluxCols),"FNFlux"] <- NA
  
  DailyFN <- DailyFN[,!(names(DailyFN) %in% c(FNConcCols,FNFluxCols))]
  
  INFO <- eList$INFO
  INFO$shortName <- paste0(INFO$shortName,"*")
  INFO$nSegments <- nSegments
  
  attr(INFO,"segmentInfo") <- dateInfo
  
  newList <- as.egret(INFO,DailyFN,Sample,eList$surfaces)
  
  return(newList)
  
}


#' Segment estimates
#' 
#' @param eList named list with at least the Daily, Sample, and INFO dataframes
#' @param segStart_i integer vector of start years (water) for each FN conc/flux segment
#' @param segEnd_i integer vector of end years (water) for each FN conc/flux segment
#' @param dStart_i Date vector of start days for each flow segment
#' @param dEnd_i Date vector of end days for each flow segment
#' 
#' @export
#' @importFrom EGRET estDailyFromSurfaces
#' @importFrom EGRET as.egret
#' @examples
#' library(EGRET)
#' eList <- Choptank_eList
#' eList <- setUpEstimation(eList)
#' nSegments <- 2
#' segStart <- c(1985,2000)
#' segEnd <- c(2001,2010)
#' dStart <- as.Date(c("1988-10-01","2002-05-15"))
#' dEnd <- as.Date(c("1998-09-30","2009-09-30"))
#' eList <- estFNsegs(eList,segStart[1],segEnd[1],dStart[1],dEnd[1])
estFNsegs <- function(eList, segStart_i,segEnd_i,dStart_i,dEnd_i){
  
  Daily <- eList$Daily
  Sample <- eList$Sample
  
  Daily <- Daily[Daily$Date >= dStart_i & Daily$Date <= dEnd_i,]
  
  Sample$WaterYear <- as.integer(Sample$DecYear)
  Sample$WaterYear[Sample$Month >= 10] <- Sample$WaterYear[Sample$Month >= 10] +1
  Sample <- Sample[Sample$WaterYear >= segStart_i & Sample$WaterYear <= segEnd_i,]
  
  newList <- as.egret(eList$INFO,Daily,Sample,eList$surfaces)
  
  newDaily <- estDailyFromSurfaces(newList)
  return(newDaily)
}