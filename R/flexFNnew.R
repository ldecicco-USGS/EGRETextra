#' Flexible Flow Normalization
#' 
#' @param eList named list with at least the Daily, Sample, and INFO dataframes
#' @param sampleSegStart integer vector of start years (water) for each FN conc/flux segment
#' @param flowSegStart integer vector of start years (water) for flow normalization
#' @param flowSegEnd integer vector of end years (water) for flow normalization
#' 
#' @export
#' @import EGRET
#' @examples
#' library(EGRET)
#' eList <- Choptank_eList
#' eList <- setUpEstimation(eList)
#' sampleSegStart <- c(1980,1990,2000)
#' flowSegStart <- c(1980,1985,1992)
#' flowSegEnd <- c(1994,2004,2011)
#' eList <- flexFN(eList,sampleSegStart, flowSegStart, flowSegEnd)
#' plotFluxHist(eList)
flexFN <- function(eList, sampleSegStart, flowSegStart, flowSegEnd){
  
  dateInfo <- data.frame(sampleSegStart, flowSegStart, flowSegEnd)
  
  Daily <- eList$Daily
  Sample <- eList$Sample
  
  Sample <- calcWY(Sample)
  Daily <- calcWY(Daily)
  
  dateInfo$sampleSegEnd <- c(dateInfo$sampleSegStart[2:nrow(dateInfo)]-1,max(Sample$WaterYear))

  DailyFN <- estDailyFromSurfaces(eList)
  DailyFN$FNConc <- NA
  DailyFN$FNFlux <- NA
  
  DailyFN <- calcWY(DailyFN)
  
  newList <- as.egret(eList$INFO,DailyFN,Sample,eList$surfaces)
  
  for(i in seq(nrow(dateInfo))){
    dailyReturn <- estFNsegs(newList,dateInfo[i,])
    sampleSegments <- c(dateInfo$sampleSegStart[i]:dateInfo$sampleSegEnd[i])
    DailyFN$FNConc[DailyFN$WaterYear %in% sampleSegments] <- dailyReturn$FNConc[dailyReturn$WaterYear %in% sampleSegments]
    DailyFN$FNFlux[DailyFN$WaterYear %in% sampleSegments] <- dailyReturn$FNFlux[dailyReturn$WaterYear %in% sampleSegments]
  }
  
  INFO <- eList$INFO
  INFO$shortName <- paste0(INFO$shortName,"*")
  INFO$nSegments <- nrow(dateInfo)
  
  attr(INFO,"segmentInfo") <- dateInfo
  
  newList <- as.egret(INFO,DailyFN,Sample,eList$surfaces)
  
  return(newList)
  
}


#' Segment estimates
#' 
#' @param eList named list with at least the Daily, Sample, and INFO dataframes
#' @param dateInfo dataframe with sampleSegStart, flowSegStart, flowSegEnd, sampleSegEnd
#' @export
#' @import EGRET 
#' @examples
#' library(EGRET)
#' eList <- Choptank_eList
#' eList <- setUpEstimation(eList)
#' sampleSegStart <- c(1980,1990,2000)
#' flowSegStart <- c(1980,1985,1992)
#' flowSegEnd <- c(1994,2004,2011)
#' dateInfo <- dateInfo <- data.frame(sampleSegStart, flowSegStart, flowSegEnd)
#' dateInfo$sampleSegEnd <- c(dateInfo$sampleSegStart[2:nrow(dateInfo)]-1,floor(max(eList$Sample$DecYear)))
#' Daily <- calcWY(eList$Daily)
#' Sample <- calcWY(eList$Sample)
#' eList <- as.egret(eList$INFO,Daily,Sample,eList$surfaces)
#' eList <- estFNsegs(eList,dateInfo[1,])
estFNsegs <- function(eList, dateInfo){
  
  Daily <- eList$Daily
  Sample <- eList$Sample
  
  Daily <- Daily[Daily$WaterYear >= dateInfo$flowSegStart & Daily$WaterYear <= dateInfo$flowSegEnd,]
  Sample <- Sample[Sample$WaterYear >= dateInfo$sampleSegStart & Sample$WaterYear <= dateInfo$sampleSegEnd,]
  
  newList <- as.egret(eList$INFO,Daily,Sample,eList$surfaces)
  
  newDaily <- EGRET::estDailyFromSurfaces(newList)
  return(newDaily)
}

#' Calculate Water Year
#' 
#' @param df data frame with DecYear and Month columns
#' @export 
#' @examples
#' library(EGRET)
#' eList <- Choptank_eList
#' Daily <- eList$Daily
#' Daily <- calcWY(Daily)
calcWY <- function(df){
  df$WaterYear <- as.integer(df$DecYear)
  df$WaterYear[df$Month >= 10] <- df$WaterYear[df$Month >= 10] +1
  return(df)
}