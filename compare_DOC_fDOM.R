######################################################################################################################## 
#' @title compare_DOC_fDOM

#' @author Bobby Hensley \email{hensley@battelleecology.org} \cr 

#' @description This script compares DOC from NEON surface water chemistry data
#' with fDOM from NEON water quality data.  This can be useful for generating
#' continuous estimates of DOC from fDOM sensor time-series data.

#' @return This script produces a .csv file 

#' @references 
#' License: GNU AFFERO GENERAL PUBLIC LICENSE Version 3, 19 November 2007

# changelog and author contributions / copyrights
#   Bobby Hensley (11/26/2025)
#     Original script created
######################################################################################################################## 
library(neonUtilities)
library(plyr)
library(lubridate)
library(ggplot2)
######################################################################################################################## 

siteID<-"COMO"

#' Pulls surface water chemistry data from NEON data portal and loads tables into R environment
swc<-neonUtilities::loadByProduct(dpID="DP1.20093.001", site=siteID, startdate="2014-10",enddate="2025-09", 
                                  package="expanded", include.provisional=TRUE, check.size = FALSE)
list2env(swc,.GlobalEnv)
#' Parses only DOC samples and averages replicates
swc_DOC<-swc_externalLabDataByAnalyte[(swc_externalLabDataByAnalyte$analyte=="DOC"),]
swc_DOC<-plyr::ddply(swc_DOC,c("collectDate"),summarise,domainID=unique(domainID),
                     siteID=unique(siteID), DOC=mean(analyteConcentration)) 
#' Round collect date to nearest 5 min to matching with fDOM
swc_DOC$collectDate<-lubridate::round_date(swc_DOC$collectDate,unit="5 minute")

#' Pulls water quality data from NEON data portal and loads tables into R environment
waq<-neonUtilities::loadByProduct(dpID="DP1.20288.001", site=siteID, startdate="2014-10",enddate="2025-09", 
                                  package="expanded", include.provisional=TRUE, check.size = FALSE)
list2env(waq,.GlobalEnv)

#' Parses locations where fDOM measuremnts are expected
waq_fDOM<-waq_instantaneous[(waq_instantaneous$horizontalPosition=="102"|waq_instantaneous$horizontalPosition=="112"|waq_instantaneous$horizontalPosition=="103"),]
#' Parses only fDOM measurements (rawCalibrated is uses here but alternatively absorbance corrected can be used)
waq_fDOM<-waq_fDOM[,c("startDateTime","rawCalibratedfDOM")]
#' Calculates 5 min averages (Buoys will already be at 5 min)
waq_fDOM$startDateTime<-lubridate::round_date(waq_fDOM$startDateTime,unit="5 minute")
waq_fDOM<-plyr::ddply(waq_fDOM,c("startDateTime"),summarise,fDOM=mean(rawCalibratedfDOM))

#' Pair DOC and fDOM measurements using collect times
paired_data<-merge(swc_DOC,waq_fDOM,by.x="collectDate",by.y="startDateTime",all.x=TRUE,all.y=FALSE)

#' Fit linear regression to data
regression<-lm(DOC~fDOM,data=paired_data)
summary(regression)

#' Writes out csv file of results
write.csv(paired_data,file=paste0(siteID,".DOCvfDOM.csv"))

##############################################################################################################################################################################
plot<-ggplot(paired_data,aes(x=fDOM,y=DOC))+
  geom_point()+
  stat_smooth(method="lm",formula=y~x,geom="smooth")
plot
