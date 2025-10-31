######################################################################################################################## 
#' @title suvaNEON

#' @author Bobby Hensley \email{hensley@battelleecology.org} \cr 

#' @description This script calculates Specific Ultra-Violet Absorbance (SUVA)
#' from NEON surface water chemistry data.

#' @return This script produces a .csv file 

#' @references 
#' License: GNU AFFERO GENERAL PUBLIC LICENSE Version 3, 19 November 2007

# changelog and author contributions / copyrights
#   Bobby Hensley (10/21/2025)
#     Original script created
######################################################################################################################## 
library(neonUtilities)
library(plyr)
library(ggplot2)
######################################################################################################################## 

siteID<-"COMO"

#' Pulls surface water chemistry data from NEON data portal and loads tables into R environment
swc<-neonUtilities::loadByProduct(dpID="DP1.20093.001", site=siteID, startdate="2023-10",enddate="2025-09", 
                                  package="expanded", include.provisional=TRUE, check.size = F)
list2env(swc,.GlobalEnv)

#' Averages decadicAbsorbance from "A" and "B" spectrum replicates
swc_externalLabAbsorbanceScan$sampleID.wavelength<-paste(swc_externalLabAbsorbanceScan$sampleID, swc_externalLabAbsorbanceScan$wavelength, sep=".")
swc_fullUV<-plyr::ddply(swc_externalLabAbsorbanceScan,c("sampleID.wavelength"),summarise,sampleID=unique(sampleID),domainID=unique(domainID),siteID=unique(siteID),
                        collectDate=unique(collectDate),wavelength=unique(wavelength),absorbance=mean(decadicAbsorbance)) 

#' Combines DOC and Abs254 values into wide format using sampleID's
swc_DOC<-swc_externalLabDataByAnalyte[(swc_externalLabDataByAnalyte$analyte=="DOC"),]
swc_DOC<-swc_DOC[,c("sampleID","analyteConcentration")]
colnames(swc_DOC)<-c("sampleID","DOC")
swc_fullUV<-merge(swc_fullUV, swc_DOC,by.x="sampleID",by.y="sampleID")

#' Calculates specific ultra-violet absorbance (units - L/mg-m)
swc_fullUV$SUVA<-swc_fullUV$absorbance/swc_fullUV$DOC*100 

#' Writes out csv file of results
write.csv(swc_suva,file=paste0(siteID,".SUVA.csv"))

##############################################################################################################################################################################
plot<-ggplot(swc_fullUV,aes(x=wavelength,y=SUVA, colour=sampleID))+
  geom_line()+scale_x_continuous(limits = c(200, 400))+scale_y_continuous(limits = c(0, 10))
plot
