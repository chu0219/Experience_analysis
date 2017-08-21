source("G:/shared/Project Vita/R_Projects_Output/SY_Projects/Industry Effects/Experience analysis - functions.R")

# Packages to load and options ------------------------------------------------------

library(fastmatch)
library(lubridate)
library(data.table)
library(profvis)
options(datatable.nomatch = 0)

# Set parameters ---------------------------------------------------------------------

# Years to include in analysis
minYear <- 2010
maxYear <- 2010

# Ages to include in analysis
minimumAgeToInclude <- 60
maximumAgeToInclude <- 95

# Remove dependents?
removeMaleDependants <- TRUE
removeFemaleDependants <- FALSE

dateToRevalTo <- as.Date("2013-07-01", format = "%Y-%m-%d")

pensionRevalMethod <- "RPII"
ppnRPIITables <- fread("G:/shared/Project Vita/R_Projects_Output/JM_Projects/Sheffield/RPIIProportionTable.csv")
indexTables <- fread("G:/shared/Project Vita/R_Projects_Output/JM_Projects/Sheffield/IndexTables.csv")

useVitaCurves <- TRUE

DT <- fread("//hrglafs01/level2access$/VitaExchange/Sheffield/ResearchExtract20160809/VitaExchangeExtract_09-08-2016@10-06-11.27_CalibID_35_Srate_1_Smod_0_hrglavita03.csv", nrow=100000)

if(removeMaleDependants){DT <- DT[Pensionertype=="P" | gender=="F", ]}
if(removeFemaleDependants){DT <- DT[Pensionertype=="P" | gender=="M", ]}

DT <- format.relevant.dates.in.data(DT)

#Merge exchange data with schemes
schemeData <- fread("//hrglafs01/level2access$/VitaExchange/Sheffield/ResearchExtract20160809/Sheffield Extract 281 Scheme Decode (INTERNAL USE ONLY).csv")
setnames(schemeData, old="formalname", new="Scheme")
DT <- merge(DT, schemeData, by="MemberIdentity", all.x=TRUE)

DT <- add.in.force.dates(DT)
DT <- handle.missing.pension.dates(DT)
DT <- revalue.pensions.in.data(DT, indexTables, ppnRPIITables, dateToRevalTo, SchemeInForceDate)
DT <- handle.date.pension.ceased(DT, keepDeathsWithin1YrCease = TRUE) #Ignore false option for now, needs some thinking

DT[, FPSusedToCalcBens := as.numeric(FPSusedToCalcBens)]
DT <- revalue.salaries.in.data(DT, dateToRevalTo, indexTables)

#Merge exchange data with industries
industryData <- fread("//hrglafs01/level3access$/VitaITTracker/SupportingDocuments/SR 2488 - Sheffield Industry Code/20160912 Member to Industry Decode for Sheffield Extract 281.csv")
industryData <- industryData[,list(MemberIdentity, ICBIndustry)]
DT <- merge(DT, industryData, by="MemberIdentity", all.x=TRUE)

setkey(DT, ICBIndustry)
industryMapping <- data.table(ICBIndustry = c("0001", "1000", "2000", "3000", "5000",
                                              "7000", "8000", "9000", "GOVT", "LGPS", "MISC"),
                              Industry = c("Oil & Gas", "Basic Materials", "Industrials", "Consumer Goods",
                                           "Consumer Services", "Utilities", "Financials", "Technology",
                                           "Government", "Local Authority", "Miscellanious"))
DT <- merge(DT, industryMapping, by="ICBIndustry")

#Added to get na dates removed for LGEUD/PenEUD, and ensure existing bad QF do not have defined EUDs for covariates
DT[AdrExtFilter_Qualityflag == "B" | is.na(LGEUD), c("AdrExtFilter_Qualityflag","LGEUD") := list("B",NA)]
#DT[SalExtFilter_Qualityflag == "B" | is.na(LGEUD), c("SalExtFilter_Qualityflag","LGEUD") := list("B",NA)]
DT[,SalEUD:=PenEUD] #Temporary
DT[PenExtFilter_Qualityflag == "B" | is.na(PenEUD), c("PenExtFilter_Qualityflag","PenEUD") := list("B",NA)]

#Read in mortality tables
standardTables <- fread("//hrlonfs01/dept/shared/Project Vita/R_Projects_Output/SY_Projects/Industry Effects/S2Tables.csv", header = TRUE)
setnames(standardTables,old=c("Age"), new=c("ageNearInYOE"))

DT[, RetirementHealth := ifelse(RetirementHealth=="0", "N", ifelse(RetirementHealth=="9", "I", "A"))]

if(useVitaCurves){
  vitaCurveDir <- "V:/ROutput/DataRead/2016-02-03DataRead_CV16Fit/VitaCurvesExport/230316AllFullVer1/ExtendedCurves230316AllFullVer1_230316/Average220416"
  vitaCurves <- read.in.vita.curves(vitaCurveDir)
  allMortalityTables <- merge(standardTables, vitaCurves, by="ageNearInYOE")
}else{
  allMortalityTables <- standardTables
}

allMortalityTables <- melt(allMortalityTables,id.vars="ageNearInYOE",variable.name="MortalityTable",value.name="qx")

#Read in improvement tables (CMI13)
CMI13_M_0pctLTR <- fread("//hrlonfs01/dept/shared/Project Vita/R_Projects_Output/SY_Projects/Industry Effects/Cumulative Rates Males.csv", header = TRUE)
CMI13_F_0pctLTR <- fread("//hrlonfs01/dept/shared/Project Vita/R_Projects_Output/SY_Projects/Industry Effects/Cumulative Rates Females.csv", header = TRUE)

#Map to mortality tables
DT <- map.members.to.standard.table(DT)

if(useVitaCurves){
  #Merge exchange data with postcodes
  postcodeData <- fread("//hrglafs01/level2access$/VitaExchange/Sheffield/ResearchExtract20160809/20160809 Postcodes for Exchange Extract 281.csv")
  postcodeData <- postcodeData[,list(V2,V3)]
  setnames(postcodeData, c("MemberIdentity", "CompressedPostcode"))
  DT <- merge(DT, postcodeData, by="MemberIdentity", all.x=TRUE)
  
  postcodeAcornMapping <- fread("G:/shared/Project Vita/R_Projects_Output/SY_Projects/Industry Effects/20150608 CV15 ACORN mappings.csv")
  setnames(postcodeAcornMapping,c("CompressedPostcode","AcornType","MaleLG","FemaleLG","VersionKey"))
  penBandCutoffs <- fread("G:/shared/Project Vita/R_Projects_Output/SY_Projects/Industry Effects/PenBandCutoffs.csv")
  salBandCutoffs <- fread("G:/shared/Project Vita/R_Projects_Output/SY_Projects/Industry Effects/SalBandCutoffs.csv")
  
  DT[SalExtFilter_Qualityflag=="NULL" | is.na(SalExtFilter_Qualityflag), SalExtFilter_Qualityflag := "B"]
  
  DT[, ManualClsn := ifelse(ManualClsn %in% c("O","M"), ManualClsn, NA)]
  
  if(!("MaleLG" %in% colnames(DT))){
    DT <- merge(DT, postcodeAcornMapping, by="CompressedPostcode", all.x=TRUE)
    DT[AdrExtFilter_Qualityflag=="G" & gender=="M" & (MaleLG=="NULL" | is.na(MaleLG)), AdrExtFilter_Qualityflag:="B"]
    DT[AdrExtFilter_Qualityflag=="G" & gender=="F" & (FemaleLG=="NULL" | is.na(FemaleLG)), AdrExtFilter_Qualityflag:="B"]
  }
  
  DT <- map.members.to.vita.curve(DT, "CV16", salBandCutoffs, penBandCutoffs)
}

startTime <- Sys.time()
timeOfRunning <- gsub(":","-",as.character(Sys.time()))
annualResults <- lapply(minYear:maxYear, 
                        function(x) DT[, calculate.ETR.in.year(.SD, x, allMortalityTables, 2013,
                                                               CMI13_M_0pctLTR, CMI13_F_0pctLTR,
                                                               variablesToSplitBy = c("Industry","Scheme"),
                                                               minimumAgeToInclude,maximumAgeToInclude,
                                                               splitByAgeGroup = TRUE, useLUY = TRUE,
                                                               amountsBasis = FALSE, useVitaCurves = TRUE)])

annualResults <- rbindlist(annualResults)
endTime <- Sys.time()
print(endTime - startTime)

resultsBySchemeAndIndustry <- aggregate.results(annualResults, aggregateBy=c("Industry","Scheme"))
resultsByIndustry <- aggregate.results(annualResults, aggregateBy=c("Industry","AgeGroup"), includeCrudeLE=TRUE)
setnames(resultsByIndustry, old="InitialETR", new="IndustryInitialETR")
resultsBySchemeAndIndustry <- merge(resultsBySchemeAndIndustry, resultsByIndustry[,list(Industry,IndustryInitialETR)], by=c("Industry"))
resultsBySchemeAndIndustry[, SchemePropnOfETR := InitialETR/IndustryInitialETR]
resultsBySchemeAndIndustry[, CrudeRate := Observed / InitialETR]

resultsDirLabel <- "Industry Effects Analysis"
resultsDir <- "//hrglafs01/depts/RMC/Life/3 Prospecting and Proposition development/Project Sheffield/R output"
resultsFolder <- file.path(resultsDir, paste(timeOfRunning, resultsDirLabel, sep=" "))
if(!dir.exists(resultsFolder)){dir.create(resultsFolder)}