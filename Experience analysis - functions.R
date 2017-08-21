
# Packages to load and options ----------------------------------------------------------------------------

library(fastmatch)
library(lubridate)
library(data.table)
library(profvis)
options(datatable.nomatch = 0)

# Mortality table mapping ---------------------------------------------------------------------

read.in.vita.curves <- function(curvesDir){
  allFiles <- list.files(curvesDir)
  relevantFiles <- allFiles[grep(".csv",allFiles)]
  
  combinedTables <- data.table(ageNearInYOE=(16:125))
  for(fileName in relevantFiles){
    currentFile <- fread(file.path(curvesDir,fileName))
    colnames(currentFile)[1] <- "ageNearInYOE"
    combinedTables <- merge(combinedTables, currentFile, by = "ageNearInYOE")
  }
  
  combinedTables
}

apply.bands <- function(gender, pensionerType, affluenceAmount, bandCutoffsTable){
  memberType <- paste0(gender, pensionerType)
  bandCutoffs <- bandCutoffsTable[,get(memberType)]
  bandCutoffs <- bandCutoffs[!is.na(bandCutoffs)]
  memberBands <- cut(affluenceAmount, bandCutoffs, labels=1:(length(bandCutoffs)-1))
  memberBands
}

map.members.to.vita.curve <- function(DT, TargCV, salBandCutoffs, penBandCutoffs){
  
  setkey(DT, gender, Pensionertype, RetirementHealth,
         AdrExtFilter_Qualityflag, SalExtFilter_Qualityflag, PenExtFilter_Qualityflag)
  
  DT[, MortGroup := NA_character_]
  DT[CJ("M",c("P","W"), c("N","I","A"), "G"), MortGroup := MaleLG]
  DT[CJ("F",c("P","W"), c("N","I","A"), "G"), MortGroup := FemaleLG]
  
  DT[, SalaryBand := NA_character_]
  DT[CJ("M","P","N",c("G","B"),"G"), SalaryBand := apply.bands(gender[1],Pensionertype[1], RevaluedSalary, salBandCutoffs)]
  DT[CJ("M","P","I",c("G","B"),"G"), SalaryBand := apply.bands(gender[1],Pensionertype[1], RevaluedSalary, salBandCutoffs)]
  DT[CJ("M","P","A",c("G","B"),"G"), SalaryBand := apply.bands(gender[1],Pensionertype[1], RevaluedSalary, salBandCutoffs)]
  DT[CJ("M","W","A",c("G","B"),"G"), SalaryBand := apply.bands(gender[1],Pensionertype[1], RevaluedSalary, salBandCutoffs)]
  DT[CJ("F","P","N",c("G","B"),"G"), SalaryBand := apply.bands(gender[1],Pensionertype[1], RevaluedSalary, salBandCutoffs)]
  DT[CJ("F","P","I",c("G","B"),"G"), SalaryBand := apply.bands(gender[1],Pensionertype[1], RevaluedSalary, salBandCutoffs)]
  DT[CJ("F","P","A",c("G","B"),"G"), SalaryBand := apply.bands(gender[1],Pensionertype[1], RevaluedSalary, salBandCutoffs)]
  DT[CJ("F","W","A",c("G","B"),"G"), SalaryBand := apply.bands(gender[1],Pensionertype[1], RevaluedSalary, salBandCutoffs)]
  
  DT[, PensionBand := NA_character_]
  DT[CJ("M","P","N",c("G","B"),c("G","B"),"G"), PensionBand := apply.bands(gender[1],Pensionertype[1], RevaluedPension, penBandCutoffs)]
  DT[CJ("M","P","I",c("G","B"),c("G","B"),"G"), PensionBand := apply.bands(gender[1],Pensionertype[1], RevaluedPension, penBandCutoffs)]
  DT[CJ("M","P","A",c("G","B"),c("G","B"),"G"), PensionBand := apply.bands(gender[1],Pensionertype[1], RevaluedPension, penBandCutoffs)]
  DT[CJ("M","W","A",c("G","B"),c("G","B"),"G"), PensionBand := apply.bands(gender[1],Pensionertype[1], RevaluedPension, penBandCutoffs)]
  DT[CJ("F","P","N",c("G","B"),c("G","B"),"G"), PensionBand := apply.bands(gender[1],Pensionertype[1], RevaluedPension, penBandCutoffs)]
  DT[CJ("F","P","I",c("G","B"),c("G","B"),"G"), PensionBand := apply.bands(gender[1],Pensionertype[1], RevaluedPension, penBandCutoffs)]
  DT[CJ("F","P","A",c("G","B"),c("G","B"),"G"), PensionBand := apply.bands(gender[1],Pensionertype[1], RevaluedPension, penBandCutoffs)]
  DT[CJ("F","W","A",c("G","B"),c("G","B"),"G"), PensionBand := apply.bands(gender[1],Pensionertype[1], RevaluedPension, penBandCutoffs)]
  
  DT[, Occupation := NA_character_]
  DT[, Occupation := ifelse(ManualClsn=="O" | ManualClsn=="N", ManualClsn, NA)]
  
  bandPeriod <- paste0(as.character(as.numeric(substr(TargCV,3,4)) - 4), as.character(as.numeric(substr(TargCV,3,4)) - 2))
  DT[, VitaCurve := paste0(TargCV,"v1_",bandPeriod,"_",gender,Pensionertype,RetirementHealth,"a")]
  DT[!is.na(MortGroup), VitaCurve := paste0(VitaCurve, "lg", MortGroup)]
  
  DT[CJ("M",c("P","W"),c("N","I","A"),c("G","B"),"G","G"), VitaCurve := paste0(VitaCurve, "sb", SalaryBand)]
  DT[CJ("F",c("P","W"),c("N","I","A"),c("G","B"),"G","G"), VitaCurve := paste0(VitaCurve, "pb", PensionBand)]
  
  DT[CJ(c("M","F"),c("P","W"),c("N","I","A"),c("G","B"),"G","B"), VitaCurve := paste0(VitaCurve, "sb", SalaryBand)]
  DT[CJ(c("M","F"),c("P","W"),c("N","I","A"),c("G","B"),"B","G"), VitaCurve := paste0(VitaCurve, "pb", PensionBand)]
  
  DT[!is.na(Occupation), VitaCurve := paste0(VitaCurve, "oc", Occupation)]
  
  return(DT)
}

map.members.to.standard.table <- function(DT){
  DT[, StandardTable := paste0("S2", Pensionertype, gender, ifelse(amountsBasis,"A","L"))]
}

# Crude life expectancy calculation -----------------------------------------------------------
####Remove the function below and replace with sourcing in life expectancy script
calculate.crude.LE.by.age <- function(AgeGroup, NumberOfDeaths, Exposure, 
                                      agesToReport = NULL, confidenceLevel = 0.95, 
                                      ageGroupSize = 5) {
  summaryTable <- data.table(AgeGroup,NumberOfDeaths,Exposure)
  firstAgeInEachGroup <- as.numeric(substr(AgeGroup,2,3))
  groupDifferencesAsExpected <- diff(firstAgeInEachGroup)==ageGroupSize
  
  #agesToReport <- agesToReport[agesToReport %in% firstAgeInEachGroup]
  
  if(all(groupDifferencesAsExpected)){
    pointOfDeath <- 0.5
    highestAgeGroup<-nrow(summaryTable) # number of ages/ age groups in CovProf==j
    #if(highestAgeGroup==numGr){                    
    summaryTable[-highestAgeGroup, numberOfAgesInGroup := ageGroupSize ]
    summaryTable[highestAgeGroup, numberOfAgesInGroup := (ageGroupSize + 1)]
    
    summaryTable[,ageGroupPx := (1 - NumberOfDeaths/Exposure) ^ numberOfAgesInGroup]
    summaryTable[,ageGroupQx := 1 - ageGroupPx]
    
    summaryTable[, Mx := ageGroupQx / (numberOfAgesInGroup * (1 - (1-pointOfDeath) * ageGroupQx))]
    
    summaryTable[,POPx.AG := Exposure - (1-pointOfDeath) * NumberOfDeaths]
    
    summaryTable[,probOfSurvivalToAgeGroup := c(1,cumprod(ageGroupPx[-highestAgeGroup]))]
    summaryTable[,lx.AG := 100000*probOfSurvivalToAgeGroup]
    
    summaryTable[highestAgeGroup, dx.AG := lx.AG * ageGroupQx]    
    deathsInEachAgeGroup <- summaryTable[, -diff(lx.AG)]
    summaryTable[-highestAgeGroup, dx.AG := deathsInEachAgeGroup]
    
    summaryTable[,Lx.AG := numberOfAgesInGroup * (lx.AG - (1-pointOfDeath) * dx.AG)]
    summaryTable[,Tx.AG := rev(cumsum(rev(Lx.AG)))]
    summaryTable[,LEx.AG := Tx.AG/lx.AG]
    summaryTable[,VarQx.AG := ((numberOfAgesInGroup ^ 2) * Mx 
                               * (1 - pointOfDeath * numberOfAgesInGroup * Mx)) / 
                   (POPx.AG * (1 + (1 - pointOfDeath) * numberOfAgesInGroup * Mx) ^ 3)]
    
    shiftedLEx.AG <- summaryTable[-1,LEx.AG]
    summaryTable[-highestAgeGroup, LEVarAux := lx.AG^2 
                 * (((1-pointOfDeath) * numberOfAgesInGroup 
                     + shiftedLEx.AG)^2)
                 * VarQx.AG]
    
    summaryTable[highestAgeGroup, LEVarAux := lx.AG^2 
                 * ((1 - pointOfDeath) * numberOfAgesInGroup)^2 
                 * VarQx.AG ]
    
    summaryTable[,VarLEx.AG := rev(cumsum(rev(LEVarAux)) / rev(lx.AG ^ 2))]
    summaryTable[,sdLEx.AG := sqrt(VarLEx.AG)]
    
    summaryTable[,LExMin95.AG := LEx.AG - qnorm((1 + confidenceLevel) / 2) * sdLEx.AG]
    summaryTable[,LExMax95.AG := LEx.AG + qnorm((1 + confidenceLevel) / 2) * sdLEx.AG]
    
    if(!is.null(agesToReport)) {
      summaryTable <- summaryTable[(floor(as.numeric(substr(AgeGroup, 2, 3)) / ageGroupSize)
                                    * ageGroupSize) %in% agesToReport]
      if(nrow(summaryTable)==0) {
        message("Only LE for the lowest age in any group can be reported")
      }
    }
  } else {
    message("At least one age group has zero exposure, in at least one covariate profile.")
    message("Returning NA")
    summaryTable[, c("numberOfAgesInGroup", "ageGroupPx", "ageGroupQx", "Mx", 
                     "POPx.AG", "probOfSurvivalToAgeGroup", "lx.AG", "dx.AG", 
                     "Lx.AG", "Tx.AG", "LEx.AG", "VarQx.AG", "LEVarAux", 
                     "VarLEx.AG", "sdLEx.AG", "LExMin95.AG", "LExMax95.AG") := NA_real_, 
                 with = FALSE]
  } 
  
  return(summaryTable)
}

# Date formatting -----------------------------------------------------------------------------

format.date <- function(date) {
  # Formats date of type e.g. "01 Jan 2000" as date object
  year <- substr(date, 8, 11)
  month <- as.character(fmatch(substr(date, 4, 6), month.abb))
  month <- ifelse(nchar(month) == 1, paste("0", month, sep = ""), month)
  day <- substr(date, 1, 2)
  return(as.Date(fast_strptime(paste(year, month, day, sep = "-"), format = "%Y-%m-%d")))
}

format.relevant.dates.in.data <- function(DT){
  DT[, dob := format.date(dob)]; print("Formatting dob")
  DT[, dod := format.date(dod)]; print("Formatting dod")
  DT[, EUD := format.date(EUD)]; print("Formatting EUD")
  DT[, LUD := format.date(LUD)]; print("Formatting LUD")
  DT[, DatePensionCommenced := format.date(DatePensionCommenced)]; print("Formatting DatePensionCommenced")
  DT[, DatePenCeased := format.date(DatePenCeased)]; print("Formatting DatePenCeased")
  DT[, PensionDate := format.date(PensionDate)]; print("Formatting PensionDate")
  DT[, salaryDate := format.date(salaryDate)]; print("Formatting salaryDate")
  DT[, PenEUD := format.date(PenEUD)]; print("Formatting penEUD")
  DT[, LGEUD := format.date(LGEUD)]; print("Formatting LGEUD")
  DT
}

# Handle missing and suspicious dates ---------------------------------------------------------

#Deaths with no pension date: assume DOD. Lives with no pension date: assume modal pension date of all lives.
handle.missing.pension.dates <- function(DT){
  DT[is.na(PensionDate) & !is.na(dod), PensionDate := dod]
  #The next line has been changed from Sheffield: now using in-force date as override (data should all be good though)
  PensionDateTable <- DT[!is.na(PensionDate) & is.na(dod), PensionDate := SchemeInForceDate]
  DT
}

#Set date pension ceased to 1/1/3000 where (1) undefined, or (2) DOD within 1 year of date pension ceased
handle.date.pension.ceased <- function(DT, keepDeathsWithin1YrCease = TRUE){
  
  if(keepDeathsWithin1YrCease){
    DT[is.na(DatePenCeased) | (!is.na(dod) & as.numeric(difftime(dod, DatePenCeased, units = "days")) < 365.25), 
       DatePenCeased := as.Date("3000-01-01", format = "%Y-%m-%d")]
  }else{
    DT[is.na(DatePenCeased) | (!is.na(dod) & DatePenCeased > dod), 
       DatePenCeased := as.Date("3000-01-01", format = "%Y-%m-%d")]
  }
  DT
}

#Get in-force date for each scheme
calculate.modal.date <- function(values){
  countTable <- table(values)
  modalValue <- as.Date(names(countTable)[which.max(countTable)], format = "%Y-%m-%d")
  modalValue
}

#Add scheme in-force dates
add.in.force.dates <- function(DT){
  schemeInForceDates <- DT[is.na(dod), calculate.modal.date(PensionDate), by=Scheme]
  DT <- merge(DT, schemeInForceDates, by="Scheme")
  setnames(DT, old="V1", new="SchemeInForceDate")
  DT
}

# Revaluation ---------------------------------------------------------------------------------

revalue.pension <- function(MemberIdentity, pensionamount, PensionDate, dateToRevalTo, SchemeInForceDate,
                            indexTables, revalMethod = "RPI", ppnRPIITables = NULL, 
                            relevantPpnColumn = NULL, dateOfRetirement = NULL) {
  
  firstIndexDate <- indexTables[, min(Month)]
  lastIndexDate <- indexTables[, max(Month)]
  
  relevantInForceDate <- pmax(pmin(SchemeInForceDate, lastIndexDate), firstIndexDate)
  firstOfInForceMonth <- as.Date(fast_strptime(paste(year(relevantInForceDate), 
                                                     month(relevantInForceDate), 
                                                     "01", sep = "-"),
                                               format = "%Y-%m-%d"))
  
  relevantPensionDate <- pmax(pmin(PensionDate, lastIndexDate), firstIndexDate)
  firstOfPensionMonth <- as.Date(fast_strptime(paste(year(relevantPensionDate), 
                                                     month(relevantPensionDate), 
                                                     "01", sep = "-"),
                                               format = "%Y-%m-%d"))
  
  
  if(revalMethod == "RPI"){
    rpiAtRevaluedDate <- indexTables[fmatch(dateToRevalTo, Month), RPI]
    rpiAtPensionDate <- indexTables[fmatch(firstOfPensionMonth, Month), RPI]
    
    revaluedPension <- pensionamount * rpiAtRevaluedDate / rpiAtPensionDate
  }
  
  if(revalMethod == "RPII"){
    
    rpiAtRevaluedDate <- indexTables[fmatch(dateToRevalTo, Month), RPI]
    rpiAtInForceDate <- indexTables[fmatch(firstOfInForceMonth, Month), RPI]
    
    rpiiAtInForceDate <- indexTables[fmatch(firstOfInForceMonth, Month), RPII]
    rpiiAtPensionDate <- indexTables[fmatch(firstOfPensionMonth, Month), RPII]
    
    minPpnYear <- ppnRPIITables[, min(YearOfRetirement)]
    maxPpnYear <- ppnRPIITables[, max(YearOfRetirement)]
    relevantYOR <- pmax(pmin(year(dateOfRetirement), maxPpnYear), minPpnYear)
    
    proportionOfIncreases <- cbind(MemberIdentity, 
                                   ppnRPIITables[fmatch(relevantYOR, YearOfRetirement)], 
                                   relevantPpnColumn)
    proportionOfIncreases <- proportionOfIncreases[, list(relevantProportion = get(relevantPpnColumn)), by = MemberIdentity]
    
    pensionRolledUpToInForceDate <- pensionamount * (rpiiAtInForceDate / rpiiAtPensionDate) ^ proportionOfIncreases[, relevantProportion]
    revaluedPension <- pensionRolledUpToInForceDate * (rpiAtRevaluedDate / rpiAtInForceDate)
  }
  
  return(revaluedPension)
}

revalue.pensions.in.data <- function(DT, indexTables, ppnRPIITables, dateToRevalTo, SchemeInForceDate,
                                     revalMethod="RPII"){
  indexTables[, Month := as.Date(Month, format = "%d/%m/%Y")]
  
  DT[, sector := ifelse(LGPSInd=="Y", "Public", "Private")]
  DT[, relevantPpnColumn := paste0(sector, gender)]
  
  DT[!is.na(dod), RevaluedPension := revalue.pension(MemberIdentity, pensionamount, PensionDate, 
                                                     dateToRevalTo, SchemeInForceDate, indexTables, 
                                                     revalMethod = "RPII", ppnRPIITables, 
                                                     relevantPpnColumn, DatePensionCommenced)]
  
  DT[is.na(dod), RevaluedPension := revalue.pension(MemberIdentity, pensionamount, PensionDate, 
                                                    dateToRevalTo, SchemeInForceDate, indexTables, 
                                                    revalMethod = "RPI", ppnRPIITables, 
                                                    relevantPpnColumn, DatePensionCommenced)]
  DT
}

revalue.salary <- function(salary, salaryDate, dateToRevalTo, indexTables) {
  firstIndexDate <- indexTables[, min(Month)]
  lastIndexDate <- indexTables[, max(Month)]
  
  relevantSalaryDate <- pmax(pmin(salaryDate, lastIndexDate), firstIndexDate)
  firstOfSalaryMonth <- as.Date(fast_strptime(paste(year(relevantSalaryDate), 
                                                    month(relevantSalaryDate), 
                                                    "01", sep = "-"),
                                              format = "%Y-%m-%d"))
  
  
  rpiAtRevaluedDate <- indexTables[fmatch(dateToRevalTo, Month), RPI]
  rpiAtSalaryDate <- indexTables[fmatch(firstOfSalaryMonth, Month), RPI]
  
  revaluedSalary <- salary * rpiAtRevaluedDate / rpiAtSalaryDate
  
  return(revaluedSalary)
}

revalue.salaries.in.data <- function(DT, dateToRevalTo, indexTables){
  DT[,RevaluedSalary := revalue.salary(FPSusedToCalcBens, salaryDate, dateToRevalTo, indexTables)]
}

# Data aggregator -----------------------------------------------------------------------------

aggregate.results <- function(currentTable, aggregateBy, includeCrudeLE=FALSE){
  results <- currentTable[, list(InitialETR=sum(InitialETR), Observed=sum(Observed),Expected=sum(Expected), 
                                 BinomialVariance=sum(BinomialVariance)),
                          by=aggregateBy]
  
  results[, AoE := Observed/Expected]
  results[, AoEStdDev := sqrt(BinomialVariance / (Expected ^ 2))]
  results[, L95 := AoE - qnorm(0.975) * AoEStdDev]
  results[, U95 := AoE + qnorm(0.975) * AoEStdDev]
  
  print("Here")
  if(includeCrudeLE & "AgeGroup" %in% aggregateBy & "AgeGroup" %in% colnames(currentTable)){
    aggregateByAllButAge <- aggregateBy[-grep("AgeGroup",aggregateBy)]
    
    crudeLESummary <- results[,calculate.crude.LE.by.age(AgeGroup, Observed, InitialETR, 
                                                         confidenceLevel=0.95, ageGroupSize=5),
                              by = aggregateByAllButAge]
    columnsToRetain <- c(aggregateBy, "LEx.AG", "sdLEx.AG", "LExMin95.AG","LExMax95.AG")
    crudeLESummary <- crudeLESummary[,columnsToRetain, with=FALSE]
    setnames(crudeLESummary, old=c("LEx.AG", "sdLEx.AG", "LExMin95.AG","LExMax95.AG"),
             new=c("CrudeLE", "CrudeLEStdDev", "CrudeLE_L95","CrudeLE_U95"))
    results <- merge(results,crudeLESummary, by=aggregateBy)
  }else{
    if(includeCrudeLE){message("AgeGroup not specified as an aggregator or not in data: cannot calculate crude LE")}
  }
  return(results)
}

# Exposure calculator -------------------------------------------------------------------------

calculate.ETR.in.year <- function(DT, calendarYear, allMortalityTables, baseTableYear, 
                                  maleImprovementFactors, femaleImprovementFactors, variablesToSplitBy,
                                  minimumAgeToInclude = 60, maximumAgeToInclude = 95, 
                                  splitByAgeGroup=TRUE, useLUY = TRUE,
                                  amountsBasis = FALSE, useVitaCurves = TRUE) {
  
  print(paste0("Year: ", calendarYear))
  
  firstDayOfCalendarYear <- as.Date(paste(calendarYear, "01", "01", sep = "-"),
                                    format = "%Y-%m-%d")
  firstDayOfFollowingYear <- as.Date(paste(calendarYear + 1, "01", "01", sep = "-"),
                                     format = "%Y-%m-%d")
  
  print("Calculating exposure")
  
  #Remove those who died prior to calendar year
  DT <- DT[is.na(dod) | (year(dod) >= calendarYear)]
  
  #Calculate entry and exit date in year
  #Note: Covariate EUDs included, as they should already have been mapped to NA for members with bad flags
  DT[, EntryDate := pmax(firstDayOfCalendarYear, EUD, DatePensionCommenced, LGEUD, SalEUD, PenEUD, na.rm = TRUE)]
  
  if(useLUY){
    DT[month(LUD) < 12, LUD :=  as.Date(paste(year(LUD) - 1, "12", "31", sep = "-"), format = "%Y-%m-%d")]
  }
  
  #Changed to LUD + 1 from Sheffield
  DT[, ExitDateExcludingDOD := pmin(firstDayOfFollowingYear, LUD + 1, DatePenCeased, na.rm = TRUE)]
  
  #If date of death falls outside the EntryDate/ExitDate period, then override the dod field, since death cannot be counted
  DT[year(dod)==calendarYear & (dod >= ExitDateExcludingDOD | dod < EntryDate), dod := NA]
  
  #Adjustment if initial ETR used
  DT[, InitialETR := pmax(as.numeric(difftime(ExitDateExcludingDOD, EntryDate, units = "days")) / 365.25, 0)]
  
  print("Calculating age")
  DT[, ageNearInYOE := round(as.numeric(difftime(firstDayOfCalendarYear, dob, units = "days")) / 365.25)]
  
  #Filter on ages outside inclusion range
  DT[ageNearInYOE < minimumAgeToInclude | ageNearInYOE > maximumAgeToInclude, InitialETR := 0]
  
  #Keep only non-zero exposures (hard-coded as initial ETR for now)
  DT <- DT[InitialETR > 0]
  
  #Add age group field, if this is the preferred age method
  minAge <-  max( floor(minimumAgeToInclude/5) * 5, 0)
  maxAge <- ceiling(maximumAgeToInclude/5) * 5
  DT[, AgeGroup := cut(ageNearInYOE, seq(minAge,maxAge,by=5),
                       right=FALSE, include.lowest=TRUE)]
  
  #Calculate expected mortality, AoE and confidence interval
  print("Calculating mortality")
  
  maleImprovementFactors <- 
    maleImprovementFactors[Age %in% minimumAgeToInclude:maximumAgeToInclude, get(as.character(calendarYear))] / 
    maleImprovementFactors[Age %in% minimumAgeToInclude:maximumAgeToInclude, get(as.character(baseTableYear))]
  
  femaleImprovementFactors <- 
    femaleImprovementFactors[Age %in% minimumAgeToInclude:maximumAgeToInclude, get(as.character(calendarYear))] / 
    femaleImprovementFactors[Age %in% minimumAgeToInclude:maximumAgeToInclude, get(as.character(baseTableYear))]
  
  improvementFactors <- data.table(ageNearInYOE = minimumAgeToInclude:maximumAgeToInclude,
                                   MaleFactor = maleImprovementFactors, 
                                   FemaleFactor = femaleImprovementFactors)
  
  if(useVitaCurves){
    DT[,MortalityTable := VitaCurve]
  }else{
    DT[,MortalityTable := StandardTable]
  }
  
  setkey(DT,ageNearInYOE,MortalityTable)
  DT <- merge(DT, allMortalityTables, by = c("ageNearInYOE", "MortalityTable"))
  DT <- merge(DT, improvementFactors, by = "ageNearInYOE")
  
  DT[gender=="M", qxInCalendarYear := qx * MaleFactor]
  DT[gender=="F", qxInCalendarYear := qx * FemaleFactor]
  
  if(amountsBasis) {
    pensionAmountsToUse <- DT[, RevaluedPension]
  } else {
    pensionAmountsToUse <- 1
  }
  
  #Aggregate results
  #ageVariable <- ifelse(useAgeGroups,"AgeGroup","ageNearInYOE")
  variablesToSplitByIncludingAge <- c(variablesToSplitBy, "AgeGroup")
  
  if(splitByAgeGroup){
    aggregateBy <- variablesToSplitByIncludingAge
  }else{
    aggregateBy <- variablesToSplitBy
  }   
  
  if(nrow(DT) == 0){
    print("No exposure in year for this group")
    results <- DT[, list(InitialETR=0, Observed=0, Expected=0, BinomialVariance=0,
                         AoE=NA_real_, AoEStdDev=NA_real_, L95=NA_real_,U95=NA_real_),
                  by=aggregateBy]
  }else{
    
    print("Calculating AoE")
    DT[, AmountExposed := pensionAmountsToUse * InitialETR]
    
    DT[, Expected := AmountExposed * qxInCalendarYear]
    DT[, BinomialVariance := pensionAmountsToUse ^ 2 * (InitialETR * qxInCalendarYear) * (InitialETR * (1-qxInCalendarYear))]
    
    DT[, Death := FALSE]
    DT[year(dod)==calendarYear, Death := TRUE] #Cases outside entry/exit date should already be excluded by this point
    DT[, Observed := AmountExposed * Death]
    
    print("Aggregating")
    
    results <- aggregate.results(DT, aggregateBy=aggregateBy, includeCrudeLE = FALSE)
  }
  
  results[, YOE := calendarYear]
  
  print("Done")
  results
}


# Graphics (unfinished - still pseudo) ------------------------------------------------------------------------------------
generate.plots <- function(annualResults, 
                           x_variable, 
                           y_variable, y_variable_L95, y_variable_U95,
                           variablesToSplitByOnSameChart, 
                           variablesToSplitByOnSeparateChart){
  
  #Output list of graphs to call (from markdown or main code -> pdf)
  graphsToPlot <-   
    annualResults[,
                  ggplot(data = .SD, aes_string(x = x_variable, y = y_variable)) + 
                    geom_points(aes_string(group = variablesToSplitByOnSameChart, 
                                           col = variablesToSplitByOnSameChart), position="dodge") + 
                    geom_errorbar(aes_string(group = variablesToSplitByOnSameChart,
                                             ymin = y_variable_L95, ymax = y_variable_U95), 
                                  position="dodge", col="orange"),
                  by = variablesToSplitByOnSeparateChart]
  
}



