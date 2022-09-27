# Load all predictions and compare with validation data
# The prediction closest to the peak of vegetation is the one compared to the validation entry of the year
library(pbapply)

source("../utils/load-sampling-data.r")
source("../utils/covariate-names.r")
source("../utils/utils.r")
source("../utils/accuracy-statistics.r")

ReferenceCSV = "../../data/raw/reference_global_100m_orig&change_year2015-2019_20210407.csv"
InputDir = "../../data/predictions" # Where to look for predictions
TargetSeason = 0.535 # Decimal date of July 15

ReferenceData = read.csv(ReferenceCSV)
ReferenceData = RenameReferenceData(ReferenceData)
ReferenceData = TidyData(ReferenceData)

# ValidateLocation = function(InputData)
# {
#     #InputData = Data[Data$location_id == location_id,]
#     #PeakData = lapply(unique(InputData$dataYear), GetPeakSeason, InputData)
#     PeakData = by(InputData, InputData$dataYear, GetPeakSeason)
#     PeakData = do.call(rbind, PeakData)
#     return(PeakData)
# }

# GetPeakSeason = function(InputYear)
# {
#     #InputYear = InputData[InputData$dataYear == Year, ]
#     # Shift southern hemisphere by half year
#     if (InputYear[1,"subpix_mean_y"] < 0)
#         InputYear$season = ShiftTime(InputYear$season, 0.5)
#     # Shift everything by TargetSeason
#     InputYear$season = ShiftTime(InputYear$season, -TargetSeason)
#     PeakSeason = InputYear[which.min(InputYear$season),]
#     return(PeakSeason)
# }
GetPeakSeason = function(InputYear)
    PeakSeason = InputYear[which.min(InputYear$season),]

# Shift decimal dates by a given amount, wrapping around
ShiftTime = function(input, amount)
{
    input = input + amount
    input[input > 1] = input[input > 1] - 1
    input[input < 0] = input[input < 0] + 1
    return(input)
}

# Harmonise predictions with validation data by extacting predictions closest to peak season
# and write the results into CSV/RDS
ValidatePredictions = function(InputFile)
{
    OutFile = sub("\\.rds$", "-val.csv", InputFile)
    if (file.exists(OutFile))
        return(OutFile)
    
    Prediction = readRDS(InputFile)
    Prediction$dataYear = lubridate::year(Prediction$timestamp.x)
    Data = merge(Prediction, ReferenceData, by=c("location_id", "dataYear"))
    # Filter to one per year, closest to the peak season
    Data$season = lubridate::decimal_date(Data$timestamp.x) %% 1
    # Shift south hemisphere seasons by 0.5
    Data[Data$subpix_mean_y < 0,"season"] = ShiftTime(Data[Data$subpix_mean_y < 0,"season"], 0.5)
    # Shift everything by target season so that 0 = peak season
    Data$season = ShiftTime(Data$season, TargetSeason)
    
    Results = by(Data, list(Data$location_id, Data$dataYear), GetPeakSeason)
    Results = do.call(rbind, Results)
    write.csv(Results, OutFile)
    return(OutFile)
}

InputFiles = list.files(InputDir, pattern=glob2rx("*.rds"), recursive = TRUE, full.names = TRUE)
ValFiles = pblapply(InputFiles, ValidatePredictions, cl=12)
ValFiles = unlist(ValFiles)

# Yearly validation
# Load each prediction, validate the reference data, return as a df
# TODO: This should be updated to use the output from ValidatePredictions() above to save time

for (InputFile in ValFiles)
{
    OutFile = sub("-val\\.csv$", ".csv", InputFile)
    if (file.exists(OutFile))
        next
    
    if (FALSE) {
    Prediction = readRDS(InputFile)
    Prediction$dataYear = lubridate::year(Prediction$timestamp.x)
    Data = merge(Prediction, ReferenceData, by=c("location_id", "dataYear"))
    # Filter to one per year, closest to the peak season
    Data$season = lubridate::decimal_date(Data$timestamp.x) %% 1
    #Results = lapply(unique(Data$location_id), ValidateLocation)
    #Results = pbtapply(Data, list(Data$location_id), ValidateLocation)
    #Results = by(Data, Data$location_id, ValidateLocation)
    #Results = do.call(rbind, Results)
    # Shift south hemisphere seasons by 0.5
    Data[Data$subpix_mean_y < 0,"season"] = ShiftTime(Data[Data$subpix_mean_y < 0,"season"], 0.5)
    # Shift everything by target season so that 0 = peak season
    Data$season = ShiftTime(Data$season, TargetSeason)
    
    Results = by(Data, list(Data$location_id, Data$dataYear), GetPeakSeason)
    Results = do.call(rbind, Results)
    }
    
    Results = read.csv(InputFile, row.names=1)
    
    Truth = Results[paste0(GetCommonClassNames(),".y")]
    Prediction = Results[paste0(GetCommonClassNames(),".x")]
    StatTable = AccuracyStatTable(Prediction, Truth)
    write.csv(StatTable, OutFile)
}

FractionColumns = c(paste0(GetCommonClassNames(),".y"),
                    paste0(GetCommonClassNames(),".x"))

ValFiles = list.files(InputDir, pattern=glob2rx("*-val.csv"), recursive = TRUE, full.names = TRUE)

# Change validation (year to year)
for (InputFile in ValFiles)
{
    OutFile = sub("-val\\.csv$", "-change_stats_unscaled.csv", InputFile)
    if (file.exists(OutFile))
        next
        
    print(OutFile)
    
    Validations = read.csv(InputFile)
    # Scale the predictions to sum to 100%
    ValidationsScaled = Validations
    ValidationsScaled[paste0(GetCommonClassNames(), ".x")] = ScalePredictions(ValidationsScaled[paste0(GetCommonClassNames(), ".x")])
    
    for (DataType in c("unscaled", "scaled")) # This is actually not needed because we already scaled the data at the prediction step, but it doesn't hurt
    {
        Data = if (DataType == "unscaled") Validations else ValidationsScaled
        OutFile = sub("-val\\.csv$", paste0("-change_stats_", DataType, ".csv"), InputFile)
        # Get one table per year
        ChangesTable = NULL
        TargetYears = seq(min(Data$dataYear), max(Data$dataYear), 1)
        for (YearTo in TargetYears[-1])
        {
            YearFrom = YearTo-1
            YearFromTable = Data[Data$dataYear == YearFrom,]
            YearToTable = Data[Data$dataYear == YearTo,]
            
            if (!all(YearToTable$location_id == YearFromTable$location_id))
            {
                # In some cases we have missing predictions for particular years,
                # remove them from both years in that case
                CommonIDs = intersect(YearFromTable$location_id, YearToTable$location_id)
                YearFromTable = YearFromTable[YearFromTable$location_id %in% CommonIDs,]
                YearToTable = YearToTable[YearToTable$location_id %in% CommonIDs,]
            }
            stopifnot(all(YearToTable$location_id == YearFromTable$location_id))
            
            ChangeTable = YearToTable[FractionColumns] - YearFromTable[FractionColumns]
            ChangeTable$location_id = YearToTable$location_id
            ChangeTable$Years = paste(YearFrom, YearTo, sep="-")
            
            ChangeFile = sub("-val\\.csv$", paste0("-change_",ChangeTable$Years[1],"_", DataType, ".csv"), InputFile)
            Truth = ChangeTable[paste0(GetCommonClassNames(),".y")]
            Prediction = ChangeTable[paste0(GetCommonClassNames(),".x")]
            StatTable = AccuracyStatTable(Prediction, Truth)
            write.csv(StatTable, ChangeFile)
            
            ChangesTable = rbind(ChangesTable, ChangeTable)
        }
        ChangeValFile = sub("-val\\.csv$", paste0("-change_val_", DataType,".csv"), InputFile) # The file with all changes per each year
        write.csv(ChangesTable, ChangeValFile)
        Truth = ChangesTable[paste0(GetCommonClassNames(),".y")]
        Prediction = ChangesTable[paste0(GetCommonClassNames(),".x")]
        StatTable = AccuracyStatTable(Prediction, Truth)
        #AccuracyStatisticsPlots(Prediction, Truth)
        write.csv(StatTable, OutFile)
    }
}
