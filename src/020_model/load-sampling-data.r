source("covariate-names.r")


# Updates the dominant_lc column based on the classes desired
UpdateDominantLC = function(df, classes = GetCommonClassNames())
{
    ClassProportions = df[,classes[classes %in% names(df)]]
    DominantClasses = apply(ClassProportions, 1, which.max)
    df$dominant_lc = factor(classes[DominantClasses])
    return(df)
}

# Remove rows with NAs and drop covariates with too few observations
TidyData = function(df, classes = GetCommonClassNames(), drop.cols=NULL)
{
    # Remove rows that have NA in some key columns.
    # NA in elevation means that the point is not actually in Africa. Remove those.
    # NA in amplitude1 means that we have no time series over the area, so remove these (though likely it's bare soil)
    # NA in evi means that we didn't have an image from the summer of year 2016. That's a lot of points to remove; but the alternative is dropping those covars altogether.
    # NA in soil or climate covars means it's over water.
    
    Before = nrow(df)
    if (is.character(drop.cols))
    {
        DropRows = apply(df[,drop.cols], 1, function(x){any(!is.finite(x))})
        #DropRows = apply(df[,GetAllPixelCovars()], 1, function(x){any(is.na(x))})
        df = df[!DropRows,]
        After = nrow(df)
        print(paste("Dropped NAs, data frame size reduced from", Before, "to", After))
        Before = After
        
        stopifnot(all(apply(df[,drop.cols], 2, function(x){sum(is.na(x))}) / nrow(df) * 100 == 0))
    }
    
    # Recalculate dominant classes based on all classes
    df = UpdateDominantLC(df, classes)
    # Drop those dominated by "not_sure"
    df = df[df$dominant_lc != "not_sure",]
    
    # Reclassify rare classes to common ones
    df = ReclassifyAndScale(df)
    
    After = nrow(df)
    print(paste("Reclassified and rescaled small classes, data frame size reduced from", Before, "to", After))
    
    # Also drop the level, otherwise sampling would try to sample from 0 points
    df = UpdateDominantLC(df, classes)
    
    return(df)
}

ReclassifyAndScale = function(df, output.classes=GetCommonClassNames())
{
    # Some classes are merged to other classes. Put the values into the bigger classes
    ClassMap = c(burnt="grassland",
                 fallow_shifting_cultivation="crops",
                 wetland_herbaceous="grassland",
                 lichen_and_moss="grassland",
                 lichen="grassland",
                 fl.grass="grassland",
                 fl.lichen="grassland",
                 snow_and_ice="bare",
                 snow="bare")
    
    for (class in 1:length(ClassMap))
    {
        if (names(ClassMap[class]) %in% names(df))
            df[[ClassMap[class]]] = df[[ClassMap[class]]] + df[[names(ClassMap[class])]]
    }
    
    # Scale relevant classes to 100%; that way we get rid of influences from not_sure and snow_and_ice
    RelevantClasses = df[, output.classes]
    ClassSums = rowSums(RelevantClasses)
    ZeroRows = ClassSums == 0
    if (any(ZeroRows))
    {
        print(paste("Dropping", sum(ZeroRows), "samples because all their relevant fractions are zero"))
        RelevantClasses = RelevantClasses[!ZeroRows,]
        df = df[!ZeroRows,]
        ClassSums = ClassSums[!ZeroRows]
    }
    
    df[,output.classes] = RelevantClasses / (ClassSums / 100)
    stopifnot(all(round(rowSums(df[,output.classes])) == 100))
    return(df)
}

# Rename the columns of the reference dataset to match those in the IIASA 2015 dataset
RenameReferenceData = function(df)
{
    NameMap = data.frame(from=c("trees", "grass", "urban"),
                         to=c("tree", "grassland", "urban_built_up"))
    NewNames = names(df)
    for (i in 1:nrow(NameMap))
        if (NameMap[i,"from"] %in% names(df))
            NewNames[names(df) == NameMap[i,"from"]] = NameMap[i, "to"]
    
    names(df) = NewNames
    return(df)
}

