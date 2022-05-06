# Get all data.frame columns that have a date in it (according to the pattern)
datecols = function(object, pattern="X????.??.??*")
{
    grep(glob2rx(pattern), names(object))
}

# Extract the time series in matrix format from an SF object by date
SFToMatrix = function(object, pattern="X????.??.??*", rowname.source="location_id")
{
    cols = datecols(object, pattern)
    result = as.matrix(st_drop_geometry(object)[cols])
    rownames(result) = object[[rowname.source]]
    return(result)
}

# Parse a matrix into a zoo object
MatrixToZoo = function(object)
{
    datenames = colnames(object)
    dates = as.Date(datenames, "X%Y.%m.%d")
    return(as.zooreg(zoo(t(object), dates), 16))
}

# Utility for both
SFToZoo = function(object, ...)
{
    MatrixToZoo(SFToMatrix(object, ...))
}

# Reintegrate a zoo object into an SF object (used as a template)
ZooToSF = function(zobj, sfobj)
{
    st_crs(sfobj) = 4326
    
    stopifnot(nrow(sfobj) == ncol(zobj))
    stopifnot(length(datecols(sfobj)) == nrow(zobj))
    
    sfobj[datecols(sfobj)] = t(zobj)
    return(sfobj)
}
