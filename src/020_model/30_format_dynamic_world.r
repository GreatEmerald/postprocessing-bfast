# Load Dynamic World and format it as an RDS file like the other models

source("../utils/load-sampling-data.r")

OutFile = "../../data/predictions/dynamicworld/dynamicworld.rds"

DynamicWorld = FlattenGPKG("../../data/WURChange20152019_DynamicWorld_TS.gpkg")
DynamicWorld = RenameReferenceData(DynamicWorld)
DynamicWorld = ReclassifyAndScale(DynamicWorld)

if (!dir.exists(dirname(OutFile)))
    dir.create(dirname(OutFile))
saveRDS(DynamicWorld, OutFile)

# Do the same, but also reclasify low probabilities to zero

OutFile = "../../data/predictions/dynamicworld/dynamicworld-m10.rds"

DynamicWorld = FlattenGPKG("../../data/WURChange20152019_DynamicWorld_TS.gpkg")
DynamicWorld = RenameReferenceData(DynamicWorld)
# Filter away all probabilities less than 10
DynamicWorld[GetCommonClassNames()][DynamicWorld[GetCommonClassNames()] < 10] = 0
DynamicWorld = ReclassifyAndScale(DynamicWorld)

if (!dir.exists(dirname(OutFile)))
    dir.create(dirname(OutFile))
saveRDS(DynamicWorld, OutFile)
