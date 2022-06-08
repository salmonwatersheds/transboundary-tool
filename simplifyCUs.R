###############################################################################
# Read in shapefiles for CU boundaries from DFO and simplify these and save as 
# .rds for reading into shiny app
###############################################################################

library(rgdal)
library(rmapshaper)

# Shapefiles are huge. Easier to save as RDS?
#------------------------------------------------------------------------------
# Chinook
#------------------------------------------------------------------------------

CU_CK <- readOGR("/Users/stephaniepeacock/Dropbox (Salmon Watersheds)/X Drive/5_DATA/Mapping/Transboundary/conservation units/DFO_CUs_updated_Dec2021_chinook.shp", layer = "DFO_CUs_updated_Dec2021_chinook", GDAL1_integer64_policy = TRUE)

saveRDS(CU_CK, file = "shiny-app/data/CU/CU_CK.rds")

#------------------------------------------------------------------------------
# Coho
#------------------------------------------------------------------------------

CU_CO <- readOGR("/Users/stephaniepeacock/Dropbox (Salmon Watersheds)/X Drive/5_DATA/Mapping/Transboundary/conservation units/DFO_CUs_updated_Dec2021_coho.shp", layer = "DFO_CUs_updated_Dec2021_coho", GDAL1_integer64_policy = TRUE)

saveRDS(CU_CO, file = "shiny-app/data/CU/CU_CO.rds")

#------------------------------------------------------------------------------
# Chum
#------------------------------------------------------------------------------

CU_CM <- readOGR("/Users/stephaniepeacock/Dropbox (Salmon Watersheds)/X Drive/5_DATA/Mapping/Transboundary/conservation units/DFO_CUs_updated_Dec2021_chum.shp", layer = "DFO_CUs_updated_Dec2021_chum", GDAL1_integer64_policy = TRUE)

saveRDS(CU_CM, file = "shiny-app/data/CU/CU_CM.rds")

#------------------------------------------------------------------------------
# Even-year pink
#------------------------------------------------------------------------------

CU_PKE <- readOGR("/Users/stephaniepeacock/Dropbox (Salmon Watersheds)/X Drive/5_DATA/Mapping/Transboundary/conservation units/DFO_CUs_updated_Dec2021_PKE.shp", layer = "DFO_CUs_updated_Dec2021_PKE", GDAL1_integer64_policy = TRUE)

saveRDS(CU_PKE, file = "shiny-app/data/CU/CU_PKE.rds")

#------------------------------------------------------------------------------
# Odd-year pink
#------------------------------------------------------------------------------

CU_PKO <- readOGR("/Users/stephaniepeacock/Dropbox (Salmon Watersheds)/X Drive/5_DATA/Mapping/Transboundary/conservation units/DFO_CUs_updated_Dec2021_PKO.shp", layer = "DFO_CUs_updated_Dec2021_PKO", GDAL1_integer64_policy = TRUE)

saveRDS(CU_PKO, file = "shiny-app/data/CU/CU_PKO.rds")

#------------------------------------------------------------------------------
# Lake-type sockeye
#------------------------------------------------------------------------------

CU_SEL <- readOGR("/Users/stephaniepeacock/Dropbox (Salmon Watersheds)/X Drive/5_DATA/Mapping/Transboundary/conservation units/DFO_CUs_updated_Dec2021_SEL.shp", layer = "DFO_CUs_updated_Dec2021_SEL", GDAL1_integer64_policy = TRUE)

saveRDS(CU_SEL, file = "shiny-app/data/CU/CU_SEL.rds")

#------------------------------------------------------------------------------
# River-type sockeye
#------------------------------------------------------------------------------

CU_SER <- readOGR("/Users/stephaniepeacock/Dropbox (Salmon Watersheds)/X Drive/5_DATA/Mapping/Transboundary/conservation units/DFO_CUs_updated_Dec2021_SER.shp", layer = "DFO_CUs_updated_Dec2021_SER", GDAL1_integer64_policy = TRUE)

saveRDS(CU_SER, file = "shiny-app/data/CU/CU_SER.rds")
