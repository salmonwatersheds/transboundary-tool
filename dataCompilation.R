###############################################################################
#
# Data compilation
# for Shiny App
#
###############################################################################
library(tools) # for toTitleCase

#------------------------------------------------------------------------------
# Read in data
#------------------------------------------------------------------------------
nuseds <- read.csv("data/NuSEDS_20210513.csv")


#------------------------------------------------------------------------------
# Transboundary Streams
#------------------------------------------------------------------------------
# This is from Gottfried's Lookup Table: https://github.com/SOLV-Code/Transboundary_Data_Report/blob/main/DATA_PROCESSING/DATA/0_LookupFiles/TBR_PSF_CU_Site_LookupFile.csv

tbr <- read.csv("data/TBR_PSF_CU_Site_LookupFile.csv")

# Subset NuSEDS data to only include Central Coast streams
nuseds <- nuseds[which(nuseds$POP_ID %in% tbr$POP_ID & !is.na(nuseds$MAX_ESTIMATE)), ]

#------------------------------------------------------------------------------
# Create dataframe for use in app
#------------------------------------------------------------------------------

# Combine even and odd year pink
n <- length(unique(tbr$POP_ID))

tbrDat <- data.frame(
	POP_ID = unique(tbr$POP_ID)
)

popInd <- numeric(n)
for(i in 1:n){
	popInd[i] <- which(tbr$POP_ID == tbrDat$POP_ID[i])[1]
}

#------------------------------------------------------------------------------
# Species (n = 5)
#------------------------------------------------------------------------------
tbrDat$Species <- tbr$SPECIES_QUALIFIED[popInd]
tbrDat$Species[tbrDat$Species == "PKE"] <- "Pink"
tbrDat$Species[tbrDat$Species == "CO"] <- "Coho"
tbrDat$Species[tbrDat$Species == "CK"] <- "Chinook"
tbrDat$Species[tbrDat$Species == "SEL"] <- "Sockeye"
tbrDat$Species[tbrDat$Species == "SER"] <- "Sockeye"

#------------------------------------------------------------------------------
# Stream Name - make simple case
#------------------------------------------------------------------------------
tbrDat$streamName <- sapply(sapply(tbr$SYSTEM_SITE[popInd], tolower), toTitleCase)

# #------------------------------------------------------------------------------
# # Match watersheds
# !! Seems to bee a problem with the watershed lookup file. Check with Vesta. !!
# #------------------------------------------------------------------------------
# 
# wshd <- read.csv("data/TBR_watershed_LookupFile.csv")
# 
# # wshd$streamName %in% tbrDat$streamName
# # tbrDat$streamName %in% wshd$streamName
# 
# tbrDat$watershed <- wshd$watershedName[match(tbrDat$streamName, wshd$streamName)]
#------------------------------------------------------------------------------
# Location
#------------------------------------------------------------------------------

tbrDat$Latitude <- tbr$Y_LAT[popInd]
tbrDat$Longitude <- tbr$X_LONGT[popInd]


#------------------------------------------------------------------------------
# Average abundance
#------------------------------------------------------------------------------
tbrDat$avgSpawners <- NA
for(i in 1:n){
	if(tbrDat$POP_ID[i] > 0){
		tbrDat$avgSpawners[i] <- mean(nuseds$MAX_ESTIMATE[nuseds$POP_ID == tbrDat$POP_ID[i]])
	}
}

#------------------------------------------------------------------------------
# Number of estimates
#------------------------------------------------------------------------------
tbrDat$nYears <- rep(0, n)
for(i in 1:n){
	if(tbrDat$POP_ID[i] > 0){
		tbrDat$nYears[i] <- sum(!is.na(nuseds$MAX_ESTIMATE[nuseds$POP_ID == tbrDat$POP_ID[i]]))
	}
}


#------------------------------------------------------------------------------
# Survey quality
#------------------------------------------------------------------------------

# Clean up Estimate Classification field of nuseds
nuseds$ESTIMATE_CLASSIFICATION[nuseds$ESTIMATE_CLASSIFICATION == "PRESENCE/ABSENCE (TYPE-6)"] <- "PRESENCE-ABSENCE (TYPE-6)"
nuseds$ESTIMATE_CLASSIFICATION[nuseds$ESTIMATE_CLASSIFICATION =="NO SURVEY THIS YEAR"] <- "NO SURVEY"
nuseds$ESTIMATE_CLASSIFICATION[nuseds$ESTIMATE_CLASSIFICATION ==""] <- "UNKNOWN"

# Re-level ESTIMATE_CLASSIFICATION for easy numeric calculations
nuseds$ESTIMATE_CLASSIFICATION <- factor(
	nuseds$ESTIMATE_CLASSIFICATION,
	levels = c(
		"TRUE ABUNDANCE (TYPE-1)",
		"TRUE ABUNDANCE (TYPE-2)",
		"RELATIVE ABUNDANCE (TYPE-3)",
		"RELATIVE ABUNDANCE (TYPE-4)",
		"RELATIVE ABUNDANCE (TYPE-5)",
		"PRESENCE-ABSENCE (TYPE-6)",
		"RELATIVE: CONSTANT MULTI-YEAR METHODS",
		"RELATIVE: VARYING MULTI-YEAR METHODS",
		"NO SURVEY",
		"UNKNOWN"))

nuseds$ESTIMATE_CLASSIFICATION_num <- as.numeric(nuseds$ESTIMATE_CLASSIFICATION)
nuseds$ESTIMATE_CLASSIFICATION_num[nuseds$ESTIMATE_CLASSIFICATION_num == 7] <- 5
nuseds$ESTIMATE_CLASSIFICATION_num[nuseds$ESTIMATE_CLASSIFICATION_num == 8] <- 6
nuseds$ESTIMATE_CLASSIFICATION_num[nuseds$ESTIMATE_CLASSIFICATION_num > 7] <- 7

# range(nuseds$ESTIMATE_CLASSIFICATION_num)

tbrDat$quality <- rep(NA, n)
for(i in 1:n){
	nuseds.p <- nuseds[which(nuseds$POP_ID == tbrDat$POP_ID[i] & nuseds$ANALYSIS_YR >= 2000), ]
	tbrDat$quality[i] <- mean(nuseds.p$ESTIMATE_CLASSIFICATION_num, na.rm = TRUE)
}

#------------------------------------------------------------------------------
# Add in NuSEDS MAX_ESIMTATES since 1950
#------------------------------------------------------------------------------

tbrDat[ , paste0("X", 1950:max(nuseds$ANALYSIS_YR))] <- NA
yr <- c(1950:max(nuseds$ANALYSIS_YR))

for(i in 1:n){
	if(tbrDat$POP_ID[i] > 0){
		nuseds.p <- nuseds[which(nuseds$POP_ID == tbrDat$POP_ID[i]), ]
		tbrDat[i , paste0("X", 1950:max(nuseds$ANALYSIS_YR))] <- nuseds.p$MAX_ESTIMATE[match(yr, nuseds.p$ANALYSIS_YR)]
	}
}


###############################################################################
# Write csv
###############################################################################

write.csv(tbrDat, file = "data/TBREscapement.csv", row.names = FALSE)
