###############################################################################
# INITIALIZE
###############################################################################
rm(list = ls())                                      # Clear global environment
cat("\014")                                          # Clear console window
file.remove(dir(paste(getwd(),"/output/", sep = ""), # Clear output folder
                full.names = TRUE))    
source("functions.R")                                # Load custom functions
load_libraries(c("rio", "Amelia"))                   # Install & load libraries

###############################################################################
# LOAD DATA
###############################################################################
file_to_import = paste(getwd(), "/data/Customer Survey-exported30oct2017.xlsx",
                       sep = "")
data = import(file_to_import)
# names(data)

###############################################################################
# CLEAN DATA
###############################################################################
# Shorten variable (column) names
old.col.names = names(data)
names(data) = c("ID", "Creatr", "Date", "LBO", "Prov", "Dist", "Comm", "Vill",
                "Lat", "Lng", "CLoc", "LatPur", "RLname", "RFname", "RGend",
                "Phone", "RisC", "CLname", "CFname", "CGend", "CrelR",
                "IDPoor", "IDPoorTyp", "M01", "M1824", "M217", "M2559",
                "M60", "F01", "F1824", "F217",
                "F2559", "F60", "LivRP", "VillOD", "DateSlabPur", "LatInst",
                "BelowGrndInst", "DateBelowGrndInst", "ShltrInst",
                "DateInstComp", "RDefBefor", "RDefBeforOthr", "FreqNeiToi",
                "WhoInstLat", "KnwSubsdy", "RecSubsdy", "BorwLat",
                "CanBuyLat", "UseFincOthr", "SlabTil", "Npits", "PitConfig",
                "NringsDir", "NringsOff", "NringsOthr", "ShltrWallTyp",
                "ShltrWallTypOthr", "LatRoofTyp", "LatRoofTypOthr", 
                "MatsPurTgthr", "Cost", "CostInclInst", "CostPitSlab", 
                "CostPitSlabKnwn", "CostShltrMats", "CostShltrMatsKnwn",
                "LabPitShltrPurTgthr", "CostLabLat", "CostLabPitSlab",
                "CostLabPitSlabKnwn", "CostLabShltr", "CostLabShltrKnwn",
                "LatTypOwndBefor", "IntndChngDich", "IntndChng", 
                "IntndChngOthr", "AdltUseLat", "ChldUseLat", "InfLatDump",
                "IntndPitFull", "IntndPitFullOthrAns", "Chlngs", "Satis", "Rec",
                "SatisSup", "RecSup", "RQues", "DateSurvCreated")
print(data.frame(names(data), old.col.names))     # Verify consistency

# Remove unused variables
drops = c("Creatr","Lat", "Lng", "CLoc", "LatPur", "RLname", "RFname",
          "Phone", "CLname", "CFname", "RQues")
data = data[ , !(names(data) %in% drops)]

# Convert data formats
for (i in 1:length(names(data))) {
  if (is.character(data[i][[1]])) {
    data[i][[1]] = as.factor(data[i][[1]])            # Characters into factors
  }
}
data$Date = as.Date(data$Date)                          # Date into date format
data$DateSlabPur = as.Date(data$DateSlabPur)
data$DateBelowGrndInst = as.Date(data$DateBelowGrndInst)
data$DateInstComp = as.Date(data$DateInstComp)
data$DateSurvCreated = as.Date(data$DateSurvCreated)

# Create Yr, Mnth, and YrMnth variables as factors
data$Yr = as.factor(format(as.Date(data$Date, format = "%Y-%m-%d"),"%Y"))
data$Mnth = as.factor(format(as.Date(data$Date, format = "%Y-%m-%d"),"%m"))
data$YrMnth = as.factor(paste(data$Yr, data$Mnth, sep = ""))

# Rename Prov variable
summary(data$Prov)                                      # Before
levels(data$Prov) = c("Banteay Meanchey", "Kampong Cham", "Kampong Speu",
                      "Kampong Thom", "Kandal", "Oddar Meanchey", "Phnom Penh",
                      "Prey Veng", "Siem Reap", "Svay Rieng", "Takeo")
summary(data$Prov)                                      # After

# If CGend is empty and RisC, then copy RGend to CGend
print(data.frame(data$RGend, data$CGend, data$RisC))     # Before
for (row in 1:length(data$ID)) {
  if (is.na(data[row,]$CGend) & data[row,]$RisC == "Yes") {
    data[row,]$CGend = data[row,]$RGend
    }
}
print(data.frame(data$RGend, data$CGend, data$RisC))     # After

# Remove rows with empty CGend
summary(data$CGend)                                      # Before
data = subset(data, !is.na(CGend))
summary(data$CGend)                                      # After

# Rename responses in CrelR
summary(data$CrelR)                                      # Before
levels(data$CrelR) = c("Siblng", "Spous", "Parnt", "Othr")
summary(data$CrelR)                                      # After

# Rename responses in IDPoorTyp
summary(data$IDPoorTyp)                                      # Before
levels(data$IDPoorTyp) = c("IDP1", "IDP2", "UnkIDP", "No")
summary(data$IDPoorTyp)                                      # After

# If IDPoor is No and IDPoorType is NA, then copy IDPoor to IDPoorTyp
print(data.frame(data$IDPoor, data$IDPoorTyp))               # Before
for (row in 1:length(data$ID)) {
  if (data[row,]$IDPoor == "No" & is.na(data[row,]$IDPoorTyp)) {
    data[row,]$IDPoorTyp = data[row,]$IDPoor
  }
}
print(data.frame(data$IDPoor, data$IDPoorTyp))               # After
summary(data$IDPoorTyp)                                      # After

# Rename responses in LivRP
summary(data$LivRP)                                      # Before
levels(data$LivRP) = c("No", "Pond", "Rivr", "Pond", "Rivr")
summary(data$LivRP)                                      # After

# Rename responses in VillOD
summary(data$VillOD)                                      # Before
levels(data$VillOD) = c("Most", "None", "Some")
summary(data$VillOD)                                      # After

# Rename responses in RDefBefor
summary(data$RDefBefor)                                      # Before
levels(data$RDefBefor) = c("Bsh/Fld", "Bsh/Fld;NA/AlwysToi", "Bsh/Fld;Othr",
                           "NeiToi", "NeiToi;Bsh/Fld", "NeiToi;Bsh/Fld;Othr",
                           "NeiToi;NA/AlwysToi", "NeiToi;Othr", "NA/AlwysToi",
                           "Othr", "NA/AlwysToi", "Riv/Pnd", "Riv/Pnd;Bsh/Fld",
                           "Riv/Pnd;NeiToi;Bsh/Fld", "Riv/Pnd;Othr")
summary(data$RDefBefor)                                      # After

# Create new variables based on RDefBefor
summary(data$RDefBefor)                                      # Before
data$RDefBefor_BshFld = ifelse(is.na(data$RDefBefor), NA,
                               ifelse(grepl("Bsh/Fld", data$RDefBefor,
                                            fixed = TRUE), 1, 0))
data$RDefBefor_RivPnd = ifelse(is.na(data$RDefBefor), NA,
                               ifelse(grepl("Riv/Pnd", data$RDefBefor,
                                            fixed = TRUE), 1, 0))
data$RDefBefor_NeiToi = ifelse(is.na(data$RDefBefor), NA,
                               ifelse(grepl("NeiToi", data$RDefBefor,
                                            fixed = TRUE), 1, 0))
data$RDefBefor_Othr = ifelse(is.na(data$RDefBefor), NA,
                             ifelse(grepl("Othr", data$RDefBefor,
                                          fixed = TRUE), 1, 0))
data$RDefBefor_NAAlwysToi = ifelse(is.na(data$RDefBefor), NA,
                                   ifelse(grepl("AlwysToi", data$RDefBefor,
                                                fixed = TRUE), 1, 0))
data$RDefBefor_BshFld = as.factor(data$RDefBefor_BshFld)
data$RDefBefor_RivPnd = as.factor(data$RDefBefor_RivPnd)
data$RDefBefor_NeiToi = as.factor(data$RDefBefor_NeiToi)
data$RDefBefor_Othr = as.factor(data$RDefBefor_Othr)
data$RDefBefor_NAAlwysToi = as.factor(data$RDefBefor_NAAlwysToi)
summary(data$RDefBefor_BshFld)                                # After
summary(data$RDefBefor_RivPnd)
summary(data$RDefBefor_NeiToi)
summary(data$RDefBefor_Othr)
summary(data$RDefBefor_NAAlwysToi)

# Rename responses in FreqNeiToi
summary(data$FreqNeiToi)                                      # Before
levels(data$FreqNeiToi) = c("Freq", "Freq", "Nevr", "NA/AlwysToi",
                            "NA/AlwysToi", "Some")
summary(data$FreqNeiToi)                                      # After

# Rename responses in WhoInstLat
summary(data$WhoInstLat)                                      # Before
levels(data$WhoInstLat) = c("Self/Fam", "Self/Fam", "Gov", "Friend$", "Mason",
                            "LBO", "NGO")
summary(data$WhoInstLat)                                      # After

# Rename responses in RecSubsdy
summary(data$RecSubsdy)                                      # Before
levels(data$RecSubsdy) = c("DK", "No", "Yfull", "Ypart", "Yfull", "Ypart")
summary(data$RecSubsdy)                                      # After

# Rename responses in Borw$Lat
summary(data$BorwLat)                                      # Before
levels(data$BorwLat) = c("Yes", "DK", "No", "Yes")
summary(data$BorwLat)                                      # After

# Rename responses in IntndChng
summary(data$IntndChng)                                      # Before
levels(data$IntndChng) = c("Shltr", "Shltr;Othr", "Shwr", "Shltr;Shwr",
                           "Shltr;Shwr;Othr", "Shwr;Sink", 
                           "Shltr;Shwr;Sink", "Shwr;Sink;Othr",
                           "Shwr;Othr", "WtrRes", "Shltr;WtrRes",
                           "Shwr;WtrRes", "Shltr;Shwr;WtrRes",
                           "Shwr;Sink;WtrRes", "Shltr;Shwr;Sink;WtrRes",
                           "Shltr;Shwr;Sink;WtrRes;Othr",
                           "Shwr;Sink;WtrRes;Othr", "Shwr;WtrRes;Othr",
                           "Sink;WtrRes", "Shltr;Sink;WtrRes",
                           "WtrRes;Othr", "Sink", "Shltr;Sink", "Pit",
                           "Shltr;Pit", "Shltr;Pit;Othr", "Shwr;Pit",
                           "Shltr;Shwr;Pit", "Shwr;Sink;Pit",
                           "Shwr;Pit;Othr", "WtrRes;Pit",
                           "Shltr;WtrRes;Pit", "Shwr;WtrRes;Pit",
                           "Shltr;Shwr;WtrRes;Pit", "Shwr;Sink;WtrRes;Pit",
                           "Shltr;Shwr;Sink;WtrRes;Pit", "Sink;Pit",
                           "Shltr;Sink;Pit", "Pit;Othr", "NA/AlwysToi",
                           "Othr")
summary(data$IntndChng)                                      # After

# Create new variables based on IntndChng
summary(data$IntndChng)                                      # Before
data$IntndChng_Shltr = ifelse(is.na(data$IntndChng), NA,
                              ifelse(grepl("Shltr", data$IntndChng,
                                           fixed = TRUE), "1", "0"))
data$IntndChng_Shwr = ifelse(is.na(data$IntndChng), NA,
                             ifelse(grepl("Shwr", data$IntndChng,
                                          fixed = TRUE), 1, 0))
data$IntndChng_Sink = ifelse(is.na(data$IntndChng), NA,
                               ifelse(grepl("Sink", data$IntndChng,
                                            fixed = TRUE), 1, 0))
data$IntndChng_WtrRes = ifelse(is.na(data$IntndChng), NA,
                               ifelse(grepl("WtrRes", data$IntndChng,
                                            fixed = TRUE), 1, 0))
data$IntndChng_Pit = ifelse(is.na(data$IntndChng), NA,
                             ifelse(grepl("Pit", data$IntndChng,
                                          fixed = TRUE), 1, 0))
data$IntndChng_Othr = ifelse(is.na(data$IntndChng), NA,
                             ifelse(grepl("Othr", data$IntndChng,
                                          fixed = TRUE), 1, 0))
data$IntndChng_NAAlwysToi = ifelse(is.na(data$IntndChng), NA,
                                   ifelse(grepl("NA/AlwysToi", data$IntndChng,
                                                fixed = TRUE), 1, 0))
data$IntndChng_Shltr = as.factor(data$IntndChng_Shltr)
data$IntndChng_Shwr = as.factor(data$IntndChng_Shwr)
data$IntndChng_Sink = as.factor(data$IntndChng_Sink)
data$IntndChng_WtrRes = as.factor(data$IntndChng_WtrRes)
data$IntndChng_Pit = as.factor(data$IntndChng_Pit)
data$IntndChng_Othr = as.factor(data$IntndChng_Othr)
data$IntndChng_NAAlwysToi = as.factor(data$IntndChng_NAAlwysToi)
summary(data$IntndChng_Shltr)                                # After
summary(data$IntndChng_Shwr)
summary(data$IntndChng_Sink)
summary(data$IntndChng_WtrRes)
summary(data$IntndChng_Pit)
summary(data$IntndChng_Othr)
summary(data$IntndChng_NAAlwysToi)

# Rename responses in AdltUseLat
summary(data$AdltUseLat)                                      # Before
levels(data$AdltUseLat) = c("Freq", "Freq", "DK", "Rare", "Some")
summary(data$AdltUseLat)                                      # After

# Rename responses in ChldUseLat
summary(data$ChldUseLat)                                      # Before
levels(data$ChldUseLat) = c("Freq", "NoChld", "DK/NA", "DK/NA", "Rare", "Some")
summary(data$ChldUseLat)                                      # After

# Rename responses in InfLatDump
summary(data$InfLatDump)                                      # Before
levels(data$InfLatDump) = c("Freq", "NoInf", "DK/NA", "DK/NA", "Rare", "Some")
summary(data$InfLatDump)                                      # After

# Rename responses in IntndPitFull
summary(data$IntndPitFull)                                      # Before
levels(data$IntndPitFull) = c("DK", "EmpSlf", "Pit", "Othr", "Pay", "Stop")
summary(data$IntndPitFull)                                      # After

# Create new variables based on IntndPitFull
summary(data$IntndPitFull)                                      # Before
data$IntndPitFullDes = ifelse(is.na(data$IntndPitFull), NA,
                               ifelse(grepl("Pit", data$IntndPitFull,
                                            fixed = TRUE) |
                                        grepl("Pay", data$IntndPitFull,
                                              fixed = TRUE), "1", "0"))
data$IntndPitFullDes = as.factor(data$IntndPitFullDes)
summary(data$IntndPitFullDes)                                # After
summary(data$IntndPitFull)                                      # Before
data$IntndPitFullPit = ifelse(is.na(data$IntndPitFull), NA,
                              ifelse(grepl("Pit", data$IntndPitFull,
                                           fixed = TRUE), "1", "0"))
data$IntndPitFullPit = as.factor(data$IntndPitFullPit)
summary(data$IntndPitFullPit)                                # After
summary(data$IntndPitFull)                                      # Before
data$IntndPitFullEmpSlf = ifelse(is.na(data$IntndPitFull), NA,
                               ifelse(grepl("EmpSlf", data$IntndPitFull,
                                            fixed = TRUE), "1", "0"))
data$IntndPitFullEmpSlf = as.factor(data$IntndPitFullEmpSlf)
summary(data$IntndPitFullEmpSlf)                                # After
summary(data$IntndPitFull)                                      # Before
data$IntndPitFullDK = ifelse(is.na(data$IntndPitFull), NA,
                                 ifelse(grepl("DK", data$IntndPitFull,
                                              fixed = TRUE), "1", "0"))
data$IntndPitFullDK = as.factor(data$IntndPitFullDK)
summary(data$IntndPitFullDK)                                # After
data$IntndPitFullOthr = ifelse(is.na(data$IntndPitFull), NA,
                             ifelse(grepl("Othr", data$IntndPitFull,
                                          fixed = TRUE), "1", "0"))
data$IntndPitFullOthr = as.factor(data$IntndPitFullOthr)
summary(data$IntndPitFullOthr)                                # After
data$IntndPitFullPay = ifelse(is.na(data$IntndPitFull), NA,
                               ifelse(grepl("Pay", data$IntndPitFull,
                                            fixed = TRUE), "1", "0"))
data$IntndPitFullPay = as.factor(data$IntndPitFullPay)
summary(data$IntndPitFullPay)                                # After
data$IntndPitFullStop = ifelse(is.na(data$IntndPitFull), NA,
                              ifelse(grepl("Stop", data$IntndPitFull,
                                           fixed = TRUE), "1", "0"))
data$IntndPitFullStop = as.factor(data$IntndPitFullStop)
summary(data$IntndPitFullStop)                                # After

# Rename responses in Chlngs
summary(data$Chlngs)                                      # Before
levels(data$Chlngs) = c("NoFlsh", "NoFlsh;Flood", "NoFlsh;Othr",
                        "NoFlsh;Ful/OvrFlw", "NoFlsh;Ful/OvrFlw;Flood",
                        "Flood", "NoWtr", "NoFlsh;NoWtr", 
                        "NoFlsh;Ful/OvrFlw;NoWtr", "NoWtr;Smels",
                        "OK", "NoFlsh", "NoWtr", "Ful/OvrFlw", "Smels",
                        "Othr", "Ful/OvrFlw", "Ful/OvrFlw", "Smels",
                        "NoFlsh;Smels", "NoFlsh;Flood;Smels",
                        "NoFlsh;Ful/OvrFlw;Smels", "Flood;Smels", "Smels;Othr",
                        "Ful/OvrFlw;Flood;Smels")
summary(data$Chlngs)                                      # After

# Create new variables based on Chlngs
summary(data$Chlngs)                                      # Before
data$ChlngsNoFlsh = ifelse(is.na(data$Chlngs), NA,
                               ifelse(grepl("NoFlsh", data$Chlngs,
                                            fixed = TRUE), "1", "0"))
data$ChlngsNoFlsh = as.factor(data$ChlngsNoFlsh)
summary(data$ChlngsNoFlsh)                                # After
summary(data$Chlngs)                                      # Before
data$ChlngsFlood = ifelse(is.na(data$Chlngs), NA,
                           ifelse(grepl("Flood", data$Chlngs,
                                        fixed = TRUE), "1", "0"))
data$ChlngsFlood = as.factor(data$ChlngsFlood)
summary(data$ChlngsFlood)                                # After
summary(data$Chlngs)                                      # Before
data$ChlngsOthr = ifelse(is.na(data$Chlngs), NA,
                          ifelse(grepl("Othr", data$Chlngs,
                                       fixed = TRUE), "1", "0"))
data$ChlngsOthr = as.factor(data$ChlngsOthr)
summary(data$ChlngsOthr)                                # After
summary(data$Chlngs)                                      # Before
data$ChlngsFulOvrFlw = ifelse(is.na(data$Chlngs), NA,
                         ifelse(grepl("Ful/OvrFlw", data$Chlngs,
                                      fixed = TRUE), "1", "0"))
data$ChlngsFulOvrFlw = as.factor(data$ChlngsFulOvrFlw)
summary(data$ChlngsFulOvrFlw)                                # After
summary(data$Chlngs)                                      # Before
data$ChlngsNoWtr = ifelse(is.na(data$Chlngs), NA,
                              ifelse(grepl("NoWtr", data$Chlngs,
                                           fixed = TRUE), "1", "0"))
data$ChlngsNoWtr = as.factor(data$ChlngsNoWtr)
summary(data$ChlngsNoWtr)                                # After
summary(data$Chlngs)                                      # Before
data$ChlngsSmels = ifelse(is.na(data$Chlngs), NA,
                          ifelse(grepl("Smels", data$Chlngs,
                                       fixed = TRUE), "1", "0"))
data$ChlngsSmels = as.factor(data$ChlngsSmels)
summary(data$ChlngsSmels)                                # After
summary(data$Chlngs)                                      # Before
data$ChlngsOK = ifelse(is.na(data$Chlngs), NA,
                          ifelse(grepl("OK", data$Chlngs,
                                       fixed = TRUE), "1", "0"))
data$ChlngsOK = as.factor(data$ChlngsOK)
summary(data$ChlngsOK)                                # After

# Rename responses in Satis
summary(data$Satis)                                      # Before
levels(data$Satis) = c("DK", 3, 2, 4, 1, 5)
summary(data$Satis)                                      # After

# Rename responses in SatisSup
summary(data$SatisSup)                                      # Before
levels(data$SatisSup) = c("DK", 3, 2, 4, 1, 5)
summary(data$SatisSup)                                      # After

# Remove data that is not applicable to this study
data = subset(data, Prov != "Phnom Penh")
data = droplevels(data)

# Remove data with low frequencies
data = subset(data, Prov != "Takeo" & Prov != "Kampong Cham" &
                Prov != "Kampong Speu")
data = droplevels(data)

# Remove data that are not of interest
names(data)
data = subset(data, select = -c(ID, Date, RGend, RisC, CrelR, DateSlabPur,
                                LatInst, BelowGrndInst, DateBelowGrndInst,
                                ShltrInst, DateInstComp, RDefBeforOthr,
                                WhoInstLat, KnwSubsdy, RecSubsdy, 
                                BorwLat, CanBuyLat, UseFincOthr, SlabTil,
                                Npits, PitConfig, NringsDir, NringsOff,
                                NringsOthr, ShltrWallTyp, ShltrWallTypOthr,
                                LatRoofTyp, LatRoofTypOthr, MatsPurTgthr,
                                Cost, CostInclInst, CostPitSlab, 
                                CostPitSlabKnwn, CostShltrMats,
                                CostShltrMatsKnwn, LabPitShltrPurTgthr,
                                CostLabLat, CostLabPitSlab,
                                CostLabPitSlabKnwn, CostLabShltr,
                                CostLabShltrKnwn, LatTypOwndBefor,
                                IntndChngOthr, IntndPitFullOthrAns,
                                DateSurvCreated, M01, M1824, M217,
                                M2559, M60, F01, F1824, F217, F2559, F60, LBO))
names(data)

# Set NAs equal to "DK" or "NA"
# summary(data, maxsum = 20)
# data$ChldUseLat[is.na(data$ChldUseLat)] = "DK/NA"
# data$InfLatDump[is.na(data$InfLatDump)] = "DK/NA"
# summary(data)

###############################################################################
# LOAD DATA FROM RAINFALL DATA FILE
###############################################################################
file_to_import = paste(getwd(), "/data/rainfall_cambodia_mm.xlsx",
                       sep = "")
data2 = import(file_to_import)
names(data2)

###############################################################################
# CLEAN DATA2
###############################################################################
names(data2) = c("Mnth", "1", "2", "3", "4", "5", "6", "7", "8")
names(data2)
summary(data2)
data2$Mnth = c(1:12)
library("reshape2")
library("ggplot2")
data2_long = melt(data2, id = "Mnth")  # convert to long format
names(data2_long) = c("Mnth", "Region", "Rainfall_mm")
ggplot(data = data2_long, aes(x = Mnth, y = Rainfall_mm, colour = Region)) +
  geom_line(aes(colour = Region), size = 1.5) +
  theme_minimal() +
  labs(x = "Month", y = "Rainfall (mm)", colour = "Regions") +
  scale_x_continuous(breaks = c(1:12)) +
  coord_cartesian(xlim = c(1, 12), ylim = c(0, max(data2_long$Rainfall_mm) + 30))
# Rainfall data not yet ready to add to primary data set (data)
# Need to get locations of villages to determine which regions apply to which
# provinces (see next section)
# Cannot proceed with detailed location (see below)
# Using province level only to map regions to data
# prov.locs = data.frame(Prov = names(summary(data$Prov)),
#                        Area_km2 = c(6185, 4536, 6838, ...)
prov.rainfall = 
  data.frame(Prov = names(summary(data$Prov)),
             Percent_Reg1 = c(0.60, 0.60,    0, 0.40, 0.60, 0.50,    0, 0.50, 0.20, 0.02,    0), 
             Percent_Reg2 = c(0.15, 0.17,    0, 0.20, 0.20, 0.30, 0.50,    0, 0.60,    0, 0.30), 
             Percent_Reg3 = c(0.25, 0.17,    0, 0.25,    0, 0.20,    0,    0, 0.20,    0,    0), 
             Percent_Reg4 = c(   0, 0.04, 0.03, 0.10, 0.10,    0, 0.50, 0.50,    0, 0.98, 0.50), 
             Percent_Reg5 = c(   0, 0.02, 0.10, 0.01, 0.10,    0,    0,    0,    0,    0, 0.12),
             Percent_Reg6 = c(   0,    0, 0.17, 0.04,    0,    0,    0,    0,    0,    0, 0.08), 
             Percent_Reg7 = c(   0,    0,    0,    0,    0,    0,    0,    0,    0,    0,    0), 
             Percent_Reg8 = c(   0,    0, 0.70,    0,    0,    0,    0,    0,    0,    0,    0 ))
provs = prov.rainfall[1]
# prov.rainfall[1] = 1:11
for (i in 1:length(prov.rainfall[,1])) {
  print(sum(prov.rainfall[i, 2:length(prov.rainfall)]))
}
names(prov.rainfall)
for (i in 1:12) {
  prov.rainfall[i + 9] = prov.rainfall$Percent_Reg1 * data2[i,2] + 
                          prov.rainfall$Percent_Reg2 * data2[i,3] + 
                          prov.rainfall$Percent_Reg3 * data2[i,4] + 
                          prov.rainfall$Percent_Reg4 * data2[i,5] + 
                          prov.rainfall$Percent_Reg5 * data2[i,6] + 
                          prov.rainfall$Percent_Reg6 * data2[i,7] + 
                          prov.rainfall$Percent_Reg7 * data2[i,8] + 
                          prov.rainfall$Percent_Reg8 * data2[i,9]
}
prov.rainfall
names(prov.rainfall) = c("Prov", "pReg1", "pReg2", "pReg3", "pReg4", 
                         "pReg5", "pReg6", "pReg7", "pReg8", "Jan_mm", 
                         "Feb_mm", "Mar_mm", "Apr_mm", "May_mm", "Jun_mm", "Jul_mm",
                         "Aug_mm", "Sep_mm", "Oct_mm", "Nov_mm", "Dec_mm")
prov.rainfall = subset(prov.rainfall, select = c(Prov, Jan_mm, Feb_mm, Mar_mm,
                                                 Apr_mm, May_mm, Jun_mm, Jul_mm,
                                                 Aug_mm, Sep_mm, Oct_mm, Nov_mm,
                                                 Dec_mm))
names(prov.rainfall) = c("Prov", "1", 
                         "2", "3", "4", "5", "6", "7",
                         "8", "9", "10", "11", "12")
prov.rainfall.long = melt(prov.rainfall, id = "Prov")  # convert to long format
names(prov.rainfall.long) = c("Prov", "Mnth", "Rainfall_mm")
prov.rainfall.long$Mnth = as.integer(prov.rainfall.long$Mnth)
ggplot(data = prov.rainfall.long, aes(x = Mnth, y = Rainfall_mm, colour = Prov)) +
  geom_line(aes(colour = Prov), size = 1.5) +
  theme_minimal() +
  labs(x = "Month", y = "Rainfall (mm)", colour = "Provinces") +
  scale_x_continuous(breaks = c(1:12)) +
  coord_cartesian(xlim = c(1, 12), ylim = c(0, max(data2_long$Rainfall_mm) + 30))
summary(data$Mnth)
data$Rain.mm = NA
for (i in 1:length(data$Prov)) {
  data$Rain.mm[i] = 
    prov.rainfall.long$Rainfall_mm[
      as.integer(data$Mnth[i]) == prov.rainfall.long$Mnth &
        data$Prov[i] == prov.rainfall.long$Prov]
}
summary(data$Rain.mm)

###############################################################################
# LOAD DATA FROM LOCATIONS DATA FILE
###############################################################################
file_to_import = paste(getwd(), "/data/locations.xlsx",
                       sep = "")
locs = import(file_to_import)
locs$Prov = as.factor(locs$Prov)
locs$Dist = as.factor(locs$Dist)
# summary(locs, maxsum = 200)
# locs$Prov[locs$Dist == "Samraong"]
# data.dist = levels(data$Dist)
# locs.dist = levels(locs$Dist)
# for (i in 1:length(data.dist)) {
#   print(paste(data.dist[i], locs.dist[i], sep = "     "))
# }
# names(data)
# locs$Dist = factor(locs$Dist, levels = levels(data$Dist))
sort(locs$Dist)
data$DistE = NA
for (i in 1:length(data$Dist)) {
  print(paste(i, data$Prov[i], data$Dist[i], sep = ", "))
  data$DistE[i] = try(locs$DistE[
    as.character(data$Prov[i]) == as.character(locs$Prov) &
      as.character(data$Dist[i]) == as.character(locs$Dist)], silent = T)
  if (inherits(data$DistE[i], "try-error")) {
    print("error1")
    next
  }
}
data$DistN = NA
for (i in 1:length(data$Dist)) {
  print(paste(i, data$Prov[i], data$Dist[i], sep = ", "))
  data$DistN[i] = try(locs$DistN[
    as.character(data$Prov[i]) == as.character(locs$Prov) &
      as.character(data$Dist[i]) == as.character(locs$Dist)], silent = T)
  if (inherits(data$DistN[i], "try-error")) {
    print("error1")
    next
  }
}
head(data.frame(Easting = data$DistE, Northing = data$DistN))
utms = SpatialPoints(cbind(data$DistE, data$DistN),
                     proj4string = CRS("+proj=utm +zone=48P"))
longlat = spTransform(utms, CRS("+proj=longlat"))
head(data$DistE)
head(data$DistN)
head(longlat)
plot(longlat)
head(longlat@coords[,2])
data$DistLong = longlat@coords[,1]
data$DistLat = longlat@coords[,2]
head(data$DistLat)
head(data.frame(Easting = data$DistE, Northing = data$DistN,
                Long = data$DistLong, Lat = data$DistLat))

###############################################################################
# SAVE LOCATIONS DATA TO DISK
###############################################################################
save(locs, file = "locations.RData")
# load(file = "locations.RData")

###############################################################################
# SAVE DATA TO DISK
###############################################################################
save(data, file = "iDE_Oct2017.RData")
# load(file = "iDE_Oct2017.RData")

###############################################################################
# DATA QUALITY CONTROL
###############################################################################
# Visualize NAs
missmap(data[1:10], main = "Missing vs observed", legend = F)
missmap(data[11:20], main = "Missing vs observed", legend = F)
missmap(data[21:30], main = "Missing vs observed", legend = F)
missmap(data[31:40], main = "Missing vs observed", legend = F)
missmap(data[41:50], main = "Missing vs observed", legend = F)

# Search for NAs and reduce to important data
summary(test)
sapply(data[31:40], function(x) sum(is.na(x)))
test2 = subset(test, !is.na(IntndPitFull))
sapply(test2, function(x) sum(is.na(x)))
summary(test2)
test3 = subset(test2, !is.na(IntndChng))
summary(test3)

# TASKS REMAINING
# Skipped data$RDefBeforOthr
# Khmer character in Borw$Lat
# Clean CanBuyLat thru LatTypOwndBefor
# Double check IntndChng renames
# Skipped data$IntndChngOthr
# Skipped data$IntndPitFullOthr
# Skipped data$DateSurvCreated
