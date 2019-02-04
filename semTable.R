load("M:/Anders L Kolstad/systherb data/TEMPERATURE PAPER/psem_summary.RData")
MySummary

library(plyr)

MyTable <- MySummary$coefficients[1:14,]
MyTable$Response <- revalue(MyTable$Response, replace = c(
                           "CCI" = "Canopy cover (%)",
                           "Soil_temp_C" =   "Soil temperature (˚C)",
                           "avenellaBM_l" = "Avenella flexuosa (g m-2)",
                           "allHerbs" = "Herb biomass (g m-2)",
                           "vasc_SR" = "Vascular plants (spp plot-1)",
                           "moss_SR" = "Bryophyte (spp plot-1)"))
                           
MyTable$Predictor <- revalue(MyTable$Predictor, replace = c(
  "TreatmentOpen plots" = "Herbivore exclusion",
  "CCI"                 = "Canopy cover (%)",
  "Moss_depth"          = "Bryophyte depth (cm)",
  "Moss_depth2"         = "(Bryophyte depth)2",
  "Soil_temp_C"         =   "Soil temperature (˚C)",
  "Soil_temp2"         =   "(Soil temperature)2",
  "allHerbs"            = "Herb biomass (g m-2)",
  "allHerbs2"           = "(Herb biomass)2"))
  #"avenellaBM_l" = "Avenella flexuosa (g m-2)",
 
MyTable$Estimate[MyTable$Predictor == "Herbivore exclusion"] <- 
  MyTable$Estimate[MyTable$Predictor == "Herbivore exclusion"]*(-1)
MyTable$Std.Estimate[MyTable$Predictor == "Herbivore exclusion"] <- 
  MyTable$Std.Estimate[MyTable$Predictor == "Herbivore exclusion"]*(-1)






MyTable2 <- MySummary$coefficients[15:43,]
MyTable2$Response <- revalue(MyTable2$Response, replace = c(
  
  "~~avenellaBM_l"      = "Avenella flexuosa",
  "~~NMDS1"             = "NMDS1",
  "~~vasc_SR"           = "Vascular plants"))


MyTable2$Predictor <- revalue(MyTable2$Predictor, replace = c(
  
  "~~CCI"                 = "Canopy cover",
  "~~Moss_depth"          = "Bryophyte depth",
  "~~Soil_temp_C"         =   "Soil temperature",
  "~~allHerbs"            = "Herb biomass",
  "~~vasc_SR"            = "Vascular plants",
  "~~moss_SR"            = "Bryophytes",
  "~~avenellaBM_l"      = "Avenella flexuosa"))

MyTable2 <- MyTable2[!grepl("~~", MyTable2$Predictor),]
MyTable2 <- MyTable2[!grepl("~~", MyTable2$Response),]
MyTable2$Std.Error <- " "
MyTable2$Crit.Value <- " "
MyTable2$Std.Estimate <- " "
MyTable3 <- rbind(MyTable, MyTable2)


library(dplyr)
colnames(MyTable3)[9] <- "sig."
MyTable3 <- select(MyTable3, 
                  Response, 
                  Predictor, 
                  "Std estimate" = Std.Estimate,  
                  "Raw estimate" = Estimate,
                  "SE" = Std.Error, 
                  DF, 
                  "p-value" =  P.Value,
                  sig.) 



MyTable3$`p-value` <- round(MyTable3$`p-value`, 3)
MyTable3$`Std estimate` <- as.numeric(MyTable3$`Std estimate`)
MyTable3$`Std estimate` <- round(MyTable3$`Std estimate`, 2)

MyTable3$`Raw estimate` <- round(MyTable3$`Raw estimate`, 2)
MyTable3$SE <- as.numeric(MyTable3$SE)
MyTable3$SE <- round(MyTable3$SE, 2)

MyTable3$`p-value`[MyTable3$`p-value` < 0.0011] <- "<0.001"

#setwd("M:\\Anders L Kolstad\\systherb data\\TEMPERATURE PAPER\\SEM manuscript and figures\\Ecosystems\\new")
write.csv(MyTable3, "SEMcoeffs.csv", row.names = F)




