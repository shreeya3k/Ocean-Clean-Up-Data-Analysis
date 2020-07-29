#California County codes source: https://www.nrcs.usda.gov/wps/portal/nrcs/detail/ca/home/?cid=nrcs143_013697


library(readxl)
library(dplyr)
library(ggplot2)
library(tidyr)
library(stringr)
library(gsubfn)
Data_Level5_BAH_OceanCleanup <- read_excel("Downloads/Level5_BAH_OceanCleanup/Data_Level5_BAH_OceanCleanup.xlsx")
CA_Data <- read_excel("Downloads/Level5_BAH_OceanCleanup/CA_Counties.xlsx")

OceanCleanup <- filter(Data_Level5_BAH_OceanCleanup,State == "California, USA")
for(i in 1:58)
{
  CA_Data$Zone[i] <- paste0(CA_Data$Zone[i]," County, CA, USA")
}

CA_Map <- inner_join(OceanCleanup,CA_Data,by = "Zone",copy = FALSE)

