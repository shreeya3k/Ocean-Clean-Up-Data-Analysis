#import the data first
library(readxl)
library(dplyr)
library(ggplot2)
library(tidyr)
Data_Level5_BAH_OceanCleanup <- read_excel("Downloads/Level5_BAH_OceanCleanup/Data_Level5_BAH_OceanCleanup.xlsx")

#get list of unique states 
statelist <- unique(Data_Level5_BAH_OceanCleanup$State)
print(statelist)

#first prep some variable lists to help with the sorting algorithm
#column names
names <- c("State","Garbage1","Total1",
           "Garbage2","Total2",
           "Garbage3","Total3",
           "Garbage4","Total4",
           "Garbage5","Total5",
           "Garbage6","Total6",
           "Garbage7","Total7",
           "Garbage8","Total8",
           "Garbage9","Total9",
           "Garbage10","Total10")
Popular_Garbage_by_State <- data.frame(matrix(ncol = 21, nrow = 0))
colnames(Popular_Garbage_by_State) <- names



#for loop for each unique state in the table...
for(i in 1:length(statelist))
{
  #set up some more variables
  names2 <- c("GarbageType","GarbageTotal")
  State_Garbage_Totals <- data.frame(matrix(ncol = 2, nrow = 0))
  colnames(State_Garbage_Totals) <- names2
  
  #filter cleanup data to only include rows with statelist[i] in the State column
   Filtered_CleanupData <- dplyr::filter(Data_Level5_BAH_OceanCleanup,State == statelist[i])
  
  #once that's done, fill out state garbage totals... (Warning: long, unefficient block of code...)
   total <- sum(Filtered_CleanupData$`Cigarette Butts`)
   new_row1 <- c("Cigarette Butts",total)
   State_Garbage_Totals <- rbind(State_Garbage_Totals,new_row1,stringsAsFactors = FALSE)
   total <- sum(Filtered_CleanupData$`Food Wrappers (candy, chips, etc.)`)
   new_row1 <- c("Food Wrappers (candy, chips, etc.)",total)
   State_Garbage_Totals <- rbind(State_Garbage_Totals,new_row1,stringsAsFactors = FALSE)
   total <- sum(Filtered_CleanupData$`Take Out/Away Containers (Plastic)`)
   new_row1 <- c("Take Out/Away Containers (Plastic)",total)
   State_Garbage_Totals <- rbind(State_Garbage_Totals,new_row1,stringsAsFactors = FALSE)
   total <- sum(Filtered_CleanupData$`Take Out/Away Containers (Foam)`)
   new_row1 <- c("Take Out/Away Containers (Foam)",total)
   State_Garbage_Totals <- rbind(State_Garbage_Totals,new_row1,stringsAsFactors = FALSE)
   total <- sum(Filtered_CleanupData$`Bottle Caps (Plastic)`)
   new_row1 <- c("Bottle Caps (Plastic)",total)
   State_Garbage_Totals <- rbind(State_Garbage_Totals,new_row1,stringsAsFactors = FALSE)
   total <- sum(Filtered_CleanupData$`Bottle Caps (Metal)`)
   new_row1 <- c("Bottle Caps (Metal)",total)
   State_Garbage_Totals <- rbind(State_Garbage_Totals,new_row1,stringsAsFactors = FALSE)
   total <- sum(Filtered_CleanupData$`Lids (Plastic)`)
   new_row1 <- c("Lids (Plastic)",total)
   State_Garbage_Totals <- rbind(State_Garbage_Totals,new_row1,stringsAsFactors = FALSE)
   total <- sum(Filtered_CleanupData$`Straws, Stirrers`)
   new_row1 <- c("Straws, Stirrers",total)
   State_Garbage_Totals <- rbind(State_Garbage_Totals,new_row1,stringsAsFactors = FALSE)
   total <- sum(Filtered_CleanupData$`Forks, Knives, Spoons`)
   new_row1 <- c("Forks, Knives, Spoons",total)
   State_Garbage_Totals <- rbind(State_Garbage_Totals,new_row1,stringsAsFactors = FALSE)
   total <- sum(Filtered_CleanupData$`Beverage Bottles (Plastic)`)
   new_row1 <- c("Beverage Bottles (Plastic)",total)
   State_Garbage_Totals <- rbind(State_Garbage_Totals,new_row1,stringsAsFactors = FALSE)
   total <- sum(Filtered_CleanupData$`Beverage Bottles (Glass)`)
   new_row1 <- c("Beverage Bottles (Glass)",total)
   State_Garbage_Totals <- rbind(State_Garbage_Totals,new_row1,stringsAsFactors = FALSE)
   total <- sum(Filtered_CleanupData$`Beverage Cans`)
   new_row1 <- c("Beverage Cans",total)
   State_Garbage_Totals <- rbind(State_Garbage_Totals,new_row1,stringsAsFactors = FALSE)
   total <- sum(Filtered_CleanupData$`Grocery Bags (Plastic)`)
   new_row1 <- c("Grocery Bags (Plastic)",total)
   State_Garbage_Totals <- rbind(State_Garbage_Totals,new_row1,stringsAsFactors = FALSE)
   total <- sum(Filtered_CleanupData$`Other Plastic Bags`)
   new_row1 <- c("Other Plastic Bags",total)
   State_Garbage_Totals <- rbind(State_Garbage_Totals,new_row1,stringsAsFactors = FALSE)
   total <- sum(Filtered_CleanupData$`Paper Bags`)
   new_row1 <- c("Paper Bags",total)
   State_Garbage_Totals <- rbind(State_Garbage_Totals,new_row1,stringsAsFactors = FALSE)
   total <- sum(Filtered_CleanupData$`Cups, Plates (Paper)`)
   new_row1 <- c("Cups, Plates (Paper)",total)
   State_Garbage_Totals <- rbind(State_Garbage_Totals,new_row1,stringsAsFactors = FALSE)
   total <- sum(Filtered_CleanupData$`Cups, Plates (Plastic)`)
   new_row1 <- c("Cups, Plates (Plastic)",total)
   State_Garbage_Totals <- rbind(State_Garbage_Totals,new_row1,stringsAsFactors = FALSE)
   total <- sum(Filtered_CleanupData$`Cups, Plates (Foam)`)
   new_row1 <- c("Cups, Plates (Foam)",total)
   State_Garbage_Totals <- rbind(State_Garbage_Totals,new_row1,stringsAsFactors = FALSE)
   total <- sum(Filtered_CleanupData$`Fishing Buoys, Pots & Traps`)
   new_row1 <- c("Fishing Buoys, Pots & Traps",total)
   State_Garbage_Totals <- rbind(State_Garbage_Totals,new_row1,stringsAsFactors = FALSE)
   total <- sum(Filtered_CleanupData$`Fishing Net & Pieces`)
   new_row1 <- c("Fishing Net & Pieces",total)
   State_Garbage_Totals <- rbind(State_Garbage_Totals,new_row1,stringsAsFactors = FALSE)
   total <- sum(Filtered_CleanupData$`Fishing Line (1 yard/meter = 1 piece)`)
   new_row1 <- c("Fishing Line (1 yard/meter = 1 piece)",total)
   State_Garbage_Totals <- rbind(State_Garbage_Totals,new_row1,stringsAsFactors = FALSE)
   total <- sum(Filtered_CleanupData$`Rope (1 yard/meter = 1 piece)`)
   new_row1 <- c("Rope (1 yard/meter = 1 piece",total)
   State_Garbage_Totals <- rbind(State_Garbage_Totals,new_row1,stringsAsFactors = FALSE)
   total <- sum(Filtered_CleanupData$`Fishing Gear (Clean Swell)`)
   new_row1 <- c("Dishing Gear (Clean Swell)",total)
   State_Garbage_Totals <- rbind(State_Garbage_Totals,new_row1,stringsAsFactors = FALSE)
   total <- sum(Filtered_CleanupData$`6-Pack Holders`)
   new_row1 <- c("6-Pack Holders",total)
   State_Garbage_Totals <- rbind(State_Garbage_Totals,new_row1,stringsAsFactors = FALSE)
   total <- sum(Filtered_CleanupData$`Other Plastic/Foam Packaging`)
   new_row1 <- c("Other Plastic/Foam Packaging",total)
   State_Garbage_Totals <- rbind(State_Garbage_Totals,new_row1,stringsAsFactors = FALSE)
   total <- sum(Filtered_CleanupData$`Other Plastic Bottles (oil, bleach, etc.)`)
   new_row1 <- c("Other Plastic Bottles (oil, bleach, etc.)",total)
   State_Garbage_Totals <- rbind(State_Garbage_Totals,new_row1,stringsAsFactors = FALSE)
   total <- sum(Filtered_CleanupData$`Strapping Bands`)
   new_row1 <- c("Strapping Bands",total)
   State_Garbage_Totals <- rbind(State_Garbage_Totals,new_row1,stringsAsFactors = FALSE)
   total <- sum(Filtered_CleanupData$`Tobacco Packaging/Wrap`)
   new_row1 <- c("Tobacco Packaging/Wrap",total)
   State_Garbage_Totals <- rbind(State_Garbage_Totals,new_row1,stringsAsFactors = FALSE)
   total <- sum(Filtered_CleanupData$`Other Packaging (Clean Swell)`)
   new_row1 <- c("Other Packaging (Clean Swell)",total)
   State_Garbage_Totals <- rbind(State_Garbage_Totals,new_row1,stringsAsFactors = FALSE)
   total <- sum(Filtered_CleanupData$`Appliances (refrigerators, washers, etc.)`)
   new_row1 <- c("Appliances (refrigerators, washers,etc.)",total)
   State_Garbage_Totals <- rbind(State_Garbage_Totals,new_row1,stringsAsFactors = FALSE)
   total <- sum(Filtered_CleanupData$Balloons)
   new_row1 <- c("Balloons",total)
   State_Garbage_Totals <- rbind(State_Garbage_Totals,new_row1,stringsAsFactors = FALSE)
   total <- sum(Filtered_CleanupData$`Cigar Tips`)
   new_row1 <- c("Cigar Tips",total)
   State_Garbage_Totals <- rbind(State_Garbage_Totals,new_row1,stringsAsFactors = FALSE)
   total <- sum(Filtered_CleanupData$`Cigarette Lighters`)
   new_row1 <- c("Cigarette Lighters",total)
   State_Garbage_Totals <- rbind(State_Garbage_Totals,new_row1,stringsAsFactors = FALSE)
   total <- sum(Filtered_CleanupData$`Construction Materials`)
   new_row1 <- c("Construction Materials",total)
   State_Garbage_Totals <- rbind(State_Garbage_Totals,new_row1,stringsAsFactors = FALSE)
   total <- sum(Filtered_CleanupData$Fireworks)
   new_row1 <- c("Fireworks",total)
   State_Garbage_Totals <- rbind(State_Garbage_Totals,new_row1,stringsAsFactors = FALSE)
   total <- sum(Filtered_CleanupData$Tires)
   new_row1 <- c("Tires",total)
   State_Garbage_Totals <- rbind(State_Garbage_Totals,new_row1,stringsAsFactors = FALSE)
   total <- sum(Filtered_CleanupData$Toys)
   new_row1 <- c("Toys",total)
   State_Garbage_Totals <- rbind(State_Garbage_Totals,new_row1,stringsAsFactors = FALSE)
   total <- sum(Filtered_CleanupData$`Other Trash (Clean Swell)`)
   new_row1 <- c("Other Trash (Clean Swell)",total)
   State_Garbage_Totals <- rbind(State_Garbage_Totals,new_row1,stringsAsFactors = FALSE)
   total <- sum(Filtered_CleanupData$Condoms)
   new_row1 <- c("Condoms",total)
   State_Garbage_Totals <- rbind(State_Garbage_Totals,new_row1,stringsAsFactors = FALSE)
   total <- sum(Filtered_CleanupData$Diapers)
   new_row1 <- c("Diapers",total)
   State_Garbage_Totals <- rbind(State_Garbage_Totals,new_row1,stringsAsFactors = FALSE)
   total <- sum(Filtered_CleanupData$Syringes)
   new_row1 <- c("Syringes",total)
   State_Garbage_Totals <- rbind(State_Garbage_Totals,new_row1,stringsAsFactors = FALSE)
   total <- sum(Filtered_CleanupData$`Tampons/Tampon Applicators`)
   new_row1 <- c("Tampons/Tampon Applicators",total)
   State_Garbage_Totals <- rbind(State_Garbage_Totals,new_row1,stringsAsFactors = FALSE)
   total <- sum(Filtered_CleanupData$`Personal Hygiene (Clean Swell)`)
   new_row1 <- c("Personal Hygiene (Clean Swell)",total)
   State_Garbage_Totals <- rbind(State_Garbage_Totals,new_row1,stringsAsFactors = FALSE)
   
   print(State_Garbage_Totals)
  
  #sort that by descending totals
   names3 <- c("Type","Number")
   colnames(State_Garbage_Totals) <- names3
   State_Garbage_Totals$Number = as.integer(State_Garbage_Totals$Number)
   State_Garbage_Totals <- arrange(State_Garbage_Totals,desc(Number))
   
  #now get the first ten rows & format it into single row
   new_row2 <- c(statelist[i],State_Garbage_Totals[1,1],State_Garbage_Totals[1,2],
                 State_Garbage_Totals[2,1],State_Garbage_Totals[2,2],
                 State_Garbage_Totals[3,1],State_Garbage_Totals[3,2],
                 State_Garbage_Totals[4,1],State_Garbage_Totals[4,2],
                 State_Garbage_Totals[5,1],State_Garbage_Totals[5,2],
                 State_Garbage_Totals[6,1],State_Garbage_Totals[6,2],
                 State_Garbage_Totals[7,1],State_Garbage_Totals[7,2],
                 State_Garbage_Totals[8,1],State_Garbage_Totals[8,2],
                 State_Garbage_Totals[9,1],State_Garbage_Totals[9,2],
                 State_Garbage_Totals[10,1],State_Garbage_Totals[10,2])
   
  #insert that row into main table
  Popular_Garbage_by_State <- rbind(Popular_Garbage_by_State,new_row2,stringsAsFactors = FALSE)
}

#renaming my columns because my code is inefficient
names4 <- c("State","#1 Garbage Type","Total",
           "#2 Garbage Type","Total",
           "#3 Garbage Type","Total",
           "#4 Garbage Type","Total",
           "#5 Garbage Type","Total",
           "#6 Garbage Type","Total",
           "#7 Garbage Type","Total",
           "#8 Garbage Type","Total",
           "#9 Garbage Type","Total",
           "#10 Garbage Type","Total")

colnames(Popular_Garbage_by_State) <- names4

