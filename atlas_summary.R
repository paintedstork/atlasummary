library (googlesheets)
library (splitstackshape)
library (dplyr)
library (reshape2)
library (jsonlite)
library (curl)


# Number of Species reported in atleast 5% of the subcells
# This gives an indication of how widespread the species is
noOf5percentSubCellSpecies <- function (DataSet, filename, ebd)
{
  
  DataSet    <- as.data.frame (DataSet)
  
  DataSet    <- DataSet[!(DataSet$Lists=="SNA"),]
  
  # Remove duplicated lists
  uDataSet   <- DataSet[!duplicated(DataSet[c("Lists")]),]
  
  ebd           <- inner_join (ebd, uDataSet, 
                               by = c("SAMPLING.EVENT.IDENTIFIER"="Lists"))
  
  ebd   <- ebd[!duplicated(ebd[c("SPECIES.NAME","Subcell")]),]
  
  density <- as.data.frame (table (ebd$SPECIES.NAME)) 
  
  uSubcells   <- DataSet[!duplicated(DataSet[c("Subcell")]),]
  nrow (density[density$Freq > (nrow(uSubcells)/20), ])
}

# For a district, it will combine the dry and wetseason sheets
combineSeasons <- function (district, SpreadSheet)
{
  dryseasonlists <- as.data.frame (SpreadSheet[,paste(district,"-Dry Season", sep='')]$Lists) 
  colnames (dryseasonlists) <- c ("Lists")
  
  wetseasonlists <- as.data.frame (SpreadSheet[,paste(district,"-Wet Season", sep='')]$Lists) 
  colnames (wetseasonlists) <- c ("Lists")
  
  dryseasonsubcells <- as.data.frame (SpreadSheet[,paste(district,"-Dry Season", sep='')]$Subcell) 
  colnames (dryseasonsubcells) <- c ("Subcell")
  
  wetseasonsubcells <- as.data.frame (SpreadSheet[,paste(district,"-Wet Season", sep='')]$Subcell) 
  colnames (wetseasonsubcells) <- c ("Subcell")
  
  mergedlists <-
    as.data.frame (  
      cbind (
        rbind (dryseasonsubcells, 
               wetseasonsubcells),
        rbind (dryseasonlists, 
               wetseasonlists)))
  
  colnames(mergedlists) <- c("Subcell","Lists")
  
  return (mergedlists)
}

# Number of Species reported in atleast 5% of the lists
# This gives an indication of how abundant the species is
noOf5percentListSpecies <- function (DataSet, filename, ebd)
{
  
  DataSet    <- as.data.frame (DataSet)
  
  DataSet    <- DataSet[!(DataSet$Lists=="SNA"),]
  
  duplists <- within (DataSet, DUP <-  
                        duplicated(DataSet[c("Lists")]))
  
  if(nrow(duplists[duplists$DUP == TRUE,]) > 0 )
  {
    duplists$filename <- filename
    write.table(duplists[duplists$DUP == TRUE,], "duplist.csv", append=TRUE)
  }
  
  # Remove duplicated lists
  uDataSet   <- DataSet[!duplicated(DataSet[c("Lists")]),]
  
  ebd           <- inner_join (ebd, uDataSet, 
                               by = c("SAMPLING.EVENT.IDENTIFIER"="Lists"))
  
  mapped_lists  <- ebd[!duplicated(ebd[c("SAMPLING.EVENT.IDENTIFIER")]),]
  
  orphaned_lists <- anti_join (DataSet, mapped_lists, by = c("Lists"="SAMPLING.EVENT.IDENTIFIER"))
  
  if(nrow(orphaned_lists) > 0 )
  {
    orphaned_lists$filename <- filename
    write.table(orphaned_lists, "orphanedlist.csv", append=TRUE)
  }
  
  ebd   <- ebd[!duplicated(ebd[c("SPECIES.NAME","SAMPLING.EVENT.IDENTIFIER")]),]
  
  density <- as.data.frame (table (ebd$SPECIES.NAME)) 
  
  nrow (density[density$Freq > (nrow(uDataSet)/20), ])
}

# Number of Species 
noOfSpecies <- function (DataSet, ebd)
{
  lists    <- as.data.frame(DataSet$Lists)
  
  ebd         <- inner_join (ebd, lists, 
                             by = c("SAMPLING.EVENT.IDENTIFIER"="DataSet$Lists"))
  
  #Create species list by removing duplicate species entries
  ebd_species   <- ebd[!duplicated(ebd$SPECIES.NAME),]
  
  ebd_species   <- rbind (ebd_species [ebd_species$CATEGORY == "species",],
                          ebd_species [ebd_species$CATEGORY == "domestic",])
  
  nrow(ebd_species)
}

# Writes the dataset into a file 
writeFiles <- function (fileindex, filename, DataSet)
{
  cbind (as.data.frame(DataSet$Subcell), 
         as.data.frame(DataSet$Lists)) %>% 
         write.csv2 (paste(filename,".csv", sep=''))
}


nwriteFiles <- function (fileindex, filename, DataSet)
{
  cbind (as.data.frame(DataSet$Subcell), 
         as.data.frame(DataSet$Lists),
         as.data.frame(rep(filename, length(DataSet$Lists)))) %>% 
     write.table ("Kerala.csv", append=TRUE, col.names=FALSE)
#     print(filename)
}

# Very useful function
# Reads the Google spreadsheet and creates a list of eBird Lists 
sheetRead <- function (filename)  {
  
  atlas_file <- gs_title(filename)
  gs_ws_ls(atlas_file)
  
  # Read without column names to avoid type casting to integer
  lists_sheet <- gs_read(ss=atlas_file, 
                         ws = "Birds Lists",
                         range = anchored ("A1", dim = c(500, 6)),
                         col_names = FALSE,
                         skip=0)
  lists <- as.data.frame(lists_sheet)
  
  # Make first row as header
  colnames(lists) <- as.character(lists[1,])
  
  # Remove redundant header row
  lists = lists[-1,]
  
  flatlist <- as.data.frame (
    rbind ( cbind (lists [,1], lists [,3]),
            cbind (lists [,1], lists [,4]),
            cbind (lists [,1], lists [,5]),
            cbind (lists [,1], lists [,6])
    ))
  
  flatlist <- cSplit(flatlist, 'V2', sep="S", type.convert=FALSE)
  flatlist <- within (flatlist, Lists <-  paste ("S",V2_2, sep=""))
  
  flatlist <- subset (flatlist, select = c("V1", "Lists"))
  colnames(flatlist) <- c("Subcell","Lists")
  
  flatlist    <- flatlist[!(is.na(flatlist$Subcell)),]
  
  return (flatlist)
}


#Lists the files in your Google Drive
gs_ls()


# Remove districts which are not required. 
districts <- c (
  "Kasaragod",
  "Kannur",
  "Wayanad",
  "Kozhikode",
  "Malappuram",
  "Palakkad",
  "Thrissur",
  "Ernakulam",
  "Idukki",
  "Kottayam",
  "Alappuzha",
  "Pathanamthitta",
  "Kollam",
  "TVM"
)


# Replace this with a line to load eBird data 
#unzip(paste('..\\data\\','ebd_IN-KL_relMay-2018','.zip',sep=''))
#south.mo <- read.delim(paste('ebd_IN-KL_relMay-2018','.txt',sep=''), na.strings = c("NA", "", "null"), as.is=TRUE, quote="")

#load('..\\data\\southernStates-2018-05-31-trim.RData')

# Number of species per season per district
#SpeciesRichness <- sapply (spreadSheetData, noOfSpecies, ebd = south.mo)
#write.csv2(SpeciesRichness, "species.csv")

#if (file.exists("duplist.csv")) file.remove("duplist.csv")
#if (file.exists("orphanedlist.csv")) file.remove("orphanedlist.csv")

# Number of species that occurs in 5% of lists per district
#abundantSpecies <- mapply (noOf5percentListSpecies, DataSet = spreadSheetData, filename = files,MoreArgs = list(ebd = south.mo))
#write.csv2(abundantSpecies, "abundantSpecies.csv")

# Number of species that occurs in 5% of sub-cells per district
#wideSpreadSpecies <- mapply (noOf5percentSubCellSpecies, DataSet = spreadSheetData, filename = files,MoreArgs = list(ebd = south.mo))
#write.csv2(wideSpreadSpecies, "wideSpreadSpecies.csv")

#Very useful function
#Checks if a list is a valid eBird list.
verifyChecklist <- function (Checklist)
{
   if(Checklist != "SNA")
   {
     h <- new_handle()
     handle_setheaders(h,
                      "X-eBirdApiToken" = "Your Token"
     )  
     print(Checklist)
     req <- curl_fetch_memory(paste("https://ebird.org/ws2.0/product/checklist/view/", Checklist, sep =''),h)
     Sys.sleep(0.2)     
     return (req$status_code)
   }
   else
   {
     return (200)
   }
}

#Converts the response JSON to Dataframe
convertToDf <- function (parsed)
{
  if ( length(parsed$obs) > 0)
  {
        dat <- as.data.frame ( list (parsed$obs$speciesCode,parsed$obs$obsDt,parsed$obs$howManyAtleast) )
  } 
  else
  {
        print(paste("Empty/Hidden list", parsed$subId))
        dat <- as.data.frame ( list ("empty species", parsed$obsDt, 0))
  }

  colnames(dat)       <- c("Species", "Date", "Count")
  dat$subId           <- parsed$subId
  dat$protocolId      <- parsed$protocolId
  dat$locId           <- parsed$locId
  dat$duration        <- parsed$durationHrs
  dat$complete        <- parsed$allObsReported
  dat$numObservers    <- parsed$numObservers
  dat$userDisplayName <- parsed$userDisplayName
  dat$group           <- ifelse(is.null(parsed$group),"NA",parsed$group)
  return (dat)
}

#Very useful function
#Reads eBird Checklists
readChecklist <- function (Checklist, filename)
{
  if(Checklist != "SNA")
  {
    Sys.sleep(0.2)     
    h <- new_handle()
    handle_setheaders(h,
                      "X-eBirdApiToken" = "5efhcrnce7q0"
    )  
#    print(Checklist)
    req <- curl_fetch_memory(paste("https://ebird.org/ws2.0/product/checklist/view/", Checklist, sep =''),h)
    
    if(req$status_code == 200)
    {
      jsonlite::prettify(rawToChar(req$content)) %>% 
         fromJSON(flatten=FALSE) %>%
             convertToDf() %>%
                write.table(paste(filename,"_Data.csv", sep =''), sep=";", 
                            append = TRUE,
                              col.names = FALSE)  
      return (req$status_code)
    }
    else
    {
      print(paste("Brokenlink",Checklist))
      return (req$status_code)
    }
  }
  else
  {
    return (200)
  }
}

#Uses readChecklist fo capture data from lists in a file
captureListsOnline <- function(filename, sep = ' ')
{
  atlaslists <- read.csv(paste(filename,".csv", sep =''), sep = sep, header=FALSE, stringsAsFactors = F)
  if (sep == ';')
  {
    colnames(atlaslists) <- c("No", "Subcell", "Lists")
    atlaslists    <- atlaslists[!(atlaslists$Lists=="SNA"),]
  }
  else
  {
    colnames(atlaslists) <- c("No", "Subcell", "Lists", "District-Season")
    atlaslists    <- atlaslists[!(atlaslists$Lists=="SNA"),]
  }
  
  unlink(paste(filename,"_Data.csv", sep =''))  
  atlaslists$status <- mapply (readChecklist, Checklist = atlaslists$Lists, filename = filename)
  write.table(atlaslists, paste(filename,"_Status.csv", sep =''), sep=";")  
}

transformListsData <- function(filename, sep = ' ')
{
  unlink(paste(filename,"_AtlasData.csv", sep =''))  
  atlasdata <- read.csv(paste0(filename,"_Data.csv"), sep = sep, header=FALSE, stringsAsFactors = F)
  status <- read.csv(paste0(filename, "_Status.csv"), sep = ';')
  
  names(atlasdata) <- c("No",
                        "Spcode", 
                        "Date",
                        "NoOfBirds",
                        "Lists",
                        "Protocol",
                        "Location", 
                        "Duration",
                        "AllSpecies",
                        "NoOfObservers",
                        "Observer",
                        "GroupID")
  
  atlasdata$Duration <- as.integer(60*atlasdata$Duration)
  
  ebd <- read.csv2("eBird2019.csv", sep=",")
  
  atlasdata <- left_join(atlasdata, ebd, by = "Spcode")  
  atlasdata <- left_join(atlasdata, status, by = "Lists")
  
  col_order <- c("English.Name", 
                 "Scientific.Name", 
                 "NoOfBirds",
                 "Category", 
                 "Lists", 
                 "Duration", 
                 "Location", 
                 "Protocol", 
                 "Date", 
                 "NoOfObservers", 
                 "AllSpecies", 
                 "Subcell",
                 "GroupID")
  atlasdata <- atlasdata[, col_order]
  
  write.table(atlasdata, paste(filename,"_AtlasData.csv", sep =''), sep=";", row.names=FALSE)  
}


#Very Useful function
#Uses verifyChecklist to list the broken checklist links
listBrokenLists <- function(filename, sep = ' ')
{
  atlaslists <- read.csv(paste(filename,".csv", sep =''), sep = sep, header=FALSE, stringsAsFactors = F)
  if (sep == ';')
  {
    colnames(atlaslists) <- c("No", "Subcell", "Lists")
    atlaslists    <- atlaslists[!(atlaslists$Lists=="SNA"),]
  }
  else
  {
    colnames(atlaslists) <- c("No", "Subcell", "Lists", "District-Season")
    atlaslists    <- atlaslists[!(atlaslists$Lists=="SNA"),]
  }

  atlaslists$status <- mapply (verifyChecklist, Checklist = atlaslists$Lists)
  write.table(atlaslists, paste(filename,"_Status.csv", sep =''), sep=";")  
}

#Very useful function
#Often admins make mistake of copying the same checklist twice in spreadsheet
#This function finds all duplicates marking TRUE aganist it.
listDuplicateLists <- function(filename, sep = ' ')
{
  atlaslists <- read.csv(paste(filename,".csv", sep =''), sep = sep, header=FALSE, stringsAsFactors = F)
  if (sep == ';')
  {
    colnames(atlaslists) <- c("No", "Subcell", "Lists")
    atlaslists    <- atlaslists[!(atlaslists$Lists=="SNA"),]
    
    duplists <- within (atlaslists, DUP <-  
                          duplicated(atlaslists[c("Lists")]))
  }
  else
  {
    colnames(atlaslists) <- c("No", "Subcell", "Lists", "District-Season")
    atlaslists    <- atlaslists[!(atlaslists$Lists=="SNA"),]
    
    duplists <- within (atlaslists, DUP <-  
                          duplicated(atlaslists[c("Lists", "District-Season")]))
  }
  if(nrow(duplists[duplists$DUP == TRUE,]) > 0 )
  {
    write.table(duplists, paste(filename,"_Duplicate.csv", sep =''), sep=";")  
  }
  else
  {
    print(paste("No Duplicate Lists in", filename))
  }
}

#Main body of the program

seasons <- c ("Dry Season",
              "Wet Season")

# Create the spreadsheet file names
files <- outer(districts,seasons, paste, sep="-") 
dim(files) <- NULL

# Spreadsheet Data
spreadSheetData <- as.data.frame (sapply(files, sheetRead, simplify = TRUE))

saveRDS(spreadSheetData, "Spreadsheet.rds")
#Spreadsheet with all the checklist links is saved here.

spreadSheetData <- readRDS("Spreadsheet.rds")


#files <- districts
#spreadSheetData <- as.data.frame ( sapply (districts, combineSeasons, SpreadSheet = spreadSheetData))

# Write them into individual csvs
mapply (writeFiles, fileindex = 1:length(files), filename = files, DataSet = spreadSheetData)
mapply (nwriteFiles, fileindex = 1:length(files), filename = files, DataSet = spreadSheetData)

#listDuplicateLists(filename = "Kerala")
listBrokenLists(filename = "Kerala")
#listDuplicateLists(filename = "Idukki-Dry Season", sep= ';')

#listBrokenLists(filename = "Idukki-Dry Season", sep= ';')
captureListsOnline(filename = "Ernakulam-Dry Season", sep= ';')
transformListsData(filename="Ernakulam-Dry Season", sep= ';')

