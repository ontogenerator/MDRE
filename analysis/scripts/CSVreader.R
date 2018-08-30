require(dplyr)
require(stringr)


folder <- choose.dir(caption = "Select folder with 'MasterTable' and other CSV files to load")
# folder <- "C:/Users/Vladi/Documents/MDRE/analysis"
setwd(folder)


# load MasterTable. Change for analysis of different data set
MasterTable <- read.csv2(file = paste0(folder, "/scripts/MasterTable.csv"), header = TRUE, sep = ";",
                         na.strings = "NA")
# load master list of column labels
mastercolnames <- read.csv2(file = paste0(folder, "/scripts/mastercolnames.csv"),
                            header = FALSE, sep = ";") %>%
  unlist()

#load Table of Conditions. Change for analysis of different data set
Conditions <- read.csv2(file = paste0(folder, "/scripts/Conditions.csv"), header = TRUE, sep = ";")

# only get the unique days and paths for each day
daypathlist <- MasterTable

#determine the number of days
ndays <- length(daypathlist[,1])
alldays <- c() # alldays is the sink for all daily data. reset in case it already exists

#main loop over the experimental days listed in the MasterTable; data from all days is aggregated
if (ndays > 0) {
  for (i in 1:ndays) {

    nthday <- read.csv2(file = as.character(daypathlist$path[i]),  sep = ";", dec = ",", header = TRUE,
                        fileEncoding = "UTF-16LE", as.is = T, row.names = NULL) #read csv file with raw data
    # ISSUE: Most mouse files have a ";" too many just before "SystemMsg", leading to mismatched
    # column labels
    # SOLUTION: Add a "Dummy" column label, load the list of labels from
    # a "mastercolnames" file and then apply to all subsequent files.

    # Special case for the "Males" data set, which has a different number of columns:
    # solution: remove last column
    if (ncol(nthday) == 19) {nthday <- nthday[, -19]}

    if (exists("mastercolnames")) {colnames(nthday) <- mastercolnames}

    if (i > 1)
    {colnames(nthday) <- mastercolnames} # rename column labels with correct order

    nthday <- nthday %>%
      arrange(DateTime) #sort chronologically first

    #find out first line of useful data
    firstrow <- nthday %>%
      mutate(rown = row_number(DateTime)) %>%
      filter(SystemMsg == "start") %>%
      summarise(firstrow = max(rown) + 1) %>%
      pull()
    if (is.na(firstrow) | is.infinite(firstrow)) { # in rare cases the start line is missing
      # then take the first rewarded visit of a mouse as the firstrow
      firstrow <- nthday %>%
        mutate(rown = row_number(DateTime)) %>%
        filter(IdLabel != "test", reinforce1value > 0) %>%
        summarise(firstrow = min(rown)) %>%
        pull()
}

    # find the last row number, larger than firstrow and preceding a row with "exp" in the
    # "unitLabel" column
    lastrow <- c()
    lastrow <- nthday %>%
      mutate(rown = row_number(DateTime)) %>%
      filter(unitLabel == "exp", rown > firstrow) %>%
      summarise(lastrow = min(rown) - 1) %>%
      pull()
    if (is.na(lastrow) | is.infinite(lastrow)) {lastrow <- nrow(nthday)}

    #only select relevant data and discard the rest
    nthday <- nthday %>%
      slice(firstrow:lastrow)

    #make a day column with the day number
    nthday$day <- daypathlist[i,1]

    #aggregate data from all days in one table
    if (i > 1) {
      alldays <- rbind(alldays, nthday)
    } else{
      alldays <- nthday
      # if this is the first day to be analyzed, save the column labels
      # in case they are missing or corrupt in later files
      if (!exists("mastercolnames")) {
        mastercolnames <- colnames(alldays)
        #remove the "day" column name added in line 60
        mastercolnames <- mastercolnames[-length(mastercolnames)]
      }
    }
  }


  #reformat DateTime and display properly, as characters
  alldays$DateTime <- sub(",", ".", alldays$DateTime)
  alldays$DateTime <- as.character(as.POSIXct(as.numeric(alldays$DateTime) * (60*60*24),
                                              origin = "1899-12-30", tz = "GMT"))

  #remove columns with irrelevant data
  alldays <- alldays %>%
    select(-IdRFID, -sense1duration, -sense1Events, -senseRFIDrecords,
           -reinforce1Total, -reinforce1Account, -outFuncLabel, -outLabel,
           -Dummy, -MsgValue1, -MsgValue2, -MsgValue3)


  # make new columns
  alldays <- alldays %>%
    mutate(
      #numeric column for reward volume delivered
      vol = ifelse(is.na(reinforce1value), 0, reinforce1value / 3),
      # numeric column for reward status 1=rewarded, 0=unrewarded, helps calculate reward rates
      rewardstatus = ifelse(vol > 0, 1, vol),
      #create location column from the unitLabels
      loc = as.integer(str_extract(unitLabel, "[0-9]+"))
      )


  # merge the Conditions table and the current data table to make new columns for dispenser properties
  # such as maximal volume, sugar concentration, probability, etc., as well as experimental conditions
  # days to discard, etc.


  alldays <- merge(alldays, Conditions,
                  by = c("day", "IdLabel", "loc"), all.x = TRUE, sort = FALSE)
  #sort chronologically again
  alldays <- alldays %>%
    arrange(DateTime)


  alldays <- alldays %>%
    #discard data labelled for discarding in Conditions table
    filter(discard == 0) %>%  select(-discard, -reinforce1value) %>%
    #calculate several new columns:
    mutate(
      #boolean column for whether an event was a choice (at least 200ms interruption)
      choice = (eventDuration > 200) & !is.na(loc),
      #boolean column for whether a choice was actually a poke (IR beam interrupted)
      poke = choice & str_detect(unitLabel, "Cond")
      )

  #focus only on the pokes
  onlychoices <- alldays %>%
    filter(poke) %>%
    select(-choice,-poke,-unitLabel) %>%
    arrange(IdLabel, day, DateTime)

  write.table(onlychoices, file = "ChoicesOutput.csv", sep = ";", row.names = FALSE)
}
