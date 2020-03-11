require(tidyverse)

# folder <- choose.dir(caption = "Select main analysis folder")
folder <- "analysis/data/"

# load MasterTable. Change for analysis of different data set
MasterTable <- read.csv2(file = paste0(folder, "metadata/MasterTable.csv"), header = TRUE, sep = ";",
                         na.strings = "NA")
# load master list of column labels
mastercolnames <- read.csv2(file = paste0(folder, "metadata/mastercolnames.csv"),
                            header = FALSE, sep = ";") %>%
  unlist()

# load Table of Conditions. Change for analysis of different data set
Conditions <- read.csv2(file = paste0(folder, "metadata/Conditions.csv"), header = TRUE, sep = ";")

# helper function to read csv file with raw data and preprocess them
load_raw_csv <- function(path) {
  nthday <- read.csv2(file = path,  sep = ";", dec = ".", header = TRUE,
            fileEncoding = "UTF-16LE", as.is = TRUE, row.names = NULL)

  # Special case for the "Males" data set, which has a different number of columns:
  # solution: remove last column
  if (ncol(nthday) == 19) {nthday <- nthday[, -19]}

  # ISSUE: Most mouse files have a ";" too many just before "SystemMsg", leading to mismatched
  # column labels
  # SOLUTION: Add a "Dummy" column label to a list of labels from
  # a "mastercolnames" file and then apply to all subsequent files

  if (exists("mastercolnames")) {colnames(nthday) <- mastercolnames}

  {colnames(nthday) <- mastercolnames} # rename column labels with correct order
  nthday

  nthday <- nthday %>%
    arrange(DateTime)

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
}

days <- as.list(MasterTable$day)
paths <- as.list(paste0(folder, MasterTable$path))

# function for aggregating data from all days and adding correct day column
aggregate_days <- function(paths, days) {
  map(paths, load_raw_csv) %>%
    set_names(days) %>%
    enframe("day", "day_data") %>%
    unnest(day_data) %>%
    mutate(day = as.numeric(day))
}


alldays <- aggregate_days(paths, days) %>%
  arrange(DateTime) %>% #sort chronologically
  mutate(DateTime = as.numeric(str_replace(DateTime, ",", ".")),
         DateTime = as.POSIXct(as.numeric(DateTime) * (60 * 60 * 24),
                               origin = "1899-12-30", tz = "UTC"))

# remove columns with irrelevant data
alldays <- alldays %>%
  select(day, DateTime, IdLabel, unitLabel, eventDuration, reinforce1value, SystemMsg)


# make new columns
alldays <- alldays %>%
  mutate(
    # numeric column for reward volume delivered
    vol = ifelse(is.na(reinforce1value), 0, reinforce1value / 3),
    # numeric column for reward status 1=rewarded, 0=unrewarded, helps calculate reward rates
    rewardstatus = ifelse(vol > 0, 1, vol),
    # create location column from the unitLabels
    loc = as.integer(str_extract(unitLabel, "[0-9]+"))
    )

# merge the Conditions table and the current data table to make new columns for dispenser properties
# such as maximal volume, sugar concentration, probability, etc., as well as experimental conditions
# days to discard, etc.

alldays <- left_join(alldays, Conditions, by = c("day", "IdLabel", "loc")) %>%
  select(day, IdLabel, loc, everything()) %>%
  # sort chronologically again
  arrange(DateTime)


alldays <- alldays %>%
  #discard data labelled for discarding in Conditions table
  filter(discard == 0) %>%
  select(-discard, -reinforce1value) %>%
  #calculate several new columns:
  mutate(
    #boolean column for whether an event was a choice (at least 200ms interruption)
    choice = (eventDuration > 200) & !is.na(loc),
    #boolean column for whether a choice was actually a poke (IR beam interrupted)
    poke = choice & str_detect(unitLabel, "Cond")
    )

# focus only on the pokes
onlychoices <- alldays %>%
  filter(poke) %>%
  select(-choice,-poke,-unitLabel) %>%
  arrange(IdLabel, day, DateTime)

write.table(onlychoices, file = paste0(folder, "ChoicesOutput.csv"), sep = ";", row.names = FALSE)
