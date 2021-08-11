# Author: Taylor Wood
# Purpose: The following code is used to download all of the data on
# Kenneth R. French's Website. This data is highly useful in measuring
# performance and other statistics of factors that drive cross-sectional
# returns in equity markets.

library(tidyverse)
library(rvest)
library(readr)
library(lubridate)
library(janitor)

FFExtract <- function(df){
  # Extracts description and data for a dataset from Ken French's website.
  #
  # Args:
  #   df: Dataframe containing name/description of data followed by data set
  #       with labels in first rows.
  #
  # Returns:
  #   List containing
  #   Description (char) - the description of the data
  #   Data (numeric dataframe) - the data
  name.location <- str_which(df$V1, "[A-z]+")  # Location of data description
  # If no description give NA otherwise combine multiple rows
  if(length(name.location) == 0){
    name <- NA
  }else{
    name <- str_c(str_trim(df[name.location,1][[1]]), collapse = " ")
  }
  data.location <- str_which(df$V1, "^\\s*[0-9]{4,}")  # Location of data
  data <- df[data.location,] %>%  # Clean data frame
    `colnames<-`(df[min(data.location)-1,]) %>%  # Assign first row as col names
    clean_names() %>%  # Make col names clean
    rename(`Date` = 1) %>%  # First col name will be blank so give "Date"
    mutate_all(as.numeric)  # Since df had chars for description convert to num
  return(list("Description" = name, "Data" = data))
}

FFDataDownload <- function(name, child){
  # Downloads CSV Data from supplied HTML child node from Ken French's website.
  #
  # Args:
  #   name: name of child node
  #   child: HTML child node
  #
  # Returns:
  #   List containing
  #   Description: Description of dataset
  #   Data: List of data sets with descriptions from file
  file.name <- child %>%  # Create working URL of child
    str_extract("(?<=/).+(?=\\_CSV\\.zip)") %>%
    str_c(".CSV")
  temp <- tempfile()  # Create temporary file for downloading data

  download.file(  # Download zip file from Ken French website
    url = str_c(
      "https://mba.tuck.dartmouth.edu/pages/faculty/ken.french/",
      child),
    destfile = temp)

  unzip(temp) # Unzip data file

  raw.data <- read.csv(  # Read in CSV file
    file.name,
    header = FALSE,
    sep = ",",
    col.names = paste0("V",seq_len(200)),
    fill = T,
    blank.lines.skip = F)

  # Remove Leading rows
  file.desc <- raw.data[1:(min(str_which(raw.data$V1, "^[0-9]{4,}"))-2),1]
  raw.data <- raw.data[(min(str_which(raw.data$V1, "^[0-9]{4,}"))-2):nrow(raw.data),]

  data.chunks <- raw.data %>%  # Split data into different data chunks
    filter(!str_detect(.[[1]], "Copyright")) %>%
    select_if(~!all(is.na(.))) %>%
    select_if(~!all(. == "")) %>%
    mutate(
      `istext` = ifelse(str_detect(.[[1]], "[a-z]+"),1,0),
      `change` = ifelse(`istext` == 1 & lag(`istext`,1, default = 0) == 0 , 1, 0),
      `group` = cumsum(`change`)) %>%
    select(-c(`istext`, `change`)) %>%
    group_split(`group`, .keep = F)

  extracted.list <- lapply(data.chunks,  # Extract desc/data from chunks
                           FFExtract)
  return(list("Description" = file.desc,
              "Data" = extracted.list))
}


# Download all the links to the CSV files on Kenneth R. French's website.
# Read HTML and find <b> nodes
# Identify nodes with "CSV.zip"
kenfrench.url <- "https://mba.tuck.dartmouth.edu/pages/faculty/ken.french/data_library.html"

csv.links <- kenfrench.url %>%
  read_html() %>%
    html_nodes("b") %>%
    as.character() %>%
    as_tibble(column_name = "value") %>%
    mutate(`lead2` = lead(`value`, 2)) %>%
    filter(str_detect(`lead2`, "CSV.zip")) %>%
    transmute(
        `Name` = str_extract(`value`, "(?<=\\<b\\>).*(?=\\</b\\>)"),
        `Link` = str_extract(`lead2`, "ftp.+zip"))

# Download all Data
# This will take a while so commented out for safety
#famafrench.data <- mapply(
#  FFDataDownload,
#  name = csv.links$Name,
#  child = csv.links$Link,
#  SIMPLIFY = FALSE)



#   Use Example
#   Plot Fama-Frnech 3 Factor Model Data
famafrench.data <- mapply(
  FFDataDownload,
  name = "Fama/French 3 Factors [Daily]",
  child = csv.links$Link[which(csv.links$Name == "Fama/French 3 Factors [Daily]")],
  SIMPLIFY = F)

ff.3factor <- famafrench.data$`Fama/French 3 Factors [Daily]`$Data[[1]]$Data %>%
    transmute(
        `Date` = as.Date(as.character(`Date`), "%Y%m%d"),
        `Rm_Rf_Cum` = cumprod(1 + `mkt_rf`/100) - 1,
        `SMB_Cum` = cumprod(1 + `smb`/100) - 1,
        `HML_Cum` = cumprod(1 + `hml`/100) - 1) %>%
    pivot_longer(
        cols = c(`Rm_Rf_Cum`, `SMB_Cum`, `HML_Cum`),
        names_to = "factor_name",
        values_to = "factor_value")
ggplot(ff.3factor) +
    aes(x = Date, y = factor_value, colour = factor_name) +
    scale_y_continuous(labels = scales::percent) +
    geom_line(size = 0.5) +
    labs(
        x = "Date",
        y = "Cumualtive Return",
        title = "Fama French 3 Factor Model",
        subtitle = "Cumulative Return",
        color = "Factor Name"
    ) +
    theme_minimal()
