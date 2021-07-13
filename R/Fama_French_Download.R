#' @title Download Fama-French 3 Factor Model Data
#' @description Function to download data from Ken French Website for 3 Factor Model.
#' @details Download
#' @return famafrench3factor
#' @import tidyverse
#' @import readr
#' @import lubridate
#' @export
ff3factor<-function(){

    # Download Monthly & Annual Data
    temp <-tempfile()
    download.file("https://mba.tuck.dartmouth.edu/pages/faculty/ken.french/ftp/F-F_Research_Data_Factors_CSV.zip",temp, mode="wb", quiet = T)
    unzip(temp, "F-F_Research_Data_Factors.CSV")

    raw_data <- readr::read_csv(
        "F-F_Research_Data_Factors.CSV",
        col_types = cols(
            `HML` = col_double(),
            `Mkt-RF` = col_double(),
            `RF` = col_double(),
            `SMB` = col_double(), X1 = col_character()),
        skip = 3) %>%
        rename(`Date` = 1)


    annual_match <- which(grepl("Annual", raw_data$Date))[[1]]

    monthly_data <- raw_data[1:annual_match - 1,] %>%
        mutate(
            `Date` = `Date` %>%
                as.integer() %>%
                as.character() %>%
                paste(., "01", sep = "") %>%
                as.Date(format = "%Y%m%d") %>%
                ceiling_date(unit = "month") %>%
                - 1)

    annual_data <- raw_data[(annual_match + 1): nrow(raw_data), ] %>%
        mutate(
            `Date` = `Date` %>%
                as.integer() %>%
                as.character() %>%
                paste(., "1231", sep = "") %>%
                as.Date(., format = "%Y%m%d")) %>%
        filter(!is.na(`Date`))

    # Download Weekly Data
    temp <- tempfile()
    download.file("https://mba.tuck.dartmouth.edu/pages/faculty/ken.french/ftp/F-F_Research_Data_Factors_weekly_CSV.zip",temp, mode="wb")
    unzip(temp, "F-F_Research_Data_Factors_weekly.CSV")

    weekly_data <- read_csv(
        "F-F_Research_Data_Factors_weekly.CSV",
        col_types = cols(
            `HML` = col_double(),
            `Mkt-RF` = col_double(),
            `RF` = col_double(),
            `SMB` = col_double(), X1 = col_character()),
        skip = 4) %>%
        rename(`Date` = 1) %>%
        mutate(
            `Date` = `Date` %>%
                as.integer() %>%
                as.character() %>%
                as.Date(format = "%Y%m%d")) %>%
        filter(!is.na(`Date`))

    # Download Daily Data
    temp <- tempfile()
    download.file("https://mba.tuck.dartmouth.edu/pages/faculty/ken.french/ftp/F-F_Research_Data_Factors_daily_CSV.zip",temp, mode="wb")
    unzip(temp, "F-F_Research_Data_Factors_daily.CSV")

    daily_data <- read_csv(
        "F-F_Research_Data_Factors_daily.CSV",
        col_types = cols(
            `HML` = col_double(),
            `Mkt-RF` = col_double(),
            `RF` = col_double(),
            `SMB` = col_double(), X1 = col_character()),
        skip = 4) %>%
        rename(`Date` = 1) %>%
        mutate(
            `Date` = `Date` %>%
                as.integer() %>%
                as.character() %>%
                as.Date(format = "%Y%m%d")) %>%
        filter(!is.na(`Date`))


    ff_3_factor <- list(
        "annual" = annual_data,
        "monthly" = monthly_data,
        "weekly" = weekly_data,
        "daily" = daily_data)

    return(ff_3_factor)
}


#' @title Download Fama-French 5 Factor Model Data
#' @description Function to download data from Ken French Website for 5 Factor Model.
#' @details Download
#' @return famafrench3factor
#' @import tidyverse
#' @import readr
#' @import lubridate
#' @export
ff5factor<-function(){

    # Download Monthly & Annual Data
    temp <-tempfile()
    download.file("https://mba.tuck.dartmouth.edu/pages/faculty/ken.french/ftp/F-F_Research_Data_5_Factors_2x3_CSV.zip",temp, mode="wb", quiet = T)
    unzip(temp, "F-F_Research_Data_5_Factors_2x3.CSV")

    raw_data <- readr::read_csv(
        "F-F_Research_Data_5_Factors_2x3.CSV",
        col_types = cols(
            `X1` = col_character(),
            `Mkt-RF` = col_double(),
            `SMB` = col_double(),
            `HML` = col_double(),
            `RMW` = col_double(),
            `CMA` = col_double(),
            `RF` = col_double()),
        skip = 3) %>%
        rename(`Date` = 1)


    annual_match <- which(grepl("Annual", raw_data$Date))[[1]]

    monthly_data <- raw_data[1:annual_match - 1,] %>%
        mutate(
            `Date` = `Date` %>%
                as.integer() %>%
                as.character() %>%
                paste(., "01", sep = "") %>%
                as.Date(format = "%Y%m%d") %>%
                ceiling_date(unit = "month") %>%
                - 1)

    annual_data <- raw_data[(annual_match + 1): nrow(raw_data), ] %>%
        mutate(
            `Date` = `Date` %>%
                as.integer() %>%
                as.character() %>%
                paste(., "1231", sep = "") %>%
                as.Date(., format = "%Y%m%d")) %>%
        filter(!is.na(`Date`))


    # Download Daily Data
    temp <- tempfile()
    download.file("https://mba.tuck.dartmouth.edu/pages/faculty/ken.french/ftp/F-F_Research_Data_5_Factors_2x3_daily_CSV.zip",temp, mode="wb")
    unzip(temp, "F-F_Research_Data_5_Factors_2x3_daily.CSV")

    daily_data <-
        read_csv(
            "F-F_Research_Data_5_Factors_2x3_daily.CSV",
            col_types = cols(X1 = col_character(), .default = col_double()),
            skip = 4) %>%
        rename(`Date` = 1) %>%
        mutate(
            `Date` = `Date` %>%
                as.integer() %>%
                as.character() %>%
                as.Date(format = "%Y%m%d")) %>%
        filter(!is.na(`Date`))


    ff_5_factor <- list(
        "annual" = annual_data,
        "monthly" = monthly_data,
        "daily" = daily_data)

    return(ff_5_factor)
}
