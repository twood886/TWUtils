#' @title Named Group Split
#' @description Splits a group and Adds Name
#' @import tidyverse
#' @export
named_group_split <- function(.tbl, ...){

    grouped <- group_by(.tbl, ...)
    names <- rlang::eval_bare(rlang::expr(paste(!!!group_keys(grouped), sep = " / ")))

    grouped %>%
        group_split() %>%
        rlang::set_names(names)
}


#' @title Bloomberg Custom Index - Index Divisor
#' @description Function to calcualted index divisor for list of securities
#' @details Will enter
#' @param Portfolio Dataframe containing Date, Bloomberg Security Identifier, Share Position
#' @return
#' @import tidyverse
#' @import Rblpapi
#' @export
bb_customindex_calculate_divisor<-function(Portfolio){

    Px<-bdh(securities = unique(Portfolio$Security),
            fields = "PX_LAST",
            start.date = min(Portfolio$Date),
            end.date = max(Portfolio$Date),
            options = c(
                "nonTradingDayFillOption"="ALL_CALENDAR_DAYS",
                "nonTradingDayFillMethod"="PREVIOUS_VALUE")) %>%
        bind_rows( .id = "Security") %>%
        rename(Price = PX_LAST)

    #   Calcualte Divisor
    Divisor<- Portfolio %>%
        mutate(Date = as.Date(Date)) %>%
        complete(Date = seq.Date(min(Date), max(Date), by="day"), Security) %>%
        full_join(., Px, by = c("Date" = "date", "Security" = "Security")) %>%
        arrange(Date) %>%
        group_by(Security) %>%
        mutate(Position.Prev = lag(Position, 1)) %>%
        mutate(Price.Prev = lag(Price, 1)) %>%
        ungroup() %>%
        mutate(Position.Prev = ifelse(Date == min(Date), Position, Position.Prev)) %>%
        mutate(Price.Prev = ifelse(Date == min(Date), Price, Price.Prev)) %>%
        group_by(Date) %>%
        summarize(
            Cur.Mkt.Cap = sum(Position * Price.Prev, na.rm = T),
            Prv.Prc.Cap = sum(Position.Prev * Price.Prev, na.rm = T),
            Adjustment = Cur.Mkt.Cap / Prv.Prc.Cap) %>%
        mutate(Orig.Cap = first(Cur.Mkt.Cap),
               Cum.Adjustment = cumprod(Adjustment),
               Divisor = Cum.Adjustment * Orig.Cap / 100) %>%
        select(Date, Divisor)

    return(Divisor)
}


#' @title Bloomberg Custom Index - Aggregate Value
#' @description Function to calcualted aggregate value for a portfolio
#' @param Portfolio Dataframe containing Date, Bloomberg Security Identifier, Share Position
#' @param Field Character Bloomberg Field
#' @param Overrides Named Character Vector containing RBlpapi Overrides for Field
#' @return
#' @import tidyverse
#' @import Rblpapi
#' @export
bb_customindex_aggregate_value<-function(Portfolio, Field, Overrides){

    Px<-bdh(securities = unique(Portfolio$Security),
            fields = "PX_LAST",
            start.date = min(Portfolio$Date),
            end.date = max(Portfolio$Date),
            options = c(
                "nonTradingDayFillOption"="ALL_CALENDAR_DAYS",
                "nonTradingDayFillMethod"="PREVIOUS_VALUE")) %>%
        bind_rows( .id = "Security") %>%
        rename(`Price` = `PX_LAST`)

    Values<-bdh(securities = unique(Portfolio$Security),
                fields = c(Field," EQY_SH_OUT"),
                start.date = min(Portfolio$Date),
                end.date = max(Portfolio$Date),
                options = c(
                    "nonTradingDayFillOption"="ALL_CALENDAR_DAYS",
                    "nonTradingDayFillMethod"="PREVIOUS_VALUE"),
                overrides = Overrides) %>%
        bind_rows( .id = "Security") %>%
        rename(`Value` = all_of(Field))

    Out<-
        full_join(
            Portfolio,
            Px,
            by = c("Date" = "date", "Security")) %>%
        full_join(
            .,
            Values,
            by = c("Date" = "date", "Security")) %>%
        mutate(
            `Value` = `Value` / `EQY_SH_OUT`,
            `Ptfl.Value` = `Position`  * `Price`) %>%
        group_by(Date) %>%
        summarise(
            `Pct.Avail` = sum(ifelse(is.na(`Value`),0,`Ptfl.Value`), na.rm = T) / sum(`Ptfl.Value`, na.rm = T),
            `Agg.Value` = sum(`Position` * `Value`, na.rm = T))

    return(Out)
}


#' @title Bloomberg Custom Index - Value Per Share
#' @description Function to calcualted value per share for list of securities
#' @param Portfolio Dataframe containing Date, Bloomberg Security Identifier, Share Position
#' @param Field Character Bloomberg Field
#' @param Overrides Named Character Vector containing RBlpapi Overrides for Field
#' @return
#' @import tidyverse
#' @import Rblpapi
#' @export
bbcustomindex_aggregate_valuepershare<-function(Portfolio, Field, Overrides){

    Divisor<-bb_customindex_calculate_divisor(Portfolio)

    Px<-bdh(securities = unique(Portfolio$Security),
            fields = "PX_LAST",
            start.date = min(Portfolio$Date),
            end.date = max(Portfolio$Date),
            options = c(
                "nonTradingDayFillOption"="ALL_CALENDAR_DAYS",
                "nonTradingDayFillMethod"="PREVIOUS_VALUE")) %>%
        bind_rows( .id = "Security") %>%
        rename(`Price` = PX_LAST)

    Values<-bdh(securities = unique(Portfolio$Security),
                fields = Field,
                start.date = min(Portfolio$Date),
                end.date = max(Portfolio$Date),
                options = c(
                    "nonTradingDayFillOption"="ALL_CALENDAR_DAYS",
                    "nonTradingDayFillMethod"="PREVIOUS_VALUE"),
                overrides = Overrides) %>%
        bind_rows( .id = "Security") %>%
        rename(`Value` = all_of(Field))

    Out<-
        full_join(
            Portfolio,
            Px,
            by = c("Date" = "date", "Security")) %>%
        full_join(
            .,
            Values,
            by = c("Date" = "date", "Security")) %>%
        mutate(Ptfl.Value = Position  * Price) %>%
        group_by(Date) %>%
        summarise(
            `Pct.Avail` = sum(ifelse(is.na(`Value`),0,`Ptfl.Value`), na.rm = T) / sum(`Ptfl.Value`, na.rm = T),
            `Agg.Value` = sum(`Position` * `Value`, na.rm = T)) %>%
        full_join(
            .,
            Divisor,
            by = c("Date" = "Date")) %>%
        mutate(`Agg.Val.Avail.Adj` = `Agg.Value` / `Pct.Avail`) %>%
        mutate(`Agg.Val.Per.Share` = `Agg.Val.Avail.Adj` / `Divisor`) %>%
        select(`Date`, `Agg.Val.Per.Share`)

    return(Out)
}


#' @title Bloomberg Custom Index - Aggregate Value (Data from Filings)
#' @description Function to calcualted aggregate value for a portfolio using filing data
#' @param Portfolio Dataframe containing Date, Bloomberg Security Identifier, Share Position
#' @param Field Character Bloomberg Field
#' @param Overrides Named Character Vector containing RBlpapi Overrides for Field
#' @return
#' @import tidyverse
#' @import Rblpapi
#' @export
bbcustomindex_aggregate_value_fa<-function(Portfolio, Field, Overrides){

    Px<-bdh(securities = unique(Portfolio$Security),
            fields = "PX_LAST",
            start.date = min(Portfolio$Date),
            end.date = max(Portfolio$Date),
            options = c(
                "nonTradingDayFillOption"="ALL_CALENDAR_DAYS",
                "nonTradingDayFillMethod"="PREVIOUS_VALUE")) %>%
        bind_rows( .id = "Security") %>%
        rename(Price = PX_LAST)



    Values <-bdh(
        securities = c("AAPL US Equity", "MSFT US Equity"),
        fields = c("RR020", "IS_SH_FOR_DILUTED_EPS"),
        start.date = as.Date("2020-01-01"),
        end.date = as.Date("2021-05-09"),
        options = c(
            "nonTradingDayFillOption"="ALL_CALENDAR_DAYS",
            "nonTradingDayFillMethod"="PREVIOUS_VALUE"),
        overrides = c(
            "DS323" = "Q"))


    Values<-plyr::ldply(


        as.character(format(seq.Date(min(Portfolio$Date),max(Portfolio$Date), by = "day"),"%Y%m%d")),
        function(x, Securities, Field, Overrides){
            Val<-bdh(securities = Securities,
                     start.date = min(Portfolio$Date),
                     end.date = max(Portfolio$Date),
                     fields = c(Field, "IS_SH_FOR_DILUTED_EPS"),
                     overrides = c(
                         Overrides,
                         "DY891" = x)) %>%
                rownames_to_column('Securities') %>%
                mutate(`Date` = as.Date(x, format = "%Y%m%d")) %>%
                rename(`Value` = all_of(Field))

        },
        Securities = unique(Portfolio$Security),
        Overrides = Overrides,
        Field = Field)


    Out<-
        full_join(
            Portfolio,
            Px,
            by = c("Date" = "date", "Security")) %>%
        full_join(
            .,
            Values,
            by = c("Date" = "Date", "Security" = "Securities")) %>%
        mutate(
            `Value` = `Value` / `IS_SH_FOR_DILUTED_EPS`,
            `Ptfl.Value` = `Position`  * `Price`) %>%
        group_by(Date) %>%
        summarise(
            `Pct.Avail` = sum(ifelse(is.na(`Value`),0,`Ptfl.Value`), na.rm = T) / sum(`Ptfl.Value`, na.rm = T),
            `Agg.Value` = sum(`Position` * `Value`, na.rm = T))

        return(Out)
}


#' @title Bloomberg Custom Index - Value Per Share (Data from Filings)
#' @description Function to calcualted value per share for list of securities using filing data
#' @param Portfolio Dataframe containing Date, Bloomberg Security Identifier, Share Position
#' @param Field Character Bloomberg Field
#' @param Overrides Named Character Vector containing RBlpapi Overrides for Field
#' @return
#' @import tidyverse
#' @import Rblpapi
#' @export
bbcustomindex_aggregate_valuepershare_fa<-function(Portfolio, Field, Overrides){

    Divisor<-bb_customindex_calculate_divisor(Portfolio)

    Px<-bdh(securities = unique(Portfolio$Security),
            fields = "PX_LAST",
            start.date = min(Portfolio$Date),
            end.date = max(Portfolio$Date),
            options = c(
                "nonTradingDayFillOption"="ALL_CALENDAR_DAYS",
                "nonTradingDayFillMethod"="PREVIOUS_VALUE")) %>%
        bind_rows( .id = "Security") %>%
        rename(`Price` = PX_LAST)

    Values<-plyr::ldply(
        as.character(format(seq.Date(min(Portfolio$Date),max(Portfolio$Date), by = "day"),"%Y%m%d")),
        function(x, Securities, Field, Overrides){
            Val<-bdp(securities = Securities,
                     fields = Field,
                     overrides = c(
                         Overrides,
                         "DY891" = x)) %>%
                rownames_to_column('Securities') %>%
                mutate(`Date` = as.Date(x, format = "%Y%m%d")) %>%
                rename(`Value` = all_of(Field))

        },
        Securities = unique(Portfolio$Security),
        Overrides = Overrides,
        Field = Field)


    Out<-
        full_join(
            Portfolio,
            Px,
            by = c("Date" = "date", "Security")) %>%
        full_join(
            .,
            Values,
            by = c("Date" = "Date", "Security" = "Securities")) %>%
        mutate(`Ptfl.Value` = `Position`  * `Price`) %>%
        group_by(`Date`) %>%
        summarise(
            `Pct.Avail` = sum(ifelse(is.na(`Value`),0,`Ptfl.Value`), na.rm = T) / sum(`Ptfl.Value`, na.rm = T),
            `Agg.Value` = sum(`Position` * `Value`, na.rm = T)) %>%
        full_join(
            .,
            Divisor,
            by = c("Date" = "Date")) %>%
        mutate(`Agg.Val.Avail.Adj` = `Agg.Value` / `Pct.Avail`) %>%
        mutate(`Agg.Val.Per.Share` = `Agg.Val.Avail.Adj` / `Divisor`) %>%
        select(`Date`, `Agg.Val.Per.Share`)

    return(Out)
}


