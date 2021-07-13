#' @title Bloomberg Portfolio Holdings Download
#' @description Download Client Portfolio Holdings for Date
#' @param date Date in Date format
#' @param Portfolio character column name of Portfolio
#' @return data
#' @import tidyverse
#' @import Rblpapi
#' @export
bb_download_portfolio_equity_deltaexp<-function(
    date = Sys.Date(),
    Portfolio = "U22936327-2 Client"){

    #   Download portfolio current and previous postions
    PtflPos<-getPortfolio(
        security = Portfolio,
        field = "PORTFOLIO_MPOSITION",
        overrides = c(
            "REFERENCE_DATE" = as.character(format(date , "%Y%m%d")))) %>%
        mutate_all(~replace(., is.na(.), 0)) %>%
        filter(Position != 0) %>%
        select(Security, Position)


    #   Download asset class type
    SecClass<-bdp(securities = PtflPos$Security, fields = "EX028")
    SecClass$Security<-row.names(SecClass)

    #   Merge positions with asset class type and split
    PtflPos <- PtflPos %>%
        full_join(., SecClass, by = "Security") %>%
        filter(EX028 != "Currency") %>%
        mutate(Asset.Class = ifelse(EX028 %in% c("Equity","Option"), EX028, "Other")) %>%
        select(-EX028) %>%
        named_group_split(Asset.Class)

    # Out Data Frame
    PtflDta<-data.frame(
        "Security" = as.character(),
        "Asset.Class" = as.character(),
        "GICS.Sector" = as.character(),
        "Total.Return" = as.numeric(),
        "Position.Avg.Dol" = as.numeric())


    #   Options
    if("Option" %in% names(PtflPos)){

        #   Download underlying securities and contract size
        OptUnder<-bdp(
            securities = PtflPos$Option$Security,
            fields = c("DY216", "OP032")) %>%
            mutate(Security = row.names(.)) %>%
            rename(
                Option.Underlying = DY216,
                Contract.Size = OP032)

        #   Download Underlying security type
        UnderType<-bdp(
            securities = unique(OptUnder$Option.Underlying),
            fields = "EX028") %>%
            mutate(
                Option.Underlying = row.names(.),
                Underlying.Type = ifelse(EX028=="Equity", EX028, "Other")) %>%
            select(-EX028)

        #   Download Delta of options
        OptDelta<-bdh(
            securities = PtflPos$Option$Security,
            fields = "OP054",
            start.date = date - 1,
            end.date = date,
            options = c("nonTradingDayFillOption"="ALL_CALENDAR_DAYS",
                        "nonTradingDayFillMethod"="PREVIOUS_VALUE"))

        OptDelta<-bind_rows(OptDelta, .id = "Security") %>%
            rename(Delta = OP054) %>%
            group_by(Security) %>%
            summarise(Avg.Delta = mean(Delta, na.rm = TRUE), .groups = "drop")

        OptDta<-PtflPos$Option %>%
            full_join(., OptUnder, by = "Security") %>%
            full_join(., OptDelta, by = "Security") %>%
            full_join(., UnderType, by = "Option.Underlying") %>%
            mutate(Position = Position * Contract.Size * Avg.Delta) %>%
            select(Option.Underlying,
                   Underlying.Type,
                   Position) %>%
            rename(Security = Option.Underlying,
                   Asset.Class = Underlying.Type) %>%
            named_group_split(Asset.Class)

        if("Equity" %in% names(OptDta)){
            if("Equity" %in% names(PtflPos)){
                PtflPos$Equity<-bind_rows(PtflPos$Equity, OptDta$Equity)
            }else{
                PtflPos$Equity<-OptDta$Equity
            }
        }


        if("Other" %in% names(OptDta)){
            if("Other" %in% names(PtflPos)){
                PtflPos$Other<-bind_rows(PtflPos$Other, OptDta$Other)
            }else{
                PtflPos$Other<-OptDta$Other
            }
        }
    }

    #
    #   If get etf underlying add here
    #

    if("Other" %in% names(PtflPos)){PtflPos<-bind_rows(PtflPos$Equity, PtflPos$Other)}else{PtflPos<-PtflPos[["Equity"]]}

    #   Calculate sector, exposure, and Return for Equities
    PtflDtaOut<- PtflPos %>%
        mutate(Date = date) %>%
        select(Date, Security, Position) %>%
        group_by(Date, Security) %>%
        summarise(Position = sum(Position), .groups = "drop")

    return(PtflDtaOut)
}


#' @title Bloomberg ETF Holdings Download using DDE
#' @description Download ETF HLDR Page usign DDE
#' @param ETF character column name of ETF
#' @return data
#' @import tidyverse
#' @import tcltk2
#' @import Rblpapi
#' @import clipr
#' @export
bb_download_etf_holdings <- function(ETF = "ARKK US Equity"){

    #   Remove 'Equity'
    ETF <-gsub(" Equity", "", ETF, ignore.case = T)

    #   Go to HLDR Page for Fund
    tk2dde(topic = "bbk")
    tk2dde.exec("winblp",
                "bbk",
                paste("{<BLP-1>",
                      ETF,
                      "<EQUITY> HLDR <GO>}"))


    #   Check to see if loaded
    Sys.sleep(.5)
    loadflg<-0
    while(loadflg == 0){

        clear_clip()
        tk2dde.exec("winblp", "bbk", "{<BLP-1><COPY>}")
        x<-read_clip()
        while(length(x)==1){
            Sys.sleep(.5)
            x<-read_clip()}

        loadflg <- ifelse(length(grep("Loading",x))==0, 1, 0)
    }



    #   Cycle through pages of holdings until same
    Out<-NULL
    cycleflg<-0
    while(cycleflg == 0){

        # Find Top to Cut
        start <- grep("1)", x)[1]
        x <-
            read_tsv(
                x,
                skip = (start - 1),
                skip_empty_rows = F,
                col_names = F) %>%
            rename(
                "Num" = 1,
                "Name" = 2,
                "Ticker" = 3,
                "Source" = 4,
                "Position" = 5,
                "PosChg" = 6,
                "PctOut" = 7,
                "PctNet" = 8,
                "CurrMV" = 9,
                "RptMV" = 10,
                "FilingDate" = 11)

        if(length(which(x$Ticker %in% x$Ticker))>1){
            Out<-rbind(Out,x)
            cycleflg<-1
        }else{
            Out<-rbind(Out,x)
        }

        #   Page Down and Copy
        clear_clip()
        tk2dde.exec("winblp", "bbk", "{<BLP-1><PAGEFWD>}")
        Sys.sleep(1)
        tk2dde.exec("winblp", "bbk", "{<BLP-1><COPY>}")
        x<-read_clip()
        while(length(x)==1){
            Sys.sleep(.5)
            x<-read_clip()
        }
    }

    Out <- distinct(Out)
    Out$Date <- Sys.Date()
    Out$FundIf <- ETF
    return(Out)
}
