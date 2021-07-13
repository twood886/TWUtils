#' @title Calculate Quantiles
#' @description Function to calculate quantiles.
#' @details Used in Alpha Testing Functions
#' @param x vector to quantiled
#' @param fftile integer number of fractiles to use in spliting data
#' @return retval
#' @import tidyverse
#' @import DescTools
#' @export
ctq<-function(x, fftile){
    b<-sum(!is.na(unique(x)))
    labels<-gettextf("Q%s", 1:fftile)

    if(b>=fftile){
        qs<-round(rank(x, na.last = "keep")/sum(!is.na(x))/(1/fftile)+.4999)
        qs<-ifelse(qs<1,1,qs)
        retval<-cut_interval(qs,n = fftile,labels = labels)
    }else{
        retval<-factor(
            rep(NA, times = length(x)),
            levels = labels)
    }
    return(retval)
}


#' @title Quantile & Z-Scoring
#' @description Add Windsorized Z-Score and Quantile Score to Data
#' @details Used in Alpha Testing Functions
#' @param data dataframe containing column with data to be scored
#' @param fname character column name of factor
#' @param fftile integer number of fractiles to use in spliting data
#' @return data
#' @import tidyverse
#' @import DescTools
#' @export
f_scoring<-function(data, fname, fftile, standardize = T){

    if(standardize == T){
        data<-data %>%
            mutate(fzscore = scale(Winsorize(.data[[fname]], probs = c(0.02, 0.98), na.rm = T))) %>%
            mutate(fgroup = ctq(fzscore,!!fftile))
    }else{
        data <- data %>%
            mutate(`fzscore` = .data[[fname]]) %>%
            mutate(fgroup = ctq(fzscore, !!fftile))
    }


    return(data)
}


#' @title Alpha Testing -
#' Single Return Horizon, Single Factor, Single Period
#' @description This is the most baseline AT function.
#' @details This function returns for the universe and each quintile:
#'      number of observations \cr
#'      average return \cr
#'      average relative return for quintiles \cr
#'      security level hit rate of universe vs 0 \cr
#'      security level hit rate of universe vs universe average \cr
#'      information coefficient based on Z-Score of factor \cr
#'      information coefficient based on rank of factor \cr
#'      quintile spread between top and bottom quintile
#' @param data dataframe containing return column and factor value column
#' @param fname character column name of factor
#' @param rname character column name of return
#' @param fftile integer number of fractiles to use in spliting data
#' @return None
#' @import tidyverse
#' @import lubridate
#' @import magrittr
#' @import DescTools
#' @import reshape2
#' @export
AT_sr_sf_sp<-function(
    data,
    fname,
    rname){

    # Select return and factor columns
    data2 <- data %>%
       select({{ rname }} , {{fname}},  "fzscore", "fgroup")

    # Universe level statistics
    u_data<-data %>%
        summarize(
            u_n = n(),
            u_avg_return = mean(.data[[rname]], na.rm = T),
            u_hit_rate_z = sum(ifelse(.data[[rname]]>0,1,0), na.rm = T)/u_n,
            u_hit_rate_u = sum(ifelse(.data[[rname]]>u_avg_return,1,0), na.rm = T)/u_n,
            u_ic_score = cor(fzscore, scale(.data[[rname]]), use = "pairwise.complete.obs")[1],
            u_ic_rank = cor(rank(fzscore), rank(.data[[rname]]), use = "pairwise.complete.obs"))


    # Quantile level statistics
    q_data<-data %>%
        group_by(quintile = forcats::fct_explicit_na(fgroup, na_level = "na"),.drop = FALSE) %>%
        summarize(
            n = n(),
            avg_return = mean(.data[[rname]], na.rm = T),
            avg_return_rel = avg_return - !!u_data$u_avg_return,
            med_return = median(.data[[rname]]),
            hit_rate_z = sum(ifelse(.data[[rname]]>0,1,0))/n(),
            hit_rate_u = sum(ifelse(.data[[rname]]>!!u_data$u_avg_return,1,0))/n(),
            .groups = "drop")
    #u_ic_score = cor(fzscore, .data[[rname]], use = "pairwise.complete.obs"),
    #u_ic_rank = cor(rank(fzscore), rank(.data[[rname]]), use = "pairwise.complete.obs"))

    # if(length(which(q_data$quintile == "NA")) == 0){
    #     q_data
    # }

    q_spread<-q_data$avg_return[max(which(q_data$quintile!="na"))] -
        q_data$avg_return[min(which(q_data$quintile!="na"))]

    # Add Q spead to universe data
    u_data<-add_column(u_data, q_spread =  q_spread)

    # Flattent quintile Data
    q_df<-dcast(
        melt(as.data.frame(q_data),id.var="quintile"),
        1~variable+quintile)

    q_df<-q_df[,-1]

    # Reression based factor return
    beta<-tryCatch(
        {
            reg <- lm(
                return ~ predictor,
                data = data %>%
                    rename(
                        `return` = all_of(rname),
                        `predictor` = all_of(fname)) %>%
                    select(c(`return`, `predictor`)))

            beta <- reg$coefficients[[2]]
        },
        error = function(cond){
            return(NA)
        })

    # Bind universe data with quantile data
    tempret <- cbind(
        as.data.frame(u_data),
        q_df,
        beta = beta)

    return(tempret)
}

#' @title Alpha Testing -
#' Single Return Horizon, Single Factor, Multiple Periods
#' @description Runs AT_sr_sf_sp for multiple periods
#' @details This function returns for the universe and each quintile for each period: \cr
#'       number of observations\cr
#'       average return\cr
#'       average relative return for quantiles\cr
#'       security level hit rate of universe vs 0\cr
#'       security level hit rate of universe vs universe average\cr
#'       information coefficient based on Z-Score of factor\cr
#'       information coefficient based on rank of factor\cr
#'       quintile spread between top and bottom quintile\cr
#' @param data dataframe containing return column and factor value column
#' @param fname character column name of factor
#' @param rname character column name of return
#' @param fftile integer number of fractiles to use in spliting data
#' @import tidyverse
#' @import lubridate
#' @import magrittr
#' @import DescTools
#' @import reshape2
#' @export
AT_sr_sf_mp<-function(
    data = NULL,          # Score Data and returns
    fname = NULL,         # Factor for alpha testing
    rname = NULL,         # Return column names
    fftile = 5            # Number of fractiles
    ){

    # Split Data into time periods
    ptm <- proc.time()
    calcdata<-data %>%
        group_by(as.Date(Date, format = "%m/%d/%Y")) %>%
        group_split


    names(calcdata)<-plyr::laply(
        calcdata,
        function(x) paste(x$Date[1]))

    # Calculate periodic summary statistics
    p_stats_v2<-lapply(
        calcdata,
        AT_sr_sf_sp,
        #.progress = "text",
        #.id = "date",
        fname = fname,
        rname = rname,
        fftile = fftile)



    p_stats <- data %>%
        split(.$Date) %>%
        map(~ AT_sr_sf_sp(data = ., fname = fname, rname = rname, fftile = fftile))


    #avg_ic_score<-mean(p_stats$u_ic_score, na.rm=T)
    #avg_ic_rank<-mean(p_stats$u_ic_rank, na.rm =T)
    #avg_q_spread<-mean(p_stats$q_spread, na.rm=T)

    # Chart relative return of quintiles
    #q_return_m<-melt(
    #    calcdata,
    #    id.vars = date,
    #    measure.vars= c(""))


    return(p_stats)
}


#' @title Alpha Testing -
#' Multipe Return Horizon, Single Factor, Multiple Periods
#' @description Runs AT_sr_sf_mp for multiple return horizons
#' @details UPDATED - This function returns for the universe and each quintile
#'  for each period and for each return horizon: \cr
#'       number of observations\cr
#'       average return\cr
#'       average relative return for quintiles\cr
#'       security level hit rate of universe vs 0\cr
#'       security level hit rate of universe vs universe average\cr
#'       information coefficient based on Z-Score of factor\cr
#'       information coefficient based on rank of factor\cr
#'       quintile spread between top and bottom quintile\cr
#' @param data dataframe containing return column and factor value column
#' @param return_cols vector of column names of returns
#' @param fname character column name of factor
#' @param fftile integer number of fractiles to use in spliting data
#' @import tidyverse
#' @import lubridate
#' @import magrittr
#' @import DescTools
#' @import reshape2
#' @export
AT_mr_sf_mp<-function(
    data = NULL,
    return_cols = NULL,
    fname = NULL,
    fftile = 5,
    standardize = T){



    #   Run AT_sr_sf_mp for each return horizon
    #       sr = single return
    #       sf = single factor
    #       mp = multiple periods
    #   Returns a list of data frames. Each data frame corresponding to

    print(fname)

    # # old Verison
    # ptm <- proc.time()
    # AT_list<-sapply(
    #     return_cols,
    #     AT_sr_sf_mp,
    #     data = data,
    #     fname = fname,
    #     fftile = fftile,
    #     simplify = F)
    # proc.time() - ptm
    # orig<-ptm<-proc.time()
    #
    # IC_Decay_Rank<-sapply(AT_list, function(x) mean(x$u_ic_score, na.rm=T))
    # IC_Decay_Score<-sapply(AT_list, function(x) mean(x$u_ic_score, na.rm=T))
    # Q_Spread<-sapply(AT_list, function(x) mean(x$q_spread, na.rm = T))
    #
    #
    # return(
    #     list(
    #         IC_Decay_Rank = IC_Decay_Rank,
    #         IC_Decay_Score = IC_Decay_Score,
    #         Q_Spread = Q_Spread,
    #         AT = AT_list
    #     )
    # )


    dataset <- data %>%
        group_by(`Date`) %>%
        nest()  %>%
        transmute(f_scoring = map(data, f_scoring, fname = {{fname}}, fftile = {{fftile}}, standardize)) %>%
        unnest(f_scoring) %>%
        ungroup()


    AT_Stats <- dataset %>%
        pivot_longer(
            cols = return_cols,
            names_to = "returnPeriod",
            values_to = "return") %>%
        group_by(`returnPeriod`, `Date`) %>%
        nest() %>%
        transmute(periodAT = map(data, AT_sr_sf_sp, fname = {{fname}}, rname = "return")) %>%
        unnest(cols = c(periodAT)) %>%
        group_by(`returnPeriod`) %>%
        arrange(`Date`) %>%
        ungroup() %>%
        group_by(`Date`)  %>%
        mutate(`returnPeriod`= factor(returnPeriod, ordered = T, levels = {{return_cols}})) %>%
        ungroup()

    AT_Summary <- AT_Stats %>%
        group_by(`Date`) %>%
        arrange(`Date`, `returnPeriod`) %>%
        mutate(
            u_ic_score_increment = u_ic_score - lag(u_ic_score, default = 0)) %>%
        group_by(`returnPeriod`) %>%
        summarise(
            avgICRank = mean(u_ic_rank, na.rm = T),
            avgICScore = mean(u_ic_score, na.rm = T),
            avgQSpread = mean(q_spread, na.rm = T),
            avgICScoreIncrement = mean(u_ic_score_increment, na.rm = T))

    output <- list(
        dataset = dataset,
        periodStats = AT_Stats,
        summaryStats = AT_Summary)

    return(output)
}
