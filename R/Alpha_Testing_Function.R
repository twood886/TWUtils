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
f_scoring<-function(data, fname, fftile){
    data<-data %>%
        mutate(fzscore = scale(Winsorize(.data[[fname]], na.rm = T))) %>%
        mutate(fgroup = ctq(fzscore,!!fftile))
    return(data)
}


#' @title Alpha Testing - Single Return Horizon, Single Factor, Single Period
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
#' @examples
#' AT_sr_sf_sp()
#' @import tidyverse
#' @import lubridate
#' @import magrittr
#' @import DescTools
#' @import reshape2
#' @export
AT_sr_sf_sp<-function(
    data,
    fname,
    rname,
    fftile){

    # Select return and factor columns
    data <- data %>%
        select(all_of(c(rname, fname)))

    data<-f_scoring(
        data = data,
        fname = fname,
        fftile = fftile)

    # Universe level statistics
    u_data<-data %>%
        summarize(
            u_n = n(),
            u_avg_return = mean(.data[[rname]], na.rm = T),
            u_hit_rate_z = sum(ifelse(.data[[rname]]>0,1,0), na.rm = T)/u_n,
            u_hit_rate_u = sum(ifelse(.data[[rname]]>u_avg_return,1,0), na.rm = T)/u_n,
            u_ic_score = cor(fzscore, .data[[rname]], use = "pairwise.complete.obs"),
            u_ic_rank = cor(rank(fzscore), rank(.data[[rname]]), use = "pairwise.complete.obs"))


    # Quntile level statistics
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
    tempret<-cbind(
        as.data.frame(u_data),
        q_df,
        beta = beta)

    return(tempret)
}
