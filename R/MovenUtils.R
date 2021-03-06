require(dplyr)
require(stringr)
require(lubridate)
require(ggplot2)
require(zoo)
require(fpc)

#' Check if the date is the Last doy of the month
#'
#' @param date the date in date type.
#' @return Boolean vectors indicating if the input is the last day of the month or not
#' @export
is_end_of_month <- function(date){
    return(mday(date + days(1)) == 1)
}

#' Check if the date is the First doy of the month
#'
#' @param date the date in date type.
#' @return Boolean vectors indicating if the input is the first day of the month or not
#' @export
is_start_of_month <- function(date){
    return(mday(date) == 1)
}

#' Get number of days in the month of given date
#'
#' @param date the date in date type.
#' @return numeric value indicating number of days
numberOfDays <- function(date) {
    m <- format(date, format="%m")
    
    while (format(date, format="%m") == m) {
        date <- date + 1
    }
    
    return(as.integer(format(date - 1, format="%d")))
}

#' Vectorized version to get number of days in the month of given date
#'
#' @param date_vec the date vector in date type.
#' @return numeric vector indicating number of days in the month
#' @export
number_of_days <- function(date_vec) {
    sapply(date_vec, numberOfDays)
}



#' Check if the transaction is a valid paycheck 
#'
#' @param income numeric vector of amount.
#' @param lo_prob lower quantile
#' @param up_prob upper quantile
#' @param min_income minimum value to be considered as a paycheck
#' @return Boolean vectors indicating if the input is a valid paycheck 
#' @export
pay_check_tagger <- function(income, lo_prob=0.3, up_prob=0.9, min_income = 100){
    lower_bound <- max(quantile(income, probs = lo_prob), min_income)
    upper_bound <- quantile(income, probs = up_prob)
    return(ifelse(income >= lower_bound & income <= upper_bound, TRUE, FALSE))
}

#' Calculate euclidean distance to the centeroid
#'
#' @param x1 data matrix, each row is one data record each column is one dimision.
#' @return average euclidean distance with respect to the center point of the data points.
#' @export
eud.dist_center <- function(x1){
    ctr_pt <- colMeans(x1)
    sum(sqrt(rowSums((x1 - ctr_pt)^2))) / nrow(x1)
}


#' Check the transaction similarity of a user
#'
#' @param amount numeric vector of amount.
#' @param date vector of date type.
#' @param trans_class category identifier of the transaction.
#' @param margin the tolerable marginal difference to consider 2 transactions are the same.
#' @param pct_error percentile of average transaction amount as the margin.
#' @param time_lo minimum difference between 2 transaction
#' @param time_up maximum difference between 2 transaction
#' @return numeric vectors indicating group id for similar transactions
#' @export
sim_trans_tag <- function(amount, date, trans_class, margin=0.011, pct_error=NULL, time_lo = 5, time_up = 40){
    # create variable margin
    
    if(!is.null(pct_error)){ 
        margin <- pct_error * amount
    }
    trans_data <- data.frame(amount = as.numeric(amount), 
                             date = as.numeric(date), 
                             margin = margin, 
                             trans_class = trans_class)
    # loop through each row and return index of rows that matches criteria
    primary_cluster <- lapply(1:nrow(trans_data), function(i){
        idx <- which((abs(trans_data[i,'amount'] - trans_data[,'amount']) <= trans_data[i,'margin']) & 
                         (abs(trans_data[i,'date'] - trans_data[,'date']) <= time_up) & 
                         (abs(trans_data[i,'date'] - trans_data[,'date']) >= time_lo) & 
                         (trans_data[i,'trans_class'] == trans_data[,'trans_class']))
        sort(c(i,idx))
        
    })
    # helper function to gather collection of idx belongs to same group
    idx_puller <- function(my_collection){
        new_collection <- sort(unique(c(unlist(primary_cluster[my_collection]), my_collection)))
        if(length(new_collection)==length(my_collection) &&
               all(new_collection == my_collection)){
            return(new_collection)
        }else{
            idx_puller(new_collection)
        }
    }
    
    # return min idx in group as the grouping id return 0 if only 1 element in the group
    my_groups <- lapply(1:length(primary_cluster), function(i){
        collection <- sort(unique(unlist(primary_cluster[i])))
        if (length(collection) == 1){
            return(0)
        }else{
            return(min(idx_puller(collection)))
        }
        
    })
    return(unlist(my_groups))
}


#' Check the transaction similarity of a user
#'
#' @param col1 character column contain string to match.
#' @param col2 character column contain string to match.
#' @return A function consums a string and returns Boolean vector indicating
#'  if there is a match in the input columns or not.
#' @export
contain_string <- function(col1, col2){
    function(string){
        grepl(string, col1, ignore.case = T) | 
            grepl(string, col2, ignore.case = T)
    }
}

#' Check the transaction similarity of a user
#'
#' @param col1 character column contain string to match.
#' @param col2 character column contain string to match.
#' @return A function consums a list of strings and returns a collection of strings in the list 
#' that matches the content in either input columns.
#' @export
get_name_tag <- function(col1, col2){
    function(string_list){
        exst_str_mtx<- sapply(string_list, function(string){
            ifelse(grepl(string, col1, ignore.case = T) | 
                       grepl(string, col2, ignore.case = T),
                   gsub(pattern = "^\\s*|\\s*$", replacement = "",string),
                   "")
        })    
        out_tags <- apply(exst_str_mtx, MARGIN = 1, function(row){
            non_blank_idx <- which(row != "")
            paste(row[non_blank_idx], collapse = ", ")
        })
        return(out_tags)
    }
}


#' Check if the date is a US holiday
#'
#' @param date date object.
#' @return boolean vector/ value indicating if the input date is a holiday or not 
#' @export
check_holiday <- function(date){
    library(timeDate)
    # create year interger vector
    my_year <- as.integer(unique(year(date)))
    # holiday constant
    HOLIDAY_LIST <- c("USDecorationMemorialDay", "USPresidentsDay", "USNewYearsDay", 
                      "USInaugurationDay", "USMLKingsBirthday", "USLincolnsBirthday", 
                      "USWashingtonsBirthday", "USMemorialDay", "USIndependenceDay", 
                      "USLaborDay", "USColumbusDay", "USElectionDay", "USVeteransDay",
                      "USThanksgivingDay", "USChristmasDay", "USCPulaskisBirthday", "USGoodFriday")
    # find holidays in the corresponding year
    my_holidays  <- as.Date(as.character(holiday(my_year, HOLIDAY_LIST)),"%Y-%m-%d")
    # boolean indicator for holiday tagging
    return(as.Date(date) %in%  my_holidays)   
}



#' Calculate days before the closest holiday
#'
#' @param date date object.
#' @return boolean vector indicating how many days before the closest holiday the current date is.
#' @export
days_before_holiday <- function(date){
    library(timeDate)
    # holiday constant
    HOLIDAY_LIST <- c("USDecorationMemorialDay", "USPresidentsDay", "USNewYearsDay", 
                      "USInaugurationDay", "USMLKingsBirthday", "USLincolnsBirthday", 
                      "USWashingtonsBirthday", "USMemorialDay", "USIndependenceDay", 
                      "USLaborDay", "USColumbusDay", "USElectionDay", "USVeteransDay",
                      "USThanksgivingDay", "USChristmasDay", "USCPulaskisBirthday", "USGoodFriday")
    # create year interger vector
    date <- income_data$date
    my_year <- as.integer(unique(year(date)))
    my_holidays  <- as.Date(as.character(holiday(my_year, HOLIDAY_LIST)),"%Y-%m-%d")
    
    sapply(date, function(d){
        diff_b_hday <- my_holidays - d
        as.integer(min(diff_b_hday[diff_b_hday >= 0] )) 
    })    
}