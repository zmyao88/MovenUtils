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
#' @param margin the tolerable marginal difference to consider 2 transactions are the same.
#' @param pct_error percentile of average transaction amount as the margin.
#' @param time_lo minimum difference between 2 transaction
#' @param time_up maximum difference between 2 transaction
#' @return Boolean vectors indicating if the input has a similar transaction
#' @export
similar_transaction_tagger <- function(amount, date, margin=0.011, pct_error=NULL, time_lo = 5, time_up = 40){
    # create variable margin
    if(!is.null(pct_error)){ 
        margin <- pct_error * amount
    }
    trans_data <- cbind(amount, date, margin)
    # loop through each row and return index of rows that matches criteria
    apply(trans_data, MARGIN = 1, function(data){
        any((abs(data['amount'] - trans_data[,'amount']) <= data['margin']) & 
            (abs(data['date'] - trans_data[,'date']) <= time_up) & 
            (abs(data['date'] - trans_data[,'date']) >= time_lo))
        
        
    })    
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