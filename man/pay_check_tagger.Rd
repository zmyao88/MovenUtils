\name{pay_check_tagger}
\alias{pay_check_tagger}
\title{Check if the transaction is a valid paycheck}
\usage{
pay_check_tagger(income, lo_prob = 0.3, up_prob = 0.9, min_income = 100)
}
\arguments{
  \item{income}{numeric vector of amount.}

  \item{lo_prob}{lower quantile}

  \item{up_prob}{upper quantile}

  \item{min_income}{minimum value to be considered as a
  paycheck}
}
\value{
Boolean vectors indicating if the input is a valid paycheck
}
\description{
Check if the transaction is a valid paycheck
}

