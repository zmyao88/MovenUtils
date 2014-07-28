\name{similar_transaction_tagger}
\alias{similar_transaction_tagger}
\title{Check the transaction similarity of a user}
\usage{
similar_transaction_tagger(amount, date, margin = 0.011, pct_error = NULL,
  time_lo = 5, time_up = 40)
}
\arguments{
  \item{amount}{numeric vector of amount.}

  \item{date}{vector of date type.}

  \item{margin}{the tolerable marginal difference to
  consider 2 transactions are the same.}

  \item{pct_error}{percentile of average transaction amount
  as the margin.}

  \item{time_lo}{minimum difference between 2 transaction}

  \item{time_up}{maximum difference between 2 transaction}
}
\value{
Boolean vectors indicating if the input has a similar
transaction
}
\description{
Check the transaction similarity of a user
}

