\name{sim_trans_tag}
\alias{sim_trans_tag}
\title{Check the transaction similarity of a user}
\usage{
sim_trans_tag(amount, date, trans_class, margin = 0.011, pct_error = NULL,
  time_lo = 5, time_up = 40)
}
\arguments{
  \item{amount}{numeric vector of amount.}

  \item{date}{vector of date type.}

  \item{trans_class}{category identifier of the
  transaction.}

  \item{margin}{the tolerable marginal difference to
  consider 2 transactions are the same.}

  \item{pct_error}{percentile of average transaction amount
  as the margin.}

  \item{time_lo}{minimum difference between 2 transaction}

  \item{time_up}{maximum difference between 2 transaction}
}
\value{
numeric vectors indicating group id for similar
transactions
}
\description{
Check the transaction similarity of a user
}

