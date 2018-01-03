\name{gagen}
\alias{gagen}
\title{Gamma Distribution}
\usage{
gagen(l,k)
}
\description{
This function generates a random number from Gamma Distribution i.e. sum of k iid Exponential random numbers with parameter l.
}
\arguments{
  \item{l}{parameter for inner Exponential distribution}
  \item{k}{number of Exponential number}
}
\references{
  dr.gharib and mr.heidari
}
\examples{
gagen(4,6)
}
