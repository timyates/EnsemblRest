\name{EnsHomologyResponse}
\alias{EnsHomologyResponse}
\alias{EnsHomologyResponse-class}
\alias{\S4method{new}{EnsHomologyResponse}}
\alias{\S4method{show}{EnsHomologyResponse}}
\title{
  The Homology Response class
}
\description{
  The class returned by the methods \code{homologyById} and \code{homologyBySymbol}
}
\usage{
  \S4method{new}{EnsHomologyResponse}( x=NULL, ... )
  \S4method{show}{EnsHomologyResponse}()
}
\arguments{
  \item{x}{ The \code{List} returned by a call to the rjson wrapper }
  \item{...}{ Other params }
}
\details{
  A class \code{EnsHomologyResponse}, which contains an id field, and a field \code{homologies} which is a List of \code{\link{EnsHomology}} objects.
}
\seealso{
  \code{\link{EnsHomology-class}}\cr
}