\name{EnsTranscript}
\alias{EnsTranscript}
\alias{EnsTranscript-class}
\alias{\S4method{new}{EnsTranscript}}
\alias{\S4method{show}{EnsTranscript}}
\title{
  The Assembly Details class
}
\description{
  The class containing a Species
}
\usage{
  \S4method{new}{EnsTranscript}( x=NULL, ... )
  \S4method{show}{EnsTranscript}( prefix='' )
}
\arguments{
  \item{x}{ The \code{List} returned by a call to the rjson wrapper }
  \item{prefix}{ the prefix to prepend to all output. }
  \item{...}{ Other params }
}
\details{
  A class \code{EnsTranscript}, which contains the fields:
  \tabular{ll}{
    original \tab \code{GRanges} \cr
    mapped \tab \code{GRanges}
  }
}
\seealso{
  \code{\link{mapping}}\cr
}