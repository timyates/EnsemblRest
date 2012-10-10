\name{EnsHomology}
\alias{EnsHomology}
\alias{EnsHomology-class}
\alias{\S4method{new}{EnsHomology}}
\alias{\S4method{show}{EnsHomology}}
\title{
  The Homology class
}
\description{
  The class containing a Homology
}
\usage{
  \S4method{new}{EnsHomology}( x=NULL, ... )
  \S4method{show}{EnsHomology}( prefix='' )
}
\arguments{
  \item{x}{ The \code{List} returned by a call to the rjson wrapper }
  \item{prefix}{ the prefix to prepend to all output. }
  \item{...}{ Other params }
}
\details{
  A class \code{EnsHomology}, which contains the fields:
  \tabular{ll}{
    dn_ds \tab \code{numeric} \cr
    type \tab \code{character} \cr
    subtype \tab \code{character} \cr
    source \tab \code{\link{EnsHomologyDetail}} \cr
    target \tab \code{\link{EnsHomologyDetail}}
  }
}
\seealso{
  \code{\link{EnsHomologyDetail-class}}\cr
}