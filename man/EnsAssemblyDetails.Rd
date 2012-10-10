\name{EnsAssemblyDetails}
\alias{EnsAssemblyDetails}
\alias{EnsAssemblyDetails-class}
\alias{\S4method{new}{EnsAssemblyDetails}}
\alias{\S4method{show}{EnsAssemblyDetails}}
\title{
  The Assembly Details class
}
\description{
  The class containing an Assembly Detail
}
\usage{
  \S4method{new}{EnsAssemblyDetails}( x=NULL, ... )
  \S4method{show}{EnsAssemblyDetails}( prefix='' )
}
\arguments{
  \item{x}{ The \code{List} returned by a call to the rjson wrapper }
  \item{prefix}{ the prefix to prepend to all output. }
  \item{...}{ Other params }
}
\details{
  A class \code{EnsHomology}, which contains the fields:
  \tabular{ll}{
    is_chromosome \tab \code{logical} \cr
    length \tab \code{numeric} \cr
    assembly_exception_type \tab \code{character} \cr
    coordinate_system \tab \code{\link{character}}
  }
}
\seealso{
  \code{\link{assemblyDetails}}\cr
}