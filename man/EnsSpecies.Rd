\name{EnsSpecies}
\alias{EnsSpecies}
\alias{EnsSpecies-class}
\alias{\S4method{new}{EnsSpecies}}
\alias{\S4method{show}{EnsSpecies}}
\title{
  The Assembly Details class
}
\description{
  The class containing a Species
}
\usage{
  \S4method{new}{EnsSpecies}( x=NULL, ... )
  \S4method{show}{EnsSpecies}( prefix='' )
}
\arguments{
  \item{x}{ The \code{List} returned by a call to the rjson wrapper }
  \item{prefix}{ the prefix to prepend to all output. }
  \item{...}{ Other params }
}
\details{
  A class \code{EnsSpecies}, which contains the fields:
  \tabular{ll}{
    name \tab \code{character} \cr
    aliases \tab \code{character} \cr
    groups \tab \code{character} \cr
    release \tab \code{\link{character}}
  }
}
