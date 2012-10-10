\name{EnsRef}
\alias{EnsRef}
\alias{EnsRef-class}
\alias{\S4method{new}{EnsRef}}
\alias{\S4method{show}{EnsRef}}
\title{
  The Ref class
}
\description{
  The class containing a Ref
}
\usage{
  \S4method{new}{EnsRef}( x=NULL, ... )
  \S4method{show}{EnsRef}( prefix='' )
}
\arguments{
  \item{x}{ The \code{List} returned by a call to the rjson wrapper }
  \item{prefix}{ the prefix to prepend to all output. }
  \item{...}{ Other params }
}
\details{
  A class \code{EnsRef}, which contains the fields:
  \tabular{ll}{
    protein_features \tab \code{character} \cr
    alleles \tab \code{data.frame} \cr
    data \tab \code{data.frame}
  }
}
