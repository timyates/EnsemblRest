\name{EnsTranscript}
\alias{EnsTranscript}
\alias{EnsTranscript-class}
\alias{\S4method{new}{EnsTranscript}}
\alias{\S4method{show}{EnsTranscript}}
\title{
  The Transcript class
}
\description{
  The class containing a Transcript
}
\usage{
  \S4method{new}{EnsTranscript}( x=NULL, ... )
  \S4method{show}{EnsTranscript}()
}
\arguments{
  \item{x}{ The \code{List} returned by a call to the rjson wrapper }
  \item{...}{ Other params }
}
\details{
  A class \code{EnsTranscript}, which contains the fields:
  \tabular{ll}{
    data \tab \code{data.frame} \cr
    alleles \tab \code{data.frame} \cr
    protein_features \tab \code{data.frame}
  }
}
\seealso{
  \code{\link{variationAllele}}\cr
  \code{\link{variationId}}\cr
  \code{\link{EnsVariantConsequence}}\cr
}