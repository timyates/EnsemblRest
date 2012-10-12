\name{EnsVariantConsequence}
\alias{EnsVariantConsequence}
\alias{EnsVariantConsequence-class}
\alias{\S4method{new}{EnsVariantConsequence}}
\alias{\S4method{show}{EnsVariantConsequence}}
\title{
  The Variant Consequence class
}
\description{
  The class containing the response from variation calls
}
\usage{
  \S4method{new}{EnsVariantConsequence}( x=NULL, ... )
  \S4method{show}{EnsVariantConsequence}()
}
\arguments{
  \item{x}{ The \code{List} returned by a call to the rjson wrapper }
  \item{...}{ Other params }
}
\details{
  A class \code{EnsVariantConsequence}, which contains the fields:
  \tabular{ll}{
    is_somatic \tab \code{logical} \cr
    location \tab \code{GRanges} \cr
    name \tab \code{character} \cr
    hgvs \tab \code{character} \cr
    transcripts \tab \code{list} of \code{\link{EnsTranscript}}
  }
}
\seealso{
  \code{\link{variationAllele}}\cr
  \code{\link{variationId}}
}