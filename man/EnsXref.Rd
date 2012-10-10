\name{EnsXref}
\alias{EnsXref}
\alias{EnsXref-class}
\alias{\S4method{new}{EnsXref}}
\alias{\S4method{show}{EnsXref}}
\title{
  The Xref class
}
\description{
  The class containing an Xref
}
\usage{
  \S4method{new}{EnsXref}( x=NULL, ... )
  \S4method{show}{EnsXref}( prefix='' )
}
\arguments{
  \item{x}{ The \code{List} returned by a call to the rjson wrapper }
  \item{prefix}{ the prefix to prepend to all output. }
  \item{...}{ Other params }
}
\details{
  A class \code{EnsXref}, which contains the fields:
  \tabular{ll}{
    display_id \tab \code{character} \cr
    primary_id \tab \code{character} \cr
    version \tab \code{character} \cr
    description \tab \code{character} \cr
    dbname \tab \code{character} \cr
    synonyms \tab \code{character} \cr
    info_type \tab \code{character} \cr
    info_text \tab \code{character} \cr
    db_display_name \tab \code{character}
  }
}
