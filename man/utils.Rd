\name{utils}
\alias{getParam}
\alias{setParam}
\title{
  Utility functions
}
\description{
  Functions to access internal parameters
}
\usage{
  getParam( key )
  setParam( ... )
}
\arguments{
  \item{ ... }{ A list of key-value parameters you wish to set. }
  \item{key}{ The key for the value you want to return. }
}
\details{
  These functions allow some access to internal configuration data. They are included to help debug issues, and are not normally needed.
}
\author{
  Tim Yates
}
\examples{
  getParam( "debug" ) # FALSE
  setParam( debug=TRUE )
  getParam( "debug" ) # TRUE
}
