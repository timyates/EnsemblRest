\name{EnsAssemblyInfo}
\alias{EnsAssemblyInfo}
\alias{EnsAssemblyInfo-class}
\alias{\S4method{new}{EnsAssemblyInfo}}
\alias{\S4method{show}{EnsAssemblyInfo}}
\title{
  The Assembly Info class
}
\description{
  The class containing a Assembly Information
}
\usage{
  \S4method{new}{EnsAssemblyInfo}( x=NULL, ... )
  \S4method{show}{EnsAssemblyInfo}()
}
\arguments{
  \item{x}{ The \code{List} returned by a call to the rjson wrapper }
  \item{...}{ Other params }
}
\details{
  A class \code{EnsAssemblyInfo}, which contains the fields:
  \tabular{ll}{
    assembly_name \tab \code{character} \cr
    top_level_seq_region_names \tab \code{character} \cr
    assembly_date \tab \code{character} \cr
    coord_system_versions \tab \code{\link{character}} \cr
    genebuild_start_date \tab \code{character} \cr
    genebuild_initial_release_date \tab \code{character} \cr
    schema_build \tab \code{character} \cr
    genebuild_last_geneset_update \tab \code{\link{character}} \cr
    genebuild_method \tab \code{character}
  }
}
\seealso{
  \code{\link{infoAssembly}}\cr
}