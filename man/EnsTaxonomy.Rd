\name{EnsTaxonomy}
\alias{EnsTaxonomy}
\alias{EnsTaxonomy-class}
\alias{\S4method{new}{EnsTaxonomy}}
\alias{\S4method{show}{EnsTaxonomy}}
\title{
  The Taxonomy class
}
\description{
  The class containing an Taxonomy
}
\usage{
  \S4method{new}{EnsTaxonomy}( x=NULL, ... )
  \S4method{show}{EnsTaxonomy}()
}
\arguments{
  \item{x}{ The \code{List} returned by a call to the rjson wrapper }
  \item{...}{ Other params }
}
\details{
  A class \code{EnsTaxonomy}, which contains the fields:
  \tabular{ll}{
    ontology \tab \code{character} \cr
    id \tab \code{character} \cr
    name \tab \code{character} \cr
    scientific_name \tab \code{character} \cr
    leaf \tab \code{logical} \cr
    tags \tab \code{list} \cr
    parent \tab \code{ANY} # Will be EnsTaxonomy or NULL \cr
    children \tab \code{list}
  }
}
