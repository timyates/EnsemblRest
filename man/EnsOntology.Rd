\name{EnsOntology}
\alias{EnsOntology}
\alias{EnsOntology-class}
\alias{\S4method{new}{EnsOntology}}
\alias{\S4method{show}{EnsOntology}}
\title{
  The Ontology class
}
\description{
  The class containing an Ontology
}
\usage{
  \S4method{new}{EnsOntology}( x=NULL, ... )
  \S4method{show}{EnsOntology}()
}
\arguments{
  \item{x}{ The \code{List} returned by a call to the rjson wrapper }
  \item{...}{ Other params }
}
\details{
  A class \code{EnsOntology}, which contains the fields:
  \tabular{ll}{
    ontology \tab \code{character} \cr
    accession \tab \code{character} \cr
    definition \tab \code{character} \cr
    name \tab \code{character} \cr
    namespace \tab \code{character} \cr
    subsets \tab \code{character} \cr
    synonyms \tab \code{character} \cr
    children \tab \code{list} \cr
    parents \tab \code{list}
  }
}
