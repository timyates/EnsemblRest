\name{ontology}
\alias{EnsemblRest-ontology}
\alias{ontologyId}
\title{
  Ontology Methods
}
\description{
  Calls the Ontology methods of the Ensembl REST API.

  Online Documentation:
  \tabular{ll}{
    ontologyId: \tab \url{http://beta.rest.ensembl.org/documentation/info/ontology_id}
  }
}
\usage{
  ontologyId( id, relation=NULL, simple=FALSE )
}
\arguments{
  \item{id}{ The identifier of the term to search for }
  \item{relation}{ The types of relations to retriving both child and parent terms associcated with this term. Fetches all relations by default }
  \item{simple}{ If \code{TRUE} the API will avoid the fetching of parent and child terms }
}
\details{
  Returns a \code{\link{EnsOntology}} object.
}
\examples{
  ontologyId( 'GO:0005667' )
}
