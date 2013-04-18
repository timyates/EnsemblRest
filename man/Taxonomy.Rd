\name{taxonomy}
\alias{EnsemblRest-taxonomy}
\alias{taxonomyId}
\title{
  Taxonomy Methods
}
\description{
  Calls the Taxonomy methods of the Ensembl REST API.

  Online Documentation:
  \tabular{ll}{
    ontologyId: \tab \url{http://beta.rest.ensembl.org/documentation/info/taxonomy_id}
  }
}
\usage{
  taxonomyId( id, simple=FALSE )
}
\arguments{
  \item{id}{ The identifier of the term to search for. Can be a NCBI taxon id or a name, eg \'9606\' or \'Homo sapiens\' }
  \item{simple}{ If \code{TRUE} the API will avoid the fetching of parent and child terms }
}
\details{
  Returns a \code{\link{EnsOntology}} object.
}
\examples{
  taxonomyId( '9606' )
}
