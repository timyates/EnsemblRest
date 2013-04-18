\name{ontology}
\alias{EnsemblRest-ontology}
\alias{ontologyId}
\alias{ontologyAncestors}
\alias{ontologyAncestorsChart}
\title{
  Ontology Methods
}
\description{
  Calls the Ontology methods of the Ensembl REST API.

  Online Documentation:
  \tabular{ll}{
    ontologyId: \tab \url{http://beta.rest.ensembl.org/documentation/info/ontology_id} \cr
    ontologyAncestors: \tab \url{http://beta.rest.ensembl.org/documentation/info/ontology_ancestors} \cr
    ontologyAncestorsChart: \tab \url{http://beta.rest.ensembl.org/documentation/info/ontology_ancestors_chart}
  }
}
\usage{
  ontologyId( id, relation=NULL, simple=FALSE )
  ontologyAncestors( id, ontology=NULL )
  ontologyAncestorsChart( id, ontology=NULL )

}
\arguments{
  \item{id}{ The identifier of the term to search for }
  \item{relation}{ The types of relations to retriving both child and parent terms associcated with this term. Fetches all relations by default }
  \item{simple}{ If \code{TRUE} the API will avoid the fetching of parent and child terms }
  \item{ontology}{ The ontology the relations belong to. Used to dis-ambiguate terms which are shared between ontologies such as GO and EFO, eg GO }
}
\details{
  Returns a \code{\link{EnsOntology}} object.
}
\examples{
  ontologyId( 'GO:0005667' )
  ontologyAncestors( 'GO:0005667' )
  ontologyAncestorsChart( 'GO:0005667' )
}
