\name{ontology}
\alias{EnsemblRest-ontology}
\alias{ontologyId}
\alias{ontologyName}
\alias{ontologyAncestors}
\alias{ontologyAncestorsChart}
\alias{ontologyDescendents}
\title{
  Ontology Methods
}
\description{
  Calls the Ontology methods of the Ensembl REST API.

  Online Documentation:
  \tabular{ll}{
    ontologyId: \tab \url{http://beta.rest.ensembl.org/documentation/info/ontology_id} \cr
    ontologyName: \tab \url{http://beta.rest.ensembl.org/documentation/info/ontology_name} \cr
    ontologyAncestors: \tab \url{http://beta.rest.ensembl.org/documentation/info/ontology_ancestors} \cr
    ontologyAncestorsChart: \tab \url{http://beta.rest.ensembl.org/documentation/info/ontology_ancestors_chart} \cr
    ontologyDescendents: \tab \url{http://beta.rest.ensembl.org/documentation/info/ontology_descendents}
  }
}
\usage{
  ontologyId( id, relation=NULL, simple=FALSE )
  ontologyName( name, ontology=NULL, relation=NULL, simple=FALSE )
  ontologyAncestors( id, ontology=NULL )
  ontologyAncestorsChart( id, ontology=NULL )
  ontologyDescendents( id, closest_term=NULL, ontology=NULL, subset=NULL, zero_distance=NULL )
}
\arguments{
  \item{id}{ The name of a term to search for. SQL wildcards are supported, eg \'transcription factor complex\' }
  \item{relation}{ The types of relations to retriving both child and parent terms associcated with this term. Fetches all relations by default }
  \item{simple}{ If \code{TRUE} the API will avoid the fetching of parent and child terms }
  \item{ontology}{ The ontology the relations belong to. Used to dis-ambiguate terms which are shared between ontologies such as GO and EFO, eg GO }
  \item{name}{ The identifier of the term to search for }
  \item{closest_term}{ If true return only the closest terms to the specified term. }
  \item{subset}{ Specify a subset term to restrict terms by, eg: goslim_generic or goslim_metagenomics }
  \item{zero_distance}{ Return terms with a distance of 0 }
}
\details{
  Returns a \code{\link{EnsOntology}} object.
}
\examples{
  ontologyId( 'GO:0005667' )
  ontologyName( 'transcription factor complex' )
  ontologyAncestors( 'GO:0005667' )
  ontologyAncestorsChart( 'GO:0005667' )
  ontologyDescendents( 'GO:0005667' )
}
