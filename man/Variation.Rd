\name{variation}
\alias{EnsemblRest-variation}
\alias{variationAllele}
\alias{variationId}
\title{
  Variation Methods
}
\description{
  Calls the Variation methods of the Ensembl REST API.

  Online Documentation:
  \tabular{ll}{
    variationAllele: \tab \url{http://beta.rest.ensembl.org/documentation/info/vep} \cr
    variationId: \tab \url{http://beta.rest.ensembl.org/documentation/info/vep_id}
  }
}
\usage{
  variationAllele( allele, region, species )
  variationId( id, species )
}
\arguments{
  \item{allele}{ The allele to change the reference genome to, ie: C }
  \item{region}{ The region to mutate in the specified genome. We only support the current active assembly, eg: 9:22125503-22125502:1 }
  \item{species}{ Registry name/aliases used to restrict searches by, ie: human }
  \item{id}{ Variation ID to lookup. Supports dbSNP, COSMIC and HGMD identifiers, eg: rs9959, COSM476 }
}
\details{
  A \code{\link{EnsVariantConsequence}} object.
}
\examples{
  variationAllele( 'C', '9:22125503-22125502:1', 'human' )
  variationId( 'COSM476', 'human' )
}
