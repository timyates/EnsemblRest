\name{mapping}
\alias{EnsemblRest-mapping}
\alias{mapping}
\alias{mappingCdna}
\alias{mappingCds}
\alias{mappingTranslation}
\title{
  Mapping Methods
}
\description{
  Calls the Mapping methods of the Ensembl REST API.

  Online Documentation:
  \tabular{ll}{
    mapping: \tab \url{http://beta.rest.ensembl.org/documentation/info/assembly_map} \cr
    mappingCdna: \tab \url{http://beta.rest.ensembl.org/documentation/info/assembly_cdna} \cr
    mappingCds: \tab \url{http://beta.rest.ensembl.org/documentation/info/assembly_cds} \cr
    mappingTranslation: \tab \url{http://beta.rest.ensembl.org/documentation/info/assembly_translation}
  }
}
\usage{
  mapping( asm_one, region, asm_two, species )
  mappingCdna( id, region, species=NULL, object=c( NA, 'gene', 'transcript' ) )
  mappingCds( id, region, species=NULL, object=c( NA, 'gene', 'transcript' ) )
  mappingTranslation( id, region, species=NULL, object=c( NA, 'gene', 'transcript' ) )
}
\arguments{
  \item{asm_one}{ The version of the assembly we are converting from, eg: NCBI36 }
  \item{asm_two}{ The version of the assembly we are converting to, eg: GRCh37 }
  \item{region}{ The region to retrieve mappings for, eg: X:1..1000:1, X:1..1000:-1, X:1..1000 }
  \item{species}{ Registry name/aliases used to restrict searches by, ie: human }
  \item{id}{ The stable identifier of the entity you wish to retrieve a mapping for, eg: ENST00000288602 }
  \item{object}{ Object type to restrict searches to. Used when a stable ID is not unique to a single class, eg: gene, transcript }
}
\details{
  The function \code{mapping} returns a \code{list} containing a \code{GRanges} object (with the field \code{type} having the value \code{original} for the query location, and \code{mapped} for the result).

  The remaining 3 functions return a \code{GRanges} object containing the mapping ranges.
}
\examples{
  mapping( 'NCBI36', '1..10000:1', 'GRCh37', 'human' )
  mappingCdna( 'ENST00000288602', '100..300' )
  mappingCds( 'ENST00000288602', '100..300' )
  mappingTranslation( 'ENSP00000288602', '100..300' )
}
