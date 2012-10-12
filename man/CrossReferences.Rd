\name{crossReferences}
\alias{EnsemblRest-crossReferences}
\alias{xrefsById}
\alias{xrefsByName}
\alias{xrefsBySymbol}
\title{
  Cross Referencing Methods
}
\description{
  Calls the Cross Referencing methods of the Ensembl REST API.

  Online Documentation:
  \tabular{ll}{
    xrefsById: \tab \url{http://beta.rest.ensembl.org/documentation/info/xref_id} \cr
    xrefsByName \tab \url{http://beta.rest.ensembl.org/documentation/info/xref_name} \cr
    xrefsBySymbol \tab \url{http://beta.rest.ensembl.org/documentation/info/xref_external}
  }
}
\usage{
  xrefsById( id,       # REQUIRED
             species,  # NOT REQUIRED
             all_levels=FALSE,
             db_type=c( 'core', 'otherfeatures' ),
             object=c( NA, 'gene', 'transcript' ),
             external_db=NULL )
  xrefsByName( name,    # REQUIRED
               species, # REQUIRED
               db_type=c( 'core', 'otherfeatures' ),
               external_db=NULL )
  xrefsBySymbol( symbol,
                 species,
                 db_type=c( 'core', 'otherfeatures' ),
                 object=c( NA, 'gene', 'transcript' ),
                 external_db=NULL )
}
\arguments{
  \item{id}{ The stable identifier of the entity you wish to retrieve sequence for, ie: ENSG00000170037 }
  \item{species}{ Registry name/aliases used to restrict searches by. Only required if a stable ID is not unique to a species (not the case with Ensembl databases), ie: mouse }
  \item{all_levels}{ Descend down all available levels in the given stable id's object and return all cross references linked to its childern. Specifying this on a Gene will also return values from its transcripts and translations. }
  \item{db_type}{ Force the database to search for symbols in. Useful if you need to use a DB other than core. }
  \item{object}{ Object type to restrict searches to. Used when a stable ID is not unique to a single class, eg: gene, transcript }
  \item{external_db}{ Limit searches to a single database name, eg: HGNC }
  \item{name}{ The external name/primary accession to search for, eg: BRAF, NM_004333 }
  \item{symbol}{ The external name/symbol to search for, eg: BRAF, BRCA2 }
}
\details{
  All of these functions return a \code{data.frame}.

  It should be noted that \code{xrefsById} and \code{xrefsByName} return results from
  different datasources with different columns, so the results are returned in a data.frame containing
  a superset of column names, with non-applicable columns for a given result filled with \code{NA}.
}
\examples{
  xrefsById( 'ENSG00000170037' )
  xrefsByName( 'NM_004333', 'human', external_db='HGNC' )
  xrefsBySymbol( 'BRAF', 'human' )
}
