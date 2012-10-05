\name{lookup}
\alias{lookupId}
\title{
  Lookup Methods
}
\description{
  Calls the Lookup methods of the Ensembl REST API.

  Online Documentation:
  \tabular{ll}{
    lookupId: \tab \url{http://beta.rest.ensembl.org/documentation/info/lookup}
  }
}
\usage{
  lookupId( id, species=NULL, db_type=NULL, object=NULL )
}
\arguments{
  \item{id}{ The stable identifier of the entity you wish to retrieve sequence for, ie: ENSG00000170037 }
  \item{species}{ Registry name/aliases used to restrict searches by. Only required if a stable ID is not unique to a species (not the case with Ensembl databases), ie: mouse }
  \item{db_type}{ Force the database to search for symbols in. Useful if you need to use a DB other than core. }
  \item{object}{ Object type to restrict searches to. Used when a stable ID is not unique to a single class, eg: gene, transcript }
}
\value{
  Returns a JSON object converted to a List of Lists.
}
\author{
  Tim Yates
}
\examples{
  lookupId( 'ENSG00000170037' )
}
