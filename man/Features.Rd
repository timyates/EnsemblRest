\name{featuresByRegion}
\alias{featuresByRegion}
\title{
  Features Methods
}
\description{
  Calls the Features methods of the Ensembl REST API.

  Online Documentation:
  \tabular{ll}{
    featuresByRegion: \tab \url{http://beta.rest.ensembl.org/documentation/info/feature_region}
  }
}
\usage{
  featuresByRegion( region, species,
                    feature=c( 'gene', 'transcript', 'cds', 'exon', 'variation',
                               'somatic_variation', 'structural_variation',
                               'structural_somatic_variation', 'constrained',
                               'regulatory' ),
                    db_type=c( NA, 'core', 'otherfeatures' ),
                    logic_name=NULL,
                    so_term=NULL,
                    species_set=NULL )
}
\arguments{
  \item{region}{ The region to retrieve sequence for. A maximum of 5Mb is allowed to be requested at any one time, ie: X:1..1000:1 }
  \item{species}{ Registry name/aliases used to restrict searches by, ie: human }
  \item{feature}{ The type of feature to retrieve. Multiple values are accepted. Defaults to 'gene' if missing }
  \item{db_type}{ Specify the database type to retrieve features from if not using the core database. We automatically choose the correct type of DB for variation, comparative and regulation features, ie: otherfeatures }
  \item{logic_name}{ Limit retrieval of genes, transcripts and exons by a given logic name }
  \item{so_term}{ SO term to search for variations by. Will search the given and child terms, eg: SO:0001650 }
  \item{species_set}{ The species set name to retrieve constrained elements for. Defaults to mammals. }
}
\details{
  Returns a \code{data.frame} containing the feature response.
}
\examples{
  featuresByRegion( '7:140424943-140624564', 'human', feature=c( 'gene', 'transcript' ) )
}
