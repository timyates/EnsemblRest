\name{featuresByRegion}
\alias{featuresById}
\alias{featuresByRegion}
\title{
  Features Methods
}
\description{
  Calls the Features methods of the Ensembl REST API.

  Online Documentation:
  \tabular{ll}{
    featuresById: \tab \url{http://beta.rest.ensembl.org/documentation/info/feature_id} \cr
    featuresByRegion: \tab \url{http://beta.rest.ensembl.org/documentation/info/feature_region}
  }
}
\usage{
  featuresById( id,
                feature=c( 'gene', 'transcript', 'cds', 'exon',
                           'variation', 'somatic_variation',
                           'structural_variation',
                           'structural_somatic_variation',
                           'constrained', 'regulatory' ),
                db_type=c( NA, 'core', 'otherfeatures' ),
                logic_name=NULL,
                misc_set=NULL,
                object_type=NULL,
                so_term=NULL,
                species=NULL,
                species_set=NULL )
  featuresByRegion( region, species,
                    feature=c( 'gene', 'transcript', 'cds', 'exon', 'variation',
                               'somatic_variation', 'structural_variation',
                               'structural_somatic_variation', 'constrained',
                               'regulatory' ),
                    db_type=c( NA, 'core', 'otherfeatures' ),
                    logic_name=NULL,
                    misc_set=NULL,
                    so_term=NULL,
                    species_set=NULL )
}
\arguments{
  \item{id}{ The stable identifier of the entity you wish to retrieve, eg: ENSG00000157764 }
  \item{region}{ The region to retrieve, ie: X:1..1000:1 }
  \item{species}{ Registry name/aliases used to restrict searches by, ie: human }
  \item{feature}{ The type of feature to retrieve. Multiple values are accepted. Defaults to 'gene' if missing }
  \item{db_type}{ Specify the database type to retrieve features from if not using the core database. We automatically choose the correct type of DB for variation, comparative and regulation features, ie: otherfeatures }
  \item{logic_name}{ Limit retrieval of genes, transcripts and exons by a given logic name }
  \item{misc_set}{ Miscellaneous set which groups together misc feature entries. Consult the DB or returned data sets to discover what is available, eg: cloneset_30k }
  \item{object_type}{ Object type to restrict searches by. Only required if a stable ID is not unique to an object class (not the case with Ensembl databases), eg: gene }
  \item{so_term}{ SO term to search for variations by. Will search the given and child terms, eg: SO:0001650 }
  \item{species_set}{ The species set name to retrieve constrained elements for. Defaults to mammals. }
}
\details{
  Returns a \code{data.frame} containing the feature response.
}
\examples{
  featuresById( 'ENSG00000157764' )
  featuresByRegion( '7:140424943-140624564', 'human', feature=c( 'gene', 'transcript' ) )
}
