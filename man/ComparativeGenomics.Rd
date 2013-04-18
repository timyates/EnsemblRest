\name{comparativeGenomics}
\alias{EnsemblRest-comparativeGenomics}
\alias{geneTree}
\alias{geneTreeMember}
\alias{geneTreeSymbol}
\alias{homologyById}
\alias{homologyBySymbol}
\title{
  Comparative Genomics Methods
}
\description{
  Calls the Comparative Genomics methods of the Ensembl REST API.

  Online Documentation:
  \tabular{ll}{
    geneTree: \tab \url{http://beta.rest.ensembl.org/documentation/info/genetree} \cr
    homologyById \tab \url{http://beta.rest.ensembl.org/documentation/info/homology_ensemblgene} \cr
    homologyBySymbol \tab \url{http://beta.rest.ensembl.org/documentation/info/homology_symbol}
  }
}
\usage{
  geneTree( gid, 
            nh_format=c( 'simple',
                         'full',
                         'display_label_composite',
                         'species',
                         'species_short_name',
                         'ncbi_taxon',
                         'ncbi_name',
                         'njtree',
                         'phylip' ),
             sequence=c( 'protein', 'cdna', 'none' ),
             aligned=FALSE  )
  geneTreeMember( gid, db_type=NULL, object=NULL, species=NULL )
  geneTreeSymbol( symbol, species, db_type=NULL, object=NULL, external_db=NULL )

  homologyById( id,       # REQUIRED
                species,  # NOT REQUIRED
                target_species,
                target_taxon,
                type=c( 'all', 'orthologues', 'paralogues' ),
                format=c( 'full', 'condensed' ) )
  homologyBySymbol( symbol,  # REQUIRED
                    species, # REQUIRED
                    target_species,
                    target_taxon,
                    type=c( 'all', 'orthologues', 'paralogues' ),
                    format=c( 'full', 'condensed' ) )
}
\arguments{
  \item{gid}{ Ensembl GeneTree ID, ie: ENSGT00390000003602. }
  \item{nh_format}{ The format of a NH (New Hampshire) request. }
  \item{aligned}{ Alter if the PhyloXML returned by this endpoint shows the aligned string with insertions where applicable }
  \item{sequence}{ The type of sequence to bring back. Setting it to none results in no sequence being returned }
  \item{db_type}{ Registry group which we should limit our search to. Useful if a stable ID is not unique to a species, eg \'core\' }
  \item{object}{ Object type to restrict searches to. Used when a stable ID is not unique to a single class. This is equivalent to the Perl API object classes, eg \'gene\', or \'transcript\' }
  \item{external_db}{ Limit your symbol search to just this database, eg: \'HGNC\' }
  \item{id}{ Ensembl Gene id, ie: ENSG00000170037 }
  \item{species}{ Registry name/aliases used to restrict searches by. Only required if a stable ID is not unique to a species (not the case with Ensembl databases), ie: homo_sapiens }
  \item{target_species}{ NOT REQUIRED: Target species to restrict the homology results from. Supports all registry aliases, ie: homo_sapiens, mouse, etc  }
  \item{target_taxon}{ NOT REQUIRED: Taxon identifier to search by, ie: 9606, 10090 }
  \item{type}{ The type of homology to return from this call. Useful if you need only one type of homology back from the service }
  \item{symbol}{ Symbol to search with, ie: BRCA2 }
  \item{format}{ Specify the formats to emit from this endpoint. }
}
\details{
  \code{geneTree} returns an NH format tree as a character vector.

  \code{homologyById} and \code{homologyBySymbol} return a \code{\link{EnsHomologyResponse}} object.
}
\examples{
  geneTree( 'ENSGT00390000003602' )
  geneTreeMember( 'ENSG00000157764')
  geneTreeSymbol( 'BRCA2', 'homo_sapiens' )
  homologyById( 'ENSG00000170037' )
  homologyBySymbol( 'BRCA2', 'human' )
}
