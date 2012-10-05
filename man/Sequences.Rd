\name{sequences}
\alias{sequenceById}
\alias{sequenceByRegion}
\title{
  Sequence Methods
}
\description{
  Calls the Sequence methods of the Ensembl REST API.

  Online Documentation:
  \tabular{ll}{
    sequenceById: \tab \url{http://beta.rest.ensembl.org/documentation/info/sequence_id} \cr
    sequenceByRegion: \tab \url{http://beta.rest.ensembl.org/documentation/info/sequence_region}
  }
}
\usage{
  sequenceById( id,
                db_type=c( NA, 'core', 'otherfeatures' ),
                expand_3prime=NULL,
                expand_5prime=NULL,
                format=c( NA, 'fasta' ),
                mask=c( NA, 'hard' ),
                object=c( NA, 'gene', 'transcript' ),
                species=NULL,
                type=c( 'genomic', 'cds', 'cdna', 'protein' ) )
  sequenceByRegion( region, species,
                    expand_3prime=NULL,
                    expand_5prime=NULL,
                    format=c( NA, 'fasta' ),
                    mask=c( NA, 'hard' ) )
}
\arguments{
  \item{id}{ The stable identifier of the entity you wish to retrieve sequence for and an optional inline format, eg: ENSG00000157764 }
  \item{db_type}{ Force the database to search for symbols in. Useful if you need to use a DB other than core. }
  \item{expand_3prime}{ Expand the sequence downstream of the sequence by this many basepairs. Only available when using type=genomic, eg: 1000 }
  \item{expand_5prime}{ Expand the sequence upstream of the sequence by this many basepairs. Only available when using type=genomic, eg: 1000 }
  \item{format}{ Format to retrieve data in, default is JSON }
  \item{mask}{ Request the sequence masked for repeat sequences. Hard will mask all repeats as N's and soft will mask repeats as lowercased characters. Only available when using genomic. }
  \item{object}{ Object type to restrict searches to. Used when a stable ID is not unique to a single class, eg: gene, transcript }
  \item{species}{ Registry name/aliases used to restrict searches by, ie: human }
  \item{type}{ The type of sequence to retrieve. Used to inform the API of the type of sequence wanted. Defaults to genomic where applicable i.e. not translations. cdna refers to the spliced transcript sequence with UTR and cds refers to the spliced transcript sequence without UTR. }
  \item{region}{ The region to retrieve mappings for. A maximum of 10Mb is allowed to be requested at any one time, eg: X:1..1000:1, X:1..1000:-1, X:1..1000 }
}
\value{
  Returns a JSON object converted to a List of Lists.
}
\author{
  Tim Yates
}
\examples{
  sequenceById( 'ENSG00000157764' )
  sequenceByRegion( 'X:1_000_000..1_000_100:1', 'human', format='fasta' )
}
