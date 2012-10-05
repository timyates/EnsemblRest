\name{information}
\alias{infoAssembly}
\alias{assemblyDetails}
\alias{infoComparas}
\alias{infoData}
\alias{infoPing}
\alias{infoRest}
\alias{infoSoftware}
\alias{infoSpecies}
\title{
  Information Methods
}
\description{
  Calls the Information methods of the Ensembl REST API.

  Online Documentation:
  \tabular{ll}{
    infoAssembly \tab \url{http://beta.rest.ensembl.org/documentation/info/assembly_info} \cr
    assemblyDetails \tab \url{http://beta.rest.ensembl.org/documentation/info/assembly_stats} \cr
    infoComparas \tab \url{http://beta.rest.ensembl.org/documentation/info/comparas} \cr
    infoData \tab \url{http://beta.rest.ensembl.org/documentation/info/data} \cr
    infoPing \tab \url{http://beta.rest.ensembl.org/documentation/info/ping} \cr
    infoRest \tab \url{http://beta.rest.ensembl.org/documentation/info/rest} \cr
    infoSoftware \tab \url{http://beta.rest.ensembl.org/documentation/info/software} \cr
    infoSpecies \tab \url{http://beta.rest.ensembl.org/documentation/info/species}
  }
}
\usage{
  infoAssembly( species )
  assemblyDetails( id, species )
  infoComparas()
  infoData()
  infoPing()
  infoRest()
  infoSoftware()
  infoSpecies()
}
\arguments{
  \item{species}{ Registry name/aliases used to restrict searches by. Only required if a stable ID is not unique to a species (not the case with Ensembl databases), ie: mouse }
  \item{id}{ The sequence region name to retreive statistics for, ie: 1, X }
}
\value{
  Returns a JSON object converted to a List of Lists.
}
\author{
  Tim Yates
}
\examples{
  infoAssembly( 'human' )
  assemblyDetails( 'X', 'mouse' )
}
