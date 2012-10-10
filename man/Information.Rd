\name{information}
\alias{EnsemblRest-information}
\alias{infoAssembly}
\alias{assemblyDetails}
\alias{infoComparas}
\alias{infoData}
\alias{isAlive}
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
    isAlive \tab \url{http://beta.rest.ensembl.org/documentation/info/ping} \cr
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
  isAlive()
  infoRest()
  infoSoftware()
  infoSpecies()
}
\arguments{
  \item{species}{ Registry name/aliases used to restrict searches by. Only required if a stable ID is not unique to a species (not the case with Ensembl databases), ie: mouse }
  \item{id}{ The sequence region name to retreive statistics for, ie: 1, X }
}
\details{
  The method isAlive returns a \code{logical} showing whether the service is alive.

  The method \code{infoRest} returns a character vector containing the version of the rest API

  The methods \code{infoComparas}, \code{infoRest}, \code{infoSoftware} and \code{infoData} return a character vector of version information.

  The method \code{infoSpecies} returns a list of \code{\link{EnsSpecies}} objects.
}
\examples{
  infoAssembly( 'human' )
  assemblyDetails( 'X', 'mouse' )
}
