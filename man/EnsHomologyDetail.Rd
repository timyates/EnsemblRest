\name{EnsHomologyDetail}
\alias{EnsHomologyDetail}
\alias{EnsHomologyDetail-class}
\alias{\S4method{new}{EnsHomologyDetail}}
\alias{\S4method{show}{EnsHomologyDetail}}
\title{
  The HomologyDetail class
}
\description{
  The class containing an HomologyDetail
}
\usage{
  \S4method{new}{EnsHomologyDetail}( x=NULL, ... )
  \S4method{show}{EnsHomologyDetail}()
}
\arguments{
  \item{x}{ The \code{List} returned by a call to the rjson wrapper }
  \item{...}{ Other params }
}
\details{
  A class \code{EnsHomologyDetail}, which contains the fields:
  \tabular{ll}{
    id \tab \code{character} \cr
    species \tab \code{character} \cr
    protein_id \tab \code{character} \cr
    perc_pos \tab \code{numeric} \cr
    perc_id \tab \code{numeric} \cr
    cigar_line \tab \code{character} \cr
    align_seq \tab \code{character}
  }
}
