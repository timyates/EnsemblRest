if( require( "RUnit", quietly=TRUE ) ) {
  pkg = "EnsemblRest"
 
  if( Sys.getenv( "RCMDCHECK" ) == "FALSE" ) {
    path = file.path( getwd(), "..", "inst", "unitTests" )
  }
  else {
    path = system.file( package=pkg, "unitTests" )
  }

  cat( "
Running unit tests
" )
  print( list( pkg=pkg, getwd=getwd(), pathToUnitTests=path ) )
  library( package=pkg, character.only=TRUE )

  #Fail on warnings
  options( warn=2 )
  
  # Get the pattern (if there is one?)
  patt = Sys.getenv( "RUNITFILEPATTERN" )

  if( is.null( patt ) || nchar( patt ) == 0 ) {
    patt = "^runit.+\\.[rR]$"
  }
  else {
    patt = paste( "^runit\\.", patt, "\\.[rR]$", sep="" )
  }

  testSuite = defineTestSuite( name=paste( pkg, "unit testing" ), testFileRegexp=patt, dirs=path )
  tests = runTestSuite( testSuite )

  pathReport = file.path( path, "report" )
  cat( "------------------- UNIT TEST SUMMARY ---------------------

" )
  printTextProtocol( tests, showDetails=FALSE )
  printTextProtocol( tests, showDetails=FALSE, fileName=paste( pathReport, "Summary.txt", sep="" ) )
  printTextProtocol( tests, showDetails=TRUE,  fileName=paste( pathReport, ".txt", sep="" ) )
  printHTMLProtocol( tests, fileName=paste( pathReport, ".html", sep="" ) )
  tmp = getErrors( tests )
  if( tmp$nFail > 0 | tmp$nErr > 0 ) {
    stop( paste( "

unit testing failed (#test failures: ", tmp$nFail, ", #R errors: ",  tmp$nErr, ")

", sep="" ) )
  }
} else {
  warning( "cannot run unit tests -- package RUnit is not available" )
}
