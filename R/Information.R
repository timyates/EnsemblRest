# Information Calls

infoAssembly = function( species ) {
  getRefClass( 'EnsAssemblyInfo' )$new( .load.and.parse( .Ensembl$assembly, c( species=species ) ) )
}

assemblyDetails = function( id, species ) {
  getRefClass( 'EnsAssemblyDetails' )$new( .load.and.parse( .Ensembl$assembly.region, c( species=species, region_name=id ) ) )
}

infoComparas = function() {
  unlist( lapply( .load.and.parse( .Ensembl$info.comparas )$comparas, function( a ) {
    r = list( a$release )
    names( r ) = a$name
    r
  } ) )
}

infoData = function() {
  unlist( .load.and.parse( .Ensembl$info.data )$releases )
}

isAlive = function() {
  res = tryCatch(.load.and.parse( .Ensembl$info.ping ), error = function(e) e)
  !is(res, "error") && res$ping == "1"
}

infoRest = function() {
  .load.and.parse( .Ensembl$info.rest )$release
}

infoSoftware = function() {
  .load.and.parse( .Ensembl$info.software )$release
}

infoSpecies = function( division=NULL ) {
  params=c()
  if( !is.null( division ) ) params = c( params, .make.params( division=division ) )
  lapply( .load.and.parse( .Ensembl$info.species, NULL, params )$species, function( a ) {
    getRefClass( 'EnsSpecies' )$new( a )
  } )
}
