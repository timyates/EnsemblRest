# Information Calls

infoAssembly = function( species ) {
  getRefClass( 'EnsAssemblyInfo' )$new( .load.and.parse( c( .Ensembl$assembly, species ) ) )
}

assemblyDetails = function( id, species ) {
  getRefClass( 'EnsAssemblyDetails' )$new( .load.and.parse( c( .Ensembl$assembly, species, id ) ) )
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
  .load.and.parse( .Ensembl$info.ping )$ping == '1'
}

infoRest = function() {
  .load.and.parse( .Ensembl$info.rest )$release
}

infoSoftware = function() {
  .load.and.parse( .Ensembl$info.software )$release
}

infoSpecies = function() {
  lapply( .load.and.parse( .Ensembl$info.species )$species, function( a ) {
    getRefClass( 'EnsSpecies' )$new( a )
  } )
}