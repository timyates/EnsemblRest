# Information Calls

infoAssembly = function( species ) {
  .load.and.parse( c( .Ensembl$assembly, species ) )
}

assemblyDetails = function( id, species ) {
  .load.and.parse( c( .Ensembl$assembly, species, id ) )
}

infoComparas = function() {
  .load.and.parse( .Ensembl$info.comparas )
}

infoData = function() {
  .load.and.parse( .Ensembl$info.data )
}

infoPing = function() {
  .load.and.parse( .Ensembl$info.ping )
}

infoRest = function() {
  .load.and.parse( .Ensembl$info.rest )
}

infoSoftware = function() {
  .load.and.parse( .Ensembl$info.software )
}

infoSpecies = function() {
  .load.and.parse( .Ensembl$info.species )
}