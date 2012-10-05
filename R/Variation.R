# Variation

variationAllele = function( allele, region, species ) {
  .load.and.parse( c( .Ensembl$variation, species, region, allele, .Ensembl$variation.tail ) )
}

variationId = function( id, species ) {
  .load.and.parse( c( .Ensembl$variation, species, .Ensembl$variation.id, id, .Ensembl$variation.tail ) )
}
