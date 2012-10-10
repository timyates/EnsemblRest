# Variation

variationAllele = function( allele, region, species ) {
  lapply( .load.and.parse( c( .Ensembl$variation, species, region, allele, .Ensembl$variation.tail ) )$data, function( f ) {
    getRefClass( 'EnsVariantConsequence' )$new( f )
  } )
}

variationId = function( id, species ) {
  lapply( .load.and.parse( c( .Ensembl$variation, species, .Ensembl$variation.id, id, .Ensembl$variation.tail ) )$data, function( f ) {
    getRefClass( 'EnsVariantConsequence' )$new( f )
  } )
}
