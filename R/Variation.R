# Variation

variationAllele = function( allele, region, species ) {
  lapply( .load.and.parse( .Ensembl$variation, c( species=species, region=region, allele=allele ) )$data, function( f ) {
    getRefClass( 'EnsVariantConsequence' )$new( f )
  } )
}

variationId = function( id, species ) {
  lapply( .load.and.parse( .Ensembl$variation.id, c( species=species, id=id ) )$data, function( f ) {
    getRefClass( 'EnsVariantConsequence' )$new( f )
  } )
}
