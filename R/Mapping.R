# Mapping Calls

mapping = function( asm_one, region, asm_two, species ) {
  .load.and.parse( c( .Ensembl$mapping, species, asm_one, region, asm_two ) )
}

mappingCdna = function( id, region, species=NULL, object=c( NA, 'gene', 'transcript' ) ) {
  object = match.arg( object )
  params = c()
  if( !is.na( object ) ) params = c( params, .make.params( object=object ) )
  if( !is.null( species ) ) params = c( params, .make.params( species=species ) )
  .load.and.parse( c( .Ensembl$mapping.cdna, id, region ), params )
}

mappingCds = function( id, region, species=NULL, object=c( NA, 'gene', 'transcript' ) ) {
  object = match.arg( object )
  params = c()
  if( !is.na( object ) ) params = c( params, .make.params( object=object ) )
  if( !is.null( species ) ) params = c( params, .make.params( species=species ) )
  .load.and.parse( c( .Ensembl$mapping.cds, id, region ), params )
}

mappingTranslation = function( id, region, species=NULL, object=c( NA, 'gene', 'transcript' ) ) {
  object = match.arg( object )
  params = c()
  if( !is.na( object ) ) params = c( params, .make.params( object=object ) )
  if( !is.null( species ) ) params = c( params, .make.params( species=species ) )
  .load.and.parse( c( .Ensembl$mapping.translation, id, region ), params )
}
