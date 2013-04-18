taxonomyId = function( id, simple=FALSE ) {
  params = c()
  if( simple ) params = c( params, .make.params( simple='1' ) )
  getRefClass( 'EnsTaxonomy' )$new( .load.and.parse( .Ensembl$taxonomy.id, c( id=id ), params ) )
}
