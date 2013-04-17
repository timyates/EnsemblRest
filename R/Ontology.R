ontologyId = function( id, relation=NULL, simple=FALSE ) {
  params = c()
  if( !is.null( relation ) ) params = c( params, .make.params( relation=relation ) )
  if( simple ) params = c( params, .make.params( simple='1' ) )
  getRefClass( 'EnsOntology' )$new( .load.and.parse( .Ensembl$ontology.id, c( id=id ), params ) )
}
