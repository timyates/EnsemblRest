ontologyId = function( id, relation=NULL, simple=FALSE ) {
  params = c()
  if( !is.null( relation ) ) params = c( params, .make.params( relation=relation ) )
  if( simple ) params = c( params, .make.params( simple='1' ) )
  getRefClass( 'EnsOntology' )$new( .load.and.parse( .Ensembl$ontology.id, c( id=id ), params ) )
}

ontologyName = function( name, ontology=NULL, relation=NULL, simple=FALSE ) {
  params = c()
  if( !is.null( relation ) ) params = c( params, .make.params( relation=relation ) )
  if( !is.null( ontology ) ) params = c( params, .make.params( ontology=ontology ) )
  if( simple ) params = c( params, .make.params( simple='1' ) )
  lapply( .load.and.parse( .Ensembl$ontology.name, c( name=name ), params ), getRefClass( 'EnsOntology' )$new )
}

ontologyAncestors = function( id, ontology=NULL ) {
  params = c()
  if( !is.null( ontology ) ) params = c( params, .make.params( ontology=ontology ) )
  lapply( .load.and.parse( .Ensembl$ontology.ancestors, c( id=id ), params ), getRefClass( 'EnsOntology' )$new )
}

ontologyAncestorsChart = function( id, ontology=NULL ) {
  params = c()
  if( !is.null( ontology ) ) params = c( params, .make.params( ontology=ontology ) )
  x = .load.and.parse( .Ensembl$ontology.ancestors.chart, c( id=id ), params )
  sapply( names( x ), function( e ) {
    sapply( names( x[[ e ]] ), function( n ) {
      if( is.null( names( x[[ e ]][[ n ]] ) ) ) {
        lapply( x[[ e ]][[ n ]], getRefClass( 'EnsOntology' )$new )
      }
      else {
        getRefClass( 'EnsOntology' )$new( x[[ e ]][[ n ]] )
      }
    } )
  }, simplify=F )
}