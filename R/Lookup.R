# Lookup Calls

lookupId = function( id, species=NULL, db_type=NULL, object=NULL ) {
  params = c()
  if( !is.null( species ) ) params = c( params, .make.params( species=species ) )
  if( !is.null( db_type ) ) params = c( params, .make.params( db_type=db_type ) )
  if( !is.null( object ) ) params = c( params, .make.params( object=object ) )
  d = .load.and.parse( c( .Ensembl$lookup, id ), params )
  data.frame( id=d$id,
              species=d$species,
              object_type=d$object_type,
              db_type=d$db_type )
}
