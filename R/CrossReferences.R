# Cross References calls

xrefsById = function( id,
                      species,
                      all_levels=FALSE,
                      db_type=c( 'core', 'otherfeatures' ),
                      object=c( NA, 'gene', 'transcript' ),
                      external_db=NULL ) {
  object=match.arg( object )
  params = .make.params( db_type=match.arg( db_type ) )
  if( !is.na( object ) ) params = c( params, .make.params( object=object ) )
  if( !missing( species ) ) params = c( params, .make.params( species=species ) )
  if( all_levels ) params = c( params, .make.params( all_levels=1 ) )
  if( !is.null( external_db ) ) params = c( params, .make.params( external_db=external_db ) )
  .load.and.parse( c( .Ensembl$xrefs, id ), params )
}

xrefsByName = function( name,
                        species,
                        db_type=c( 'core', 'otherfeatures' ),
                        external_db=NULL ) {
  params = .make.params( db_type=match.arg( db_type ) )
  if( !is.null( external_db ) ) params = c( params, .make.params( external_db=external_db ) )
  .load.and.parse( c( .Ensembl$xrefs.name, species, name ), params )
}

xrefsBySymbol = function( symbol, species,
                          db_type=c( 'core', 'otherfeatures' ),
                          object=c( NA, 'gene', 'transcript' ),
                          external_db=NULL ) {
  object=match.arg( object )
  params = .make.params( db_type=match.arg( db_type ) )
  if( !is.na( object ) ) params = c( params, .make.params( object=object ) )
  if( !is.null( external_db ) ) params = c( params, .make.params( external_db=external_db ) )
  .load.and.parse( c( .Ensembl$xrefs.symbol, species, symbol ), params )
}