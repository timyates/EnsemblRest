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
  rbind.fill( lapply( .load.and.parse( .Ensembl$xrefs, c( id=id ), params ), function( elem ) {
    # Strip NULL and list()
    data.frame( elem[ !sapply( elem, is.null ) & ( sapply( elem, function( b ) { !is.list( b ) || length( b ) > 0 } ) ) ] )
  } ) )
}

xrefsByName = function( name,
                        species,
                        db_type=c( 'core', 'otherfeatures' ),
                        external_db=NULL ) {
  params = .make.params( db_type=match.arg( db_type ) )
  if( !is.null( external_db ) ) params = c( params, .make.params( external_db=external_db ) )
  rbind.fill( lapply( .load.and.parse( .Ensembl$xrefs.name, c( species=species, name=name ), params ), function( elem ) {
    # Strip NULL and list()
    data.frame( elem[ !sapply( elem, is.null ) & ( sapply( elem, function( b ) { !is.list( b ) || length( b ) > 0 } ) ) ] )
  } ) )
}

xrefsBySymbol = function( symbol, species,
                          db_type=c( 'core', 'otherfeatures' ),
                          object=c( NA, 'gene', 'transcript' ),
                          external_db=NULL ) {
  object=match.arg( object )
  params = .make.params( db_type=match.arg( db_type ) )
  if( !is.na( object ) ) params = c( params, .make.params( object=object ) )
  if( !is.null( external_db ) ) params = c( params, .make.params( external_db=external_db ) )
  do.call( 'rbind', lapply( .load.and.parse( .Ensembl$xrefs.symbol, c( species=species, symbol=symbol ), params ), as.data.frame ) )
}