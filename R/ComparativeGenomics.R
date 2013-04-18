# Comparative Genomics calls

geneTree = function( gid, 
                     nh_format=c( 'simple',
                                  'full',
                                  'display_label_composite',
                                  'species',
                                  'species_short_name',
                                  'ncbi_taxon',
                                  'ncbi_name',
                                  'njtree',
                                  'phylip' ),
                     sequence=c( 'protein', 'cdna', 'none' ),
                     aligned=FALSE  ) {
  nh_format = match.arg( nh_format )
  sequence = match.arg( sequence )
  params = .make.params( nh_format=nh_format )
  params = c( params, .make.params( sequence=sequence ) )
  if( aligned ) {
    params = c( params, 'aligned=True' )
  }
  .load.and.parse( .Ensembl$genetree, c( id=gid ), params, 'content-type=text/x-nh' )
}

geneTreeMember = function( gid,
                           db_type=NULL,
                           object=NULL,
                           species=NULL ) {
  params = c()
  if( !is.null( db_type ) ) params = c( params, .make.params( db_type=db_type ) )
  if( !is.null( object ) )  params = c( params, .make.params( object=object ) )
  if( !is.null( species ) ) params = c( params, .make.params( species=species ) )
  .load.and.parse( .Ensembl$genetree.member, c( id=gid ), params, 'content-type=text/x-nh' )
}

geneTreeSymbol = function( symbol, species,
                           db_type=NULL,
                           object=NULL,
                           external_db=NULL ) {
  params = c()
  if( !is.null( db_type ) )     params = c( params, .make.params( db_type=db_type ) )
  if( !is.null( object ) )      params = c( params, .make.params( object=object ) )
  if( !is.null( external_db ) ) params = c( params, .make.params( external_db=external_db ) )
  .load.and.parse( .Ensembl$genetree.symbol, c( symbol=symbol, species=species ), params, 'content-type=text/x-nh' )
}

homologyById = function( id,
                         species,
                         target_species,
                         target_taxon,
                         type=c( 'all', 'orthologues', 'paralogues' ),
                         format=c( 'full', 'condensed' ) ) {
  params = .make.params( type=match.arg( type ), format=match.arg( format ) )
  if( !missing( species ) ) params = c( params, .make.params( species=species ) )
  if( !missing( target_species ) ) params = c( params, .make.params( target_species=target_species ) )
  if( !missing( target_taxon ) ) params = c( params, .make.params( target_taxon=target_taxon ) )
  r = lapply( id, function( i ) {
    getRefClass( 'EnsHomologyResponse' )$new( .load.and.parse( .Ensembl$homology.id, c( id=i ), params )$data )
  } )
  names( r ) = id
  r
}

homologyBySymbol = function( symbol,
                             species,
                             target_species,
                             target_taxon,
                             type=c( 'all', 'orthologues', 'paralogues' ),
                             format=c( 'full', 'condensed' ) ) {
  params = .make.params( type=match.arg( type ), format=match.arg( format ) )
  if( !missing( target_species ) ) params = c( params, .make.params( target_species=target_species ) )
  if( !missing( target_taxon ) ) params = c( params, .make.params( target_taxon=target_taxon ) )
  r = lapply( symbol, function( i ) {
    getRefClass( 'EnsHomologyResponse' )$new( .load.and.parse( .Ensembl$homology.symbol, c( species=species, symbol=i ), params )$data )
  } )
  names( r ) = symbol
  r
}