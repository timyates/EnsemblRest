featuresById = function( id,
                         feature=c( 'gene', 'transcript', 'cds', 'exon',
                                    'variation', 'somatic_variation',
                                    'structural_variation',
                                    'structural_somatic_variation',
                                    'constrained', 'regulatory' ),
                         db_type=c( NA, 'core', 'otherfeatures' ),
                         logic_name=NULL,
                         misc_set=NULL,
                         object_type=NULL,
                         so_term=NULL,
                         species=NULL,
                         species_set=NULL ) {
  db_type = match.arg( db_type )
  feature = if( missing( feature ) ) 'gene' else match.arg( feature, several.ok=TRUE )
  params = c()
  if( !is.na( db_type ) ) params = c( params, .make.params( db_type=db_type ) )
  if( !is.null( feature ) ) params = c( params, .make.params( feature=feature ) )
  if( !is.null( logic_name ) ) params = c( params, .make.params( logic_name=logic_name ) )
  if( !is.null( misc_set ) ) params = c( params, .make.params( misc_set=misc_set ) )
  if( !is.null( object_type ) ) params = c( params, .make.params( object_type=object_type ) )
  if( !is.null( so_term ) ) params = c( params, .make.params( so_term=so_term ) )
  if( !is.null( species ) ) params = c( params, .make.params( species=species ) )
  if( !is.null( species_set ) ) params = c( params, .make.params( species_set=species_set ) )
  munger = function( e ) {
    # Rename to space for GRanges
    if( 'seq_region_name' %in% names( e ) ) {
      names( e )[ names( e ) == 'seq_region_name' ] = 'space'
    }
    # characterize strand
    if( 'strand' %in% names( e ) ) {
      e$strand = .strandString( e$strand )
    }
    # Strip NULL and make a data.frame
    data.frame( e[ !sapply( e, is.null ) ] )
  }
  as( as( do.call( 'rbind.fill',
    lapply( .load.and.parse( .Ensembl$featureId, c( id=id ), params ), munger )
  ), 'RangedData' ), 'GRanges' )
}

featuresByRegion = function( region, species,
                             feature=c( 'gene', 'transcript', 'cds', 'exon',
                                        'variation', 'somatic_variation',
                                        'structural_variation',
                                        'structural_somatic_variation',
                                        'constrained', 'regulatory' ),
                             db_type=c( NA, 'core', 'otherfeatures' ),
                             logic_name=NULL,
                             misc_set=NULL,
                             so_term=NULL,
                             species_set=NULL ) {
  db_type = match.arg( db_type )
  feature = if( missing( feature ) ) 'gene' else match.arg( feature, several.ok=TRUE )
  params = c()
  if( !is.na( db_type ) ) params = c( params, .make.params( db_type=db_type ) )
  if( !is.null( feature ) ) params = c( params, .make.params( feature=feature ) )
  if( !is.null( logic_name ) ) params = c( params, .make.params( logic_name=logic_name ) )
  if( !is.null( so_term ) ) params = c( params, .make.params( so_term=so_term ) )
  if( !is.null( species_set ) ) params = c( params, .make.params( species_set=species_set ) )
  munger = function( e ) {
    # Rename to space for GRanges
    if( 'seq_region_name' %in% names( e ) ) {
      names( e )[ names( e ) == 'seq_region_name' ] = 'space'
    }
    # characterize strand
    if( 'strand' %in% names( e ) ) {
      e$strand = .strandString( e$strand )
    }
    # Strip NULL and make a data.frame
    data.frame( e[ !sapply( e, is.null ) ] )
  }
  as( as( do.call( 'rbind.fill',
    lapply( .load.and.parse( .Ensembl$feature, c( species=species, region=region ), params ), munger )
  ), 'RangedData' ), 'GRanges' )
}