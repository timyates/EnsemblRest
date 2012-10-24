# Sequences

sequenceById = function( id,
                         db_type=c( NA, 'core', 'otherfeatures' ),
                         expand_3prime=NULL,
                         expand_5prime=NULL,
                         format=c( NA, 'fasta' ),
                         mask=c( NA, 'hard' ),
                         object=c( NA, 'gene', 'transcript' ),
                         species=NULL,
                         type=c( 'genomic', 'cds', 'cdna', 'protein' ) ) {
  db_type = match.arg( db_type )
  format = match.arg( format )
  mask = match.arg( mask )
  object = match.arg( object )
  params = .make.params( type=match.arg( type ) )
  if( !is.na( db_type ) ) params = c( params, .make.params( db_type=db_type ) )
  if( !is.na( format ) ) params = c( params, .make.params( format=format ) )
  if( !is.na( mask ) ) params = c( params, .make.params( mask=mask ) )
  if( !is.na( object ) ) params = c( params, .make.params( object_type=object ) )
  if( !is.null( expand_3prime ) ) params = c( params, .make.params( expand_3prime=expand_3prime ) )
  if( !is.null( expand_5prime ) ) params = c( params, .make.params( expand_5prime=expand_5prime ) )
  if( !is.null( species ) ) params = c( params, .make.params( species=species ) )
  .load.and.parse( .Ensembl$sequence.id, c( id=id ),
                   params,
                   if( is.na( format ) ) .Ensembl$json.content.type else 'content-type=text/x-fasta' )
}

sequenceByRegion = function( region, species,
                             expand_3prime=NULL,
                             expand_5prime=NULL,
                             format=c( NA, 'fasta' ),
                             mask=c( NA, 'hard' ) ) {
  format = match.arg( format )
  mask = match.arg( mask )
  params = c()
  if( !is.na( format ) ) params = c( params, .make.params( format=format ) )
  if( !is.na( mask ) ) params = c( params, .make.params( mask=mask ) )
  if( !is.null( expand_3prime ) ) params = c( params, .make.params( expand_3prime=expand_3prime ) )
  if( !is.null( expand_5prime ) ) params = c( params, .make.params( expand_5prime=expand_5prime ) )
  .load.and.parse( .Ensembl$sequence.region, c( species=species, region=region ),
                   params,
                   if( is.na( format ) ) .Ensembl$json.content.type else 'content-type=text/x-fasta' )
}
