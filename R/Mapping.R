# Mapping Calls

mapping = function( asm_one, region, asm_two, species ) {
  lapply( .load.and.parse( c( .Ensembl$mapping, species, asm_one, region, asm_two ) )$mappings, function( x ) {
    list( original=as( as( data.frame( space=x$original$seq_region_name,
                                       start=x$original$start,
                                       end=x$original$end,
                                       strand=if( x$original$strand == '1' ) '+' else if( x$original$strand == '-1' ) '-' else '*' ,
                                       assembly=x$original$assembly,
                                       coordinate_system=x$original$coordinate_system ), 'RangedData' ), 'GRanges' ),
          mapped=as( as( data.frame( space=x$mapped$seq_region_name,
                                     start=x$mapped$start,
                                     end=x$mapped$end,
                                     strand=if( x$mapped$strand == '1' ) '+' else if( x$mapped$strand == '-1' ) '-' else '*',
                                     assembly=x$mapped$assembly,
                                     coordinate_system=x$mapped$coordinate_system ), 'RangedData' ), 'GRanges' ) )
  } )
}

mappingCdna = function( id, region, species=NULL, object=c( NA, 'gene', 'transcript' ) ) {
  object = match.arg( object )
  params = c()
  if( !is.na( object ) ) params = c( params, .make.params( object=object ) )
  if( !is.null( species ) ) params = c( params, .make.params( species=species ) )
  as( as( do.call( 'rbind', lapply( .load.and.parse( c( .Ensembl$mapping.cdna, id, region ), params )$mappings, function( e ) {
    data.frame( space='NA', start=e$start, end=e$end, strand=if( e$strand == 1 ) '+' else if( e$strand == -1 ) '-' else '*', gap=e$gap, rank=e$rank )
  } ) ), 'RangedData' ), 'GRanges' )
}

mappingCds = function( id, region, species=NULL, object=c( NA, 'gene', 'transcript' ) ) {
  object = match.arg( object )
  params = c()
  if( !is.na( object ) ) params = c( params, .make.params( object=object ) )
  if( !is.null( species ) ) params = c( params, .make.params( species=species ) )
  as( as( do.call( 'rbind', lapply( .load.and.parse( c( .Ensembl$mapping.cds, id, region ), params )$mappings, function( e ) {
    data.frame( space='NA', start=e$start, end=e$end, strand=if( e$strand == 1 ) '+' else if( e$strand == -1 ) '-' else '*', gap=e$gap, rank=e$rank )
  } ) ), 'RangedData' ), 'GRanges' )
}

mappingTranslation = function( id, region, species=NULL, object=c( NA, 'gene', 'transcript' ) ) {
  object = match.arg( object )
  params = c()
  if( !is.na( object ) ) params = c( params, .make.params( object=object ) )
  if( !is.null( species ) ) params = c( params, .make.params( species=species ) )
  as( as( do.call( 'rbind', lapply( .load.and.parse( c( .Ensembl$mapping.translation, id, region ), params )$mappings, function( e ) {
    data.frame( space='NA', start=e$start, end=e$end, strand=if( e$strand == 1 ) '+' else if( e$strand == -1 ) '-' else '*', gap=e$gap, rank=e$rank )
  } ) ), 'RangedData' ), 'GRanges' )
}
