# Mapping Calls

mapping = function( asm_one, region, asm_two, species ) {
  lapply( .load.and.parse( c( .Ensembl$mapping, species, asm_one, region, asm_two ) )$mappings, function( x ) {
    as( as( data.frame( space=c( x$original$seq_region_name, x$mapped$seq_region_name ),
                        start=c( x$original$start, x$mapped$start ),
                        end=c( x$original$end, x$mapped$end ),
                        strand=c( .strandString( x$original$strand ), .strandString( x$mapped$strand ) ),
                        assembly=c( x$original$assembly, x$mapped$assembly ),
                        coordinate_system=c( x$original$coordinate_system, x$mapped$coordinate_system ),
                        type=c( 'original', 'mapped' ),
                        stringsAsFactors=F ), 'RangedData' ), 'GRanges' )
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
