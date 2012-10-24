# Mapping Calls

mapping = function( asm_one, region, asm_two, species ) {
  lapply( .load.and.parse( .Ensembl$mapping, c( species=species, asm_one=asm_one, region=region, asm_two=asm_two ) )$mappings, function( x ) {
    as( as( data.frame( space=c( x$original$seq_region_name, x$mapped$seq_region_name ),
                        start=c( as.numeric( x$original$start ), as.numeric( x$mapped$start ) ),
                        end=c( as.numeric( x$original$end ), as.numeric( x$mapped$end ) ),
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
  as( as( do.call( 'rbind', lapply( .load.and.parse( .Ensembl$mapping.cdna, c( id=id, region=region ), params )$mappings, function( e ) {
    data.frame( space=e$seq_region_name, start=e$start, end=e$end, strand=.strandString( e$strand ), gap=e$gap, rank=e$rank )
  } ) ), 'RangedData' ), 'GRanges' )
}

mappingCds = function( id, region, species=NULL, object=c( NA, 'gene', 'transcript' ) ) {
  object = match.arg( object )
  params = c()
  if( !is.na( object ) ) params = c( params, .make.params( object=object ) )
  if( !is.null( species ) ) params = c( params, .make.params( species=species ) )
  as( as( do.call( 'rbind', lapply( .load.and.parse( .Ensembl$mapping.cds, c( id=id, region=region ), params )$mappings, function( e ) {
    data.frame( space=e$seq_region_name, start=e$start, end=e$end, strand=.strandString( e$strand ), gap=e$gap, rank=e$rank )
  } ) ), 'RangedData' ), 'GRanges' )
}

mappingTranslation = function( id, region, species=NULL, object=c( NA, 'gene', 'transcript' ) ) {
  object = match.arg( object )
  params = c()
  if( !is.na( object ) ) params = c( params, .make.params( object=object ) )
  if( !is.null( species ) ) params = c( params, .make.params( species=species ) )
  as( as( do.call( 'rbind', lapply( .load.and.parse( .Ensembl$mapping.translation, c( id=id, region=region ), params )$mappings, function( e ) {
    data.frame( space=e$seq_region_name, start=e$start, end=e$end, strand=.strandString( e$strand ), gap=e$gap, rank=e$rank )
  } ) ), 'RangedData' ), 'GRanges' )
}
