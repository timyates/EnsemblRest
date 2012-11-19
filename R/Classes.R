# Classes
setRefClass( "EnsHomologyResponse",
            fields=list(
              id='character',
              homologies='data.frame'
            ),
            methods = list(
               initialize = function( x=NULL, ... ) {
                 'Initialize a Homology Response from data returned by rjson'
                 if( !is.null( x ) ) {
                   id <<- x[[1]]$id
                   homologies <<- rbind.fill( lapply( x[[1]]$homologies, function( a ) {
                     data.frame( a[ !sapply( a, is.null ) ], stringsAsFactors=F )
                   } ) )
                 }
               },
               show = function() {
                 'Method for automatically printing Homology'
                 cat( paste( '$id:', id, '$homologies:', length( homologies ), 'homologies' ) )
               }
            ) )

setRefClass( "EnsAssemblyInfo",
             fields=list( 
               assembly_name='character',
               top_level_seq_region_names='character',
               assembly_date='character',
               coord_system_versions='character',
               genebuild_start_date='character',
               genebuild_initial_release_date='character',
               schema_build='character',
               genebuild_last_geneset_update='character',
               genebuild_method='character'
             ),
             methods = list(
               initialize = function( x=NULL, ... ) {
                 'Initialize an Assembly info reference from data returned by rjson'
                 if( !is.null( x ) ) {
                   assembly_name                  <<- x$assembly.name
                   top_level_seq_region_names     <<- x$top_level_seq_region_names
                   assembly_date                  <<- x$assembly.date
                   coord_system_versions          <<- x$coord_system_versions
                   genebuild_start_date           <<- x$genebuild.start_date
                   genebuild_initial_release_date <<- x$genebuild.initial_release_date
                   schema_build                   <<- x$schema_build
                   genebuild_last_geneset_update  <<- x$genebuild.last_geneset_update
                   genebuild_method               <<- x$genebuild.method
                 }
               },
               show = function() {
                 'Method for automatically printing Ref'
                 if( length( assembly_name ) > 0 )                  cat( paste( 'assembly_name                  :', assembly_name ), '\n' )
                 if( length( assembly_date ) > 0 )                  cat( paste( 'assembly_date                  :', assembly_date ), '\n' )
                 if( length( coord_system_versions ) > 0 )          cat( paste( 'coord_system_versions          :', paste( coord_system_versions, collapse=', ' ) ), '\n' )
                 if( length( schema_build ) > 0 )                   cat( paste( 'schema_build                   :', schema_build ), '\n' )
                 if( length( genebuild_start_date ) > 0 )           cat( paste( 'genebuild_start_date           :', genebuild_start_date ), '\n' )
                 if( length( genebuild_initial_release_date ) > 0 ) cat( paste( 'genebuild_initial_release_date :', genebuild_initial_release_date ), '\n' )
                 if( length( genebuild_last_geneset_update ) > 0 )  cat( paste( 'genebuild_last_geneset_update  :', genebuild_last_geneset_update ), '\n' )
                 if( length( genebuild_method ) > 0 )               cat( paste( 'genebuild_method               :', genebuild_method ), '\n' )
                 if( length( top_level_seq_region_names ) > 0 )     cat( paste( 'top_level_seq_region_names     :', paste( top_level_seq_region_names, collapse=', ' ) ), '\n' )
               }
           ) )

setRefClass( "EnsAssemblyDetails",
             fields=list( 
               is_chromosome='logical',
               length='numeric',
               assembly_exception_type='character',
               coordinate_system='character'
             ),
             methods = list(
               initialize = function( x=NULL, ... ) {
                 'Initialize an Assembly details reference from data returned by rjson'
                 if( !is.null( x ) ) {
                   is_chromosome           <<- x$is_chromosome == '1'
                   length                  <<- as.numeric( x$length )
                   assembly_exception_type <<- x$assembly_exception_type
                   coordinate_system       <<- x$coordinate_system
                 }
               },
               show = function() {
                 'Method for automatically printing Ref'
                 if( length( is_chromosome ) > 0 )           cat( paste( 'is_chromosome           :', is_chromosome ), '\n' )
                 if( length( length ) > 0 )                  cat( paste( 'length                  :', length ), '\n' )
                 if( length( assembly_exception_type ) > 0 ) cat( paste( 'assembly_exception_type :', assembly_exception_type ), '\n' )
                 if( length( coordinate_system ) > 0 )       cat( paste( 'coordinate_system       :', coordinate_system ), '\n' )
               }
           ) )

setRefClass( "EnsSpecies",
             fields=list( 
               name='character',
               aliases='character',
               groups='character',
               release='character'
             ),
             methods = list(
               initialize = function( x=NULL, ... ) {
                 'Initialize an Species reference from data returned by rjson'
                 if( !is.null( x ) ) {
                   name  <<- x$name
                   aliases <<- unlist( x$aliases )
                   groups <<- unlist( x$groups )
                   release <<- x$release
                 }
               },
               show = function() {
                 'Method for automatically printing Ref'
                 if( length( name ) > 0 )    cat( paste( 'name    :', name ), '\n' )
                 if( length( aliases ) > 0 ) cat( paste( 'aliases :', paste( aliases, collapse=', ' ) ), '\n' )
                 if( length( groups ) > 0 )  cat( paste( 'groups  :', paste( groups, collapse=', ' ) ), '\n' )
                 if( length( release ) > 0 ) cat( paste( 'release :', release ), '\n' )
               }
           ) )

.skip.nulls = function( name, data, curr, f ) {
  if( is.null( data[[name]] ) ) {
    curr
  }
  else {
    curr = c( curr, if( is.null( f ) ) data[[name]] else f( data[[name]] ) )
    names(curr)[length(curr)] = as.character( name )
    curr
  }
}

.process.data = function( data, format ) {
  ret = c()
  for( x in names( format ) ) {
    ret = .skip.nulls( x, data, ret, format[[x]] )
  }
  as.data.frame( t( ret ) )
}

setRefClass( "EnsTranscript",
             fields=list(
               protein_features='data.frame',
               alleles='data.frame',
               data='data.frame'
             ),
             methods = list(
               initialize = function( x=NULL, ... ) {
                 'Initialize an Species reference from data returned by rjson'
                 if( !is.null( x ) ) {

                   data <<- .process.data( x, list( name=NULL, gene_id=NULL, transcript_id=NULL, biotype=NULL, ccds=NULL,
                                                    cdna_allele_string=NULL, codon_position=NULL, translation_stable_id=NULL,
                                                    translation_start=as.numeric, translation_end=as.numeric, exon_number=NULL, intron_number=NULL,
                                                    cdna_start=as.numeric, cdna_end=as.numeric, cds_start=as.numeric, cds_end=as.numeric,
                                                    is_canonical=function(a) { a == 1 } ) )
                   if( !is.null( x$protein_features ) ) {
                     protein_features <<- do.call( 'rbind', lapply( x$protein_features, function( p ) {
                       data.frame( name=p$name, db=p$db )
                     } ) )
                   }
                   alleles <<- rbind.fill( lapply( x$alleles, function( a ) {
                     j = a
                     .process.data( j, list( name=NULL, display_codon_allele_string=NULL, pep_allele_string=NULL,
                                             codon_allele_string=NULL, hgvs_transcript=NULL, hgvs_protein= NULL,
                                             polyphen_score=NULL, polyphen_prediction=NULL, sift_score=as.numeric,
                                             sift_prediction=NULL, consequence_terms=NULL ) )
                   } ) )
                 }
               },
               show = function() {
                 'Method for automatically printing Ref'
                 if( dim( data )[1] > 0 ) {
                   cat(                                     'data             :\n' )
                   print( data )
                 }
                 if( dim( alleles )[1] > 0 ) {
                   cat(                                     '\nalleles          :\n' )
                   print( alleles )
                 }
                 if( length( protein_features ) > 0 ) {
                   cat(                                     '\nprotein_features :\n' )
                   print( protein_features )
                 }
               }
           ) )


setRefClass( "EnsVariantConsequence",
             fields=list(
               is_somatic='logical',
               location='GRanges',
               name='character',
               hgvs='character',
               transcripts='list'
             ),
             methods = list(
               initialize = function( x=NULL, ... ) {
                 'Initialize an Species reference from data returned by rjson'
                 if( !is.null( x ) ) {
                   is_somatic <<- x$is_somatic == '1'
                   name <<- if( is.null( x$name ) ) '' else x$name
                   location <<- as( as( data.frame( space=x$location$name,
                                                    start=as.numeric( x$location$start ),
                                                    end=as.numeric( x$location$end ),
                                                    strand=.strandString( x$location$strand ),
                                                    coord_system=x$location$coord_system ), 'RangedData' ), 'GRanges' )
                   hgvs <<- unlist( lapply( names( x$hgvs ), function( n ) {
                     r = x$hgvs[ n ]
                     names( r ) = n
                     r
                   } ) )
                   transcripts <<- lapply( x$transcripts, function( t ) {
                     getRefClass( 'EnsTranscript' )$new( t )
                   } )
                 }
               },
               show = function() {
                 'Method for automatically printing Ref'
                 if( length( name ) > 0 && nchar( name ) > 0 ) cat( paste( 'name        :', name ), '\n' )
                 if( length( is_somatic ) > 0 )                 cat( paste( 'is_somatic  :', is_somatic ), '\n' )
                 if( length( hgvs ) > 0 ) {
                   cat(                                                     'hgvs        :\n' )
                   for( h in names( hgvs ) ) {
                     cat( ' ', h, '=', hgvs[ h ], '\n' )
                   }
                 }
                 if( length( transcripts ) > 0 ) {
                   cat(                                                     'transcripts :', length( transcripts ), 'in total\n' )
                 }
               }
           ) )
