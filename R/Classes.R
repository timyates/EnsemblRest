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
               karyotype='character',
               assembly_name='character',
               top_level_region='data.frame',
               assembly_date='character',
               coord_system_versions='character',
               genebuild_start_date='character',
               genebuild_initial_release_date='character',
               genebuild_last_geneset_update='character',
               genebuild_method='character',
               default_coord_system_version='character'
             ),
             methods = list(
               initialize = function( x=NULL, ... ) {
                 'Initialize an Assembly info reference from data returned by rjson'
                 .Ensembl$debugFn( str( x ) )
                 if( !is.null( x ) ) {
                   assembly_name                  <<- x$assembly_name
                   top_level_region               <<- rbind.fill( lapply( x$top_level_region, function( a ) {
                     data.frame( a[ !sapply( a, is.null ) ], stringsAsFactors=F )
                   } ) )
                   assembly_date                  <<- x$assembly_date
                   coord_system_versions          <<- x$coord_system_versions
                   genebuild_start_date           <<- x$genebuild_start_date
                   genebuild_initial_release_date <<- x$genebuild_initial_release_date
                   genebuild_last_geneset_update  <<- x$genebuild_last_geneset_update
                   genebuild_method               <<- x$genebuild_method
                   default_coord_system_version   <<- x$default_coord_system_version
                 }
               },
               show = function() {
                 'Method for automatically printing Ref'
                 if( length( karyotype ) > 0 )                      cat( paste( 'karyotype                      :', karyotype ), '\n' )
                 if( length( assembly_name ) > 0 )                  cat( paste( 'assembly_name                  :', assembly_name ), '\n' )
                 if( length( assembly_date ) > 0 )                  cat( paste( 'assembly_date                  :', assembly_date ), '\n' )
                 if( length( coord_system_versions ) > 0 )          cat( paste( 'coord_system_versions          :', paste( coord_system_versions, collapse=', ' ) ), '\n' )
                 if( length( genebuild_start_date ) > 0 )           cat( paste( 'genebuild_start_date           :', genebuild_start_date ), '\n' )
                 if( length( genebuild_initial_release_date ) > 0 ) cat( paste( 'genebuild_initial_release_date :', genebuild_initial_release_date ), '\n' )
                 if( length( genebuild_last_geneset_update ) > 0 )  cat( paste( 'genebuild_last_geneset_update  :', genebuild_last_geneset_update ), '\n' )
                 if( length( genebuild_method ) > 0 )               cat( paste( 'genebuild_method               :', genebuild_method ), '\n' )
                 if( length( default_coord_system_version ) > 0 )   cat( paste( 'default_coord_system_version   :', default_coord_system_version ), '\n' )
                                                                    cat( paste( 'top_level_region count         :', dim( top_level_region )[1] ), '\n' )
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
               release='numeric'
             ),
             methods = list(
               initialize = function( x=NULL, ... ) {
                 'Initialize an Species reference from data returned by rjson'
                 if( !is.null( x ) ) {
                   name  <<- x$name
                   ali = unlist( x$aliases )
                   aliases <<- if( is.null( ali ) ) '' else ali
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

setRefClass( "EnsOntology",
             fields=list(
               ontology='character',
               accession='character',
               definition='character',
               name='character',
               namespace='character',
               subsets='character',
               synonyms='character',
               children='list',
               parents='list'
             ),
             methods = list(
               initialize = function( x=NULL, ... ) {
                 'Initialize an Ontology reference from data returned by rjson'
                 if( !is.null( x ) ) {
                   ontology   <<- if( is.null( x$ontology ) )           '' else x$ontology
                   accession  <<- if( is.null( x$accession ) )          '' else x$accession
                   definition <<- if( is.null( x$definition ) )         '' else x$definition
                   name       <<- if( is.null( x$name ) )               '' else x$name
                   namespace  <<- if( is.null( x$namespace ) )          '' else x$namespace
                   subsets    <<- if( is.null( unlist( x$subsets ) ) )  '' else unlist( x$subsets )
                   synonyms   <<- if( is.null( unlist( x$synonyms ) ) ) '' else unlist( x$synonyms )

                   if( !is.null( x$children ) ) {
                     children <<- lapply( x$children, function( c ) {
                       getRefClass( 'EnsOntology' )$new( c )
                     } )
                   }
                   if( !is.null( x$parents ) ) {
                     parents <<- lapply( x$parents, function( p ) {
                       getRefClass( 'EnsOntology' )$new( p )
                     } )
                   }
                 }
               },
               show = function() {
                 'Method for automatically printing Ref'
                 if( length( ontology ) > 0   && max( nchar( ontology   ) ) > 0 ) cat( 'ontology   :', ontology, '\n' )
                 if( length( accession ) > 0  && max( nchar( accession  ) ) > 0 ) cat( 'accession  :', accession, '\n' )
                 if( length( definition ) > 0 && max( nchar( definition ) ) > 0 ) cat( 'definition :', definition, '\n' )
                 if( length( name ) > 0       && max( nchar( name       ) ) > 0 ) cat( 'name       :', name, '\n' )
                 if( length( namespace ) > 0  && max( nchar( namespace  ) ) > 0 ) cat( 'namespace  :', namespace, '\n' )
                 if( length( subsets ) > 0    && max( nchar( subsets    ) ) > 0 ) cat( 'subsets    :', paste( subsets, collapse=', ' ), '\n' )
                 if( length( synonyms ) > 0   && max( nchar( synonyms   ) ) > 0 ) cat( 'synonyms   :', paste( synonyms, collapse=', ' ), '\n' )
                 if( length( parents ) > 0 ) {
                   cat( 'parents    : ', length( parents ), 'in total\n' )
                 }
                 if( length( children ) > 0 ) {
                   cat( 'children   : ', length( children ), 'in total\n' )
                 }
               }
           ) )

setRefClass( "EnsTaxonomy",
             fields=list(
               id='character',
               name='character',
               scientific_name='character',
               leaf='logical',
               tags='list',
               parent='ANY',  # Will be EnsTaxonomy or NULL
               children='list'
             ),
             methods = list(
               initialize = function( x=NULL, ... ) {
                 'Initialize an Ontology reference from data returned by rjson'
                 if( !is.null( x ) ) {
                   scientific_name <<- if( is.null( x$scientific_name ) ) ''     else x$scientific_name
                   parent          <<- if( is.null( x$parent ) )          NULL   else getRefClass( 'EnsTaxonomy' )$new( x$parent )
                   name            <<- if( is.null( x$name ) )            ''     else x$name
                   id              <<- if( is.null( x$id ) )              ''     else x$id
                   leaf            <<- if( is.null( x$leaf ) )            FALSE  else x$leaf == 1
                   tags            <<- if( is.null( x$tags ) ) {          list() }
                   else {
                     tags <<- x$tags
                   }

                   if( !is.null( x$children ) ) {
                     children <<- lapply( seq_along( x$children ), function( idx ) {
                       getRefClass( 'EnsTaxonomy' )$new( x$children[[ idx ]] )
                     } )
                   }
                 }
               },
               show = function() {
                 'Method for automatically printing Ref'
                 if( length( id ) > 0   && max( nchar( id   ) ) > 0 )                       cat( 'id              : ', id, '\n' )
                 if( length( name ) > 0  && max( nchar( name  ) ) > 0 )                     cat( 'name            : ', name, '\n' )
                 if( length( scientific_name ) > 0 && max( nchar( scientific_name ) ) > 0 ) cat( 'scientific_name : ', scientific_name, '\n' )
                 cat(                                                                            'leaf            : ', leaf, '\n' )
                 if( length( tags ) > 0 ) {
                   cat(                                                                          'tags            :\n' )
                   for( h in names( tags ) ) {
                     cat( ' ', h, '=', paste( unlist( tags[ h ] ), collapse=', ' ), '\n' )
                   }
                 }
                 if( length( children ) > 0 ) {
                   cat(                                                                          'children        : ', length( children ), 'in total\n' )
                 }
                 if( !is.null( parent ) ) {
                   cat(                                                                          'parent          : EXISTS' )
                 }
               }
           ) )
