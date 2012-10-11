# Classes
setRefClass( "EnsHomologyDetail",
             fields=list( 
               perc_pos="numeric",
               perc_id="numeric",
               protein_id="character",
               align_seq="character",
               id="character",
               species="character",
               cigar_line="character" ),
             methods = list(
               initialize = function( x=NULL, ... ) {
                 'Initialize a HomologyDetail from data returned by rjson'
                 if( !is.null( x ) ) {
                   perc_pos   <<- as.numeric( x$perc_pos )
                   perc_id    <<- as.numeric( x$perc_id )
                   protein_id <<- x$protein_id
                   align_seq  <<- x$align_seq
                   id         <<- x$id
                   species    <<- x$species
                   cigar_line <<- x$cigar_line
                 }
               },
               show = function( prefix='' ) {
                 'Method for automatically printing HomologyDetail'
                 cat( prefix, 'Homology Detail of class ', classLabel( class( .self ) ), '\n' )
                 prefix = paste( prefix, '|' )
                 cat( prefix, '--         id: ', id, '\n' )
                 cat( prefix, '--    species: ', species, '\n' )
                 cat( prefix, '-- protein_id: ', protein_id, '\n' )
                 cat( prefix, '--   perc_pos: ', perc_pos, '\n' )
                 cat( prefix, '--    perc_id: ', perc_id, '\n' )
                 cat( prefix, '-- cigar_line: ', cigar_line, '\n' )
                 cat( prefix, '--  align_seq: ', align_seq, '\n' )
               }
             ) )

setRefClass( "EnsHomology",
             fields=list( 
               dn_ds='numeric',
               type='character',
               subtype='character',
               source='EnsHomologyDetail',
               target='EnsHomologyDetail'
             ),
             methods = list(
               initialize = function( x=NULL, ... ) {
                 'Initialize a Homology from data returned by rjson'
                 if( !is.null( x ) ) {
                   dn_ds   <<- as.numeric( x$dn_ds )
                   type    <<- x$type
                   subtype <<- x$subtype
                   source  <<- getRefClass( 'EnsHomologyDetail' )$new( x$source, ... )
                   target  <<- getRefClass( 'EnsHomologyDetail' )$new( x$target, ... )
                 }
               },
               show = function( prefix='' ) {
                 'Method for automatically printing Homology'
                 cat( prefix, 'Homology of class ', classLabel( class( .self ) ), '\n' )
                 prefix = paste( prefix, '|' )
                 cat( prefix, '--   dn_ds: ', dn_ds, '\n' )
                 cat( prefix, '--    type: ', type, '\n' )
                 cat( prefix, '-- subtype: ', subtype, '\n' )
                 if( length( source ) > 0 ) {
                   cat( prefix, '--  source:\n' )
                   cat( prefix, source$show( paste( prefix, ' ' ) ), '\n' )
                 }
                 if( length( target ) > 0 ) {
                   cat( prefix, '--  target:\n' )
                   cat( prefix, target$show( paste( prefix, ' ' ) ), '\n' )
                 }
               }
           ) )

setRefClass( "EnsHomologyResponse",
            fields=list(
              id='character',
              homologies='list'
            ),
            methods = list(
               initialize = function( x=NULL, ... ) {
                 'Initialize a Homology Response from data returned by rjson'
                 if( !is.null( x ) ) {
                   id <<- x[[1]]$id
                   homologies <<- lapply( x[[1]]$homologies, function( a ) {
                     getRefClass( 'EnsHomology' )$new( a, ... )
                   } )
                 }
               },
               show = function( prefix='' ) {
                 'Method for automatically printing Homology'
                 cat( prefix, 'Homology Response of class ', classLabel( class( .self ) ), '\n' )
                 prefix = paste( prefix, '|' )
                 cat( prefix, '--     id: ', id, '\n' )
                 cat( prefix, '-- homologies: (length', length( homologies ), ')\n' )
                 for( x in seq_along( homologies ) ) {
                   cat( prefix, '[[', x, ']]\n' )
                   homologies[[ x ]]$show( paste( prefix, ' ' ) )
                 }
               }
            ) )

setRefClass( "EnsRef",
             fields=list( 
               id='character',
               type='character'
             ),
             methods = list(
               initialize = function( x=NULL, ... ) {
                 'Initialize an internal Ensembl reference from data returned by rjson'
                 if( !is.null( x ) ) {
                   id   <<- x$id
                   type <<- x$type
                 }
               },
               show = function( prefix='' ) {
                 'Method for automatically printing Ref'
                 cat( prefix, 'Ref of class ', classLabel( class( .self ) ), '\n' )
                 prefix = paste( prefix, '|' )
                 if( length( id ) > 0 )   cat( prefix, '--   id: ', id, '\n' )
                 if( length( type ) > 0 ) cat( prefix, '-- type: ', type, '\n' )
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
               show = function( prefix='' ) {
                 'Method for automatically printing Ref'
                 cat( prefix, 'Ref of class ', classLabel( class( .self ) ), '\n' )
                 prefix = paste( prefix, '|' )
                 if( length( assembly_name ) > 0 )                  cat( prefix, '--                  assembly_name: ', assembly_name, '\n' )
                 if( length( assembly_date ) > 0 )                  cat( prefix, '--                  assembly_date: ', assembly_date, '\n' )
                 if( length( coord_system_versions ) > 0 )          cat( prefix, '--          coord_system_versions: ', coord_system_versions, '\n' )
                 if( length( schema_build ) > 0 )                   cat( prefix, '--                   schema_build: ', schema_build, '\n' )
                 if( length( genebuild_start_date ) > 0 )           cat( prefix, '--           genebuild_start_date: ', genebuild_start_date, '\n' )
                 if( length( genebuild_initial_release_date ) > 0 ) cat( prefix, '-- genebuild_initial_release_date: ', genebuild_initial_release_date, '\n' )
                 if( length( genebuild_last_geneset_update ) > 0 )  cat( prefix, '--  genebuild_last_geneset_update: ', genebuild_last_geneset_update, '\n' )
                 if( length( genebuild_method ) > 0 )               cat( prefix, '--               genebuild_method: ', genebuild_method, '\n' )
                 if( length( top_level_seq_region_names ) > 0 )     cat( prefix, '--     top_level_seq_region_names: ', top_level_seq_region_names, '\n' )
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
               show = function( prefix='' ) {
                 'Method for automatically printing Ref'
                 cat( prefix, 'Ref of class ', classLabel( class( .self ) ), '\n' )
                 prefix = paste( prefix, '|' )
                 if( length( is_chromosome ) > 0 )           cat( prefix, '--           is_chromosome: ', is_chromosome, '\n' )
                 if( length( length ) > 0 )                  cat( prefix, '--                  length: ', length, '\n' )
                 if( length( assembly_exception_type ) > 0 ) cat( prefix, '-- assembly_exception_type: ', assembly_exception_type, '\n' )
                 if( length( coordinate_system ) > 0 )       cat( prefix, '--       coordinate_system: ', coordinate_system, '\n' )
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
               show = function( prefix='' ) {
                 'Method for automatically printing Ref'
                 cat( prefix, 'Ref of class ', classLabel( class( .self ) ), '\n' )
                 prefix = paste( prefix, '|' )
                 if( length( name ) > 0 )    cat( prefix, '--    name: ', name, '\n' )
                 if( length( aliases ) > 0 ) cat( prefix, '-- aliases: ', aliases, '\n' )
                 if( length( groups ) > 0 )  cat( prefix, '--  groups: ', groups, '\n' )
                 if( length( release ) > 0 ) cat( prefix, '-- release: ', release, '\n' )
               }
           ) )

setRefClass( "EnsTranscript",
             fields=list(
               protein_features='character',
               alleles='data.frame',
               data='data.frame'
             ),
             methods = list(
               initialize = function( x=NULL, ... ) {
                 'Initialize an Species reference from data returned by rjson'
                 if( !is.null( x ) ) {
                   data <<- data.frame( name=if( is.null( x$name ) ) NA else x$name,
                                        gene_id=if( is.null( x$gene_id ) ) NA else x$gene_id,
                                        transcript_id=if( is.null( x$transcript_id ) ) NA else x$transcript_id,
                                        biotype=if( is.null( x$biotype ) ) NA else x$biotype,
                                        ccds=if( is.null( x$ccds ) ) NA else x$ccds,
                                        cdna_allele_string=if( is.null( x$cdna_allele_string ) ) NA else x$cdna_allele_string,
                                        codon_position=if( is.null( x$codon_position ) ) NA else x$codon_position,
                                        translation_stable_id=if( is.null( x$translation_stable_id ) ) NA else x$translation_stable_id,
                                        translation_start=if( is.null( x$translation_start ) ) NA else as.numeric( x$translation_start ),
                                        translation_end=if( is.null( x$translation_end ) ) NA else as.numeric( x$translation_end ),
                                        exon_number=if( is.null( x$exon_number ) ) NA else x$exon_number,
                                        intron_number=if( is.null( x$intron_number ) ) NA else x$intron_number,
                                        cdna_start=if( is.null( x$cdna_start ) ) NA else as.numeric( x$cdna_start ),
                                        cdna_end=if( is.null( x$cdna_end ) ) NA else as.numeric( x$cdna_end ),
                                        cds_start=if( is.null( x$cds_start ) ) NA else as.numeric( x$cds_start ),
                                        cds_end=if( is.null( x$cds_end ) ) NA else as.numeric( x$cds_end ),
                                        is_canonical=if( is.null( x$is_canonical ) ) NA else x$is_canonical == 1 )
                   if( !is.null( x$protein_features ) ) {
                     protein_features <<- unlist( lapply( x$protein_features, function( p ) {
                       r = p$name
                       names( r ) = p$db
                       r
                     } ) )
                   }
                   alleles <<- do.call( 'rbind', lapply( names( x$alleles ), function( a ) {
                     j = x$alleles[[ a ]]
                     data.frame( name=a,
                                 display_codon_allele_string=if( is.null( j$display_codon_allele_string ) ) NA else j$display_codon_allele_string,
                                 pep_allele_string=if( is.null( j$pep_allele_string ) ) NA else j$pep_allele_string,
                                 codon_allele_string=if( is.null( j$codon_allele_string ) ) NA else j$codon_allele_string,
                                 hgvs_transcript=if( is.null( j$hgvs_transcript ) ) NA else j$hgvs_transcript,
                                 hgvs_protein=if( is.null( j$hgvs_protein ) ) NA else j$hgvs_protein,
                                 polyphen_score=if( is.null( j$polyphen_score ) ) NA else as.numeric( j$polyphen_score ),
                                 polyphen_prediction=if( is.null( j$polyphen_prediction ) ) NA else j$polyphen_prediction,
                                 sift_score=if( is.null( j$sift_score ) ) NA else as.numeric( j$sift_score ),
                                 sift_prediction=if( is.null( j$sift_prediction ) ) NA else j$sift_prediction,
                                 consequence_terms=if( is.null( j$consequence_terms ) ) NA else j$consequence_terms )
                   } ) )
                 }
               },
               show = function( prefix='' ) {
                 'Method for automatically printing Ref'
                 cat( prefix, 'Ref of class ', classLabel( class( .self ) ), '\n' )
                 prefix = paste( prefix, '|' )
                 if( length( protein_features ) > 0 ) {
                   cat( prefix, '-- protein_features:\n' )
                   print( protein_features )
                 }
                 if( dim( alleles )[1] > 0 ) {
                   cat( prefix, '-- alleles:\n' )
                   print( alleles )
                 }
                 if( dim( data )[1] > 0 ) {
                   cat( prefix, '-- data:\n' )
                   print( data )
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
                                                    strand=if( x$location$strand == '1' ) '+' else if( x$location$strand == '-1' ) '-' else '*',
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
               show = function( prefix='' ) {
                 'Method for automatically printing Ref'
                 cat( prefix, 'Ref of class ', classLabel( class( .self ) ), '\n' )
                 prefix = paste( prefix, '|' )
                 if( length( name ) > 0 )       cat( prefix, '--       name: ', name, '\n' )
                 if( length( is_somatic ) > 0 ) cat( prefix, '-- is_somatic: ', is_somatic, '\n' )
                 if( length( hgvs ) > 0 ) {
                   cat( prefix, '-- hgvs:\n' )
                   print( hgvs )
                 }
                 if( length( transcripts ) > 0 ) {
                   cat( prefix, '-- transcripts:\n' )
                   print( transcripts )
                 }
               }
           ) )
