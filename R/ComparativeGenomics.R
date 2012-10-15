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
                     phyloxml_aligned=FALSE  ) {
  nh_format = match.arg( nh_format )
  params = .make.params( nh_format=nh_format )
  if( phyloxml_aligned ) {
    params = c( params, 'phyloxml_aligned=True' )
  }
  .load.and.parse( .Ensembl$genetree, c( id=gid ), params, 'content-type=text/nh' )
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
  getRefClass( 'EnsHomologyResponse' )$new( .load.and.parse( .Ensembl$homology.id, c( id=id ), params )$data )
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
  getRefClass( 'EnsHomologyResponse' )$new( .load.and.parse( .Ensembl$homology.symbol, c( species=species, symbol=symbol ), params )$data )
}