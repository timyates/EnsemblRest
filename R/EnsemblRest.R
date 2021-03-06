# This hash stores all properties, a link to the debug function (if it is turned
# on), and the format of the URLs for the REST queries
.Ensembl = new.env( hash=TRUE )

# This is the default debug function
.debug.none = function( message ) {
}

# This is used when setParam( debug=TRUE ) is called
.debug.full = function( message ) {
  cat( format( Sys.time(), "%a %b %d %X %Y" ), "::", message, "\n" )
}

# This converts between '1' and '-1' to '+', '-', or '*'
.strandString = function( strand ) {
  if( strand == '1' || strand == 1 ) '+'
  else if( strand == '-1' || strand == -1 ) '-'
  else '*'
}

# This converts a URL template, ie:
#    'homology/symbol/${species}/${symbol}'
# and a named vector, ie:
#    c( species='human', symbol='tp53' )
# to a character vector, ie:
#    'homology/symbol/human/tp53'
.build.url = function( template, vec ) {
  if( !missing( vec ) && !is.null( vec ) ) {
    for( var in names( vec ) ) {
      template = gsub( paste( "\\$\\{", var, "\\}", sep="" ), URLencode( vec[[ var ]] ), template )
    }
  }
  template
}

# Handle setting of params at runtime (esp. debug)
setParam = function( ... ) {
  .params = list( ... )
  for( .name in names( .params ) ) {
    .Ensembl[[ .name ]] = .params[ .name ][[1]]
  }
  if( !is.null( .Ensembl$debug ) && .Ensembl$debug ) {
    .Ensembl$debugFn = .debug.full
  }
  else {
    .Ensembl$debugFn = .debug.none
  }
}

# Retrieve a param
getParam = function( key ) {
  .Ensembl[[ key ]]
}

# Fetch the data from the REST service, throttling back to 3 queries per second
.load.and.parse = function( url, elements, params=c(), content_type=.Ensembl$json.content.type ) {
  if( !is.null( .Ensembl$last.query ) ) {
    sleep.time = as.numeric( Sys.time() - .Ensembl$last.query )
    if( sleep.time < 0.3 ) {
      Sys.sleep( sleep.time )
    }
  }
  params = c( params, content_type )
  .Ensembl$last.query = Sys.time()
  url = paste( paste( .Ensembl$url, .build.url( url, elements ), sep='' ),
               paste( params, collapse='&' ),
               sep='?' )
  .Ensembl$debugFn( paste( 'calling', url ) )
  result = getURL( url )
  .Ensembl$debugFn( paste( 'got', result ) )

  if( content_type == .Ensembl$json.content.type ) {
    result = suppressWarnings( fromJSON( result ) )
    if( !is.null( result$error ) ) {
      stop( result$error )
    }
  }
  result
}

# Convert a named vector to a list of 'name=value'
.make.params = function( ... ) {
  .params = list( ... )
  f = function( name ) {
    paste( name, .params[[ name ]], sep='=' )
  }
  unlist( lapply( names( .params ), f ) )
}

.initialise = function( use.cache=TRUE ) {
  # Root URL
  .Ensembl$url = 'http://beta.rest.ensembl.org/'

  .Ensembl$last.query = NULL

  # Comparative Genomics
  .Ensembl$genetree = 'genetree/id/${id}'
  .Ensembl$genetree.member = 'genetree/member/id/${id}'
  .Ensembl$genetree.symbol = 'genetree/member/symbol/${species}/${symbol}'
  .Ensembl$homology.id = 'homology/id/${id}'
  .Ensembl$homology.symbol = 'homology/symbol/${species}/${symbol}'

  # Cross References
  .Ensembl$xrefs = 'xrefs/id/${id}'
  .Ensembl$xrefs.name = 'xrefs/name/${species}/${name}'
  .Ensembl$xrefs.symbol = 'xrefs/symbol/${species}/${symbol}'

  .Ensembl$feature = 'feature/region/${species}/${region}'
  .Ensembl$featureId = 'feature/id/${id}'

  # Information
  .Ensembl$assembly = 'assembly/info/${species}'
  .Ensembl$assembly.region = 'assembly/info/${species}/${region_name}'
  .Ensembl$info.comparas = 'info/comparas'
  .Ensembl$info.data = 'info/data'
  .Ensembl$info.ping = 'info/ping'
  .Ensembl$info.rest = 'info/rest'
  .Ensembl$info.software = 'info/software'
  .Ensembl$info.species = 'info/species'

  # Lookup
  .Ensembl$lookup = 'lookup/id/${id}'

  # Mapping
  .Ensembl$mapping = 'map/${species}/${asm_one}/${region}/${asm_two}'
  .Ensembl$mapping.cdna = 'map/cdna/${id}/${region}'
  .Ensembl$mapping.cds = 'map/cds/${id}/${region}'
  .Ensembl$mapping.translation = 'map/translation/${id}/${region}'

  # Ontologies
  .Ensembl$ontology.id = 'ontology/id/${id}'
  .Ensembl$ontology.name = 'ontology/name/${name}'
  .Ensembl$ontology.ancestors = 'ontology/ancestors/${id}'
  .Ensembl$ontology.ancestors.chart = 'ontology/ancestors/chart/${id}'
  .Ensembl$ontology.descendents = 'ontology/descendents/${id}'

  # Taxonomies
  .Ensembl$taxonomy.id = 'taxonomy/id/${id}'
  .Ensembl$taxonomy.classification = 'taxonomy/classification/${id}'

  # Sequences
  .Ensembl$sequence.id = 'sequence/id/${id}'
  .Ensembl$sequence.region = 'sequence/region/${species}/${region}'

  # Variation
  .Ensembl$variation = 'vep/${species}/${region}/${allele}/consequences'
  .Ensembl$variation.id = 'vep/${species}/id/${id}/consequences'

  .Ensembl$json.content.type = c( 'content-type=application/json' )
  .Ensembl$debug = FALSE
  .Ensembl$debugFn = .debug.none
}