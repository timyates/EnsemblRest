.Ensembl = new.env( hash=TRUE )

.debug.none = function( message ) {
}

.debug.full = function( message ) {
  cat( format( Sys.time(), "%a %b %d %X %Y" ), "::", message, "\n" )
}

.strandString = function( strand ) {
  if( strand == '1' || strand == 1 ) '+'
  else if( strand == '-1' || strand == -1 ) '-'
  else '*'
}

.build.url = function( template, hash ) {
  if( !is.null( hash ) ) {
    vars = ls( envir=hash )
    for( var in vars ) {
      val = get( var, envir=hash )
      patt = paste( "\\$\\{", var, "\\}", sep="" )
      template = gsub( patt, val, template )
    }
  }
  template
}

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

getParam = function( key ) {
  .Ensembl[[ key ]]
}

.load.and.parse = function( elements, params=c(), content_type=.Ensembl$json.content.type ) {
  if( !is.null( .Ensembl$last.query ) ) {
    sleep.time = as.numeric( Sys.time() - .Ensembl$last.query )
    if( sleep.time < 0.3 ) {
      Sys.sleep( sleep.time )
    }
  }
  params = c( params, content_type )
  .Ensembl$last.query = Sys.time()
  url = paste( paste( .Ensembl$url, paste( elements, collapse='/' ), sep='' ),
               paste( params, collapse='&' ),
               sep='?' )
  .Ensembl$debugFn( paste( 'calling', url ) )
  result = getURL( url )

  if( content_type == .Ensembl$json.content.type ) {
    result = suppressWarnings( fromJSON( result ) )
    if( !is.null( result$error ) ) {
      stop( result$error )
    }
  }
  result
}

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

  # Function URL part
  .Ensembl$genetree = 'genetree/id'
  .Ensembl$homology.id = 'homology/id'
  .Ensembl$homology.symbol = 'homology/symbol'
  .Ensembl$xrefs = 'xrefs/id'
  .Ensembl$xrefs.name = 'xrefs/name'
  .Ensembl$xrefs.symbol = 'xrefs/symbol'
  .Ensembl$assembly = 'assembly/info'
  .Ensembl$info.comparas = 'info/comparas'
  .Ensembl$info.data = 'info/data'
  .Ensembl$info.ping = 'info/ping'
  .Ensembl$info.rest = 'info/rest'
  .Ensembl$info.software = 'info/software'
  .Ensembl$info.species = 'info/species'
  .Ensembl$lookup = 'lookup'
  .Ensembl$mapping = 'map'
  .Ensembl$mapping.cdna = 'map/cdna'
  .Ensembl$mapping.cds = 'map/cds'
  .Ensembl$mapping.translation = 'map/translation'
  .Ensembl$sequence.id = 'sequence/id'
  .Ensembl$sequence.region = 'sequence/region'
  .Ensembl$variation = 'vep'
  .Ensembl$variation.id = 'id'
  .Ensembl$variation.tail = 'consequences'
  
  .Ensembl$json.content.type = c( 'content-type=application/json' )
  .Ensembl$debug = FALSE
  .Ensembl$debugFn = .debug.none
}