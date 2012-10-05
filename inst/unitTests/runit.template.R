# This is a blank template for an RUnit test file
if( FALSE ) {
  library( "RUnit" )
  library( "EnsemblRest" )
}

# .setUp is called before each test method
.setUp = function() {}

# .tearDown is called after each test method
.tearDown = function() { }

# An example test
test.template = function() {
  checkTrue(   TRUE,       "True should be TRUE" )
  checkEquals( TRUE, TRUE, "And this should be ok too" )
}
