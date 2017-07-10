## makeCacheMatrix creates an object wrapping a matrix and its inverse.
##   The object itself is a list of 4 {get, set} x { matrix, inverse } closures. i.e. functions and list of variables they
##   are accessing from parent lexical scopes.
##
##   The magic happens due to the super assignment operator which can modify variables in the parent levels. Unlike the
##   assignment operator which can only modify variables in the current levels.
##
## Here is how it works:
## - Consider the setInverse function:
##   - When it is constructed, it not only has access to its formal parameter 'i', but also to the variables in its
##     parent's (i.e. makeCacheMatrix) scope viz 'inverse'.
##   - The <<- operator used by setInverse, modifies the m value from the parent lexical scope.
##   - The setInverse function returned from makeCacheMatrix (3rd entry of the list), it is actually the closure i.e. the function
##     setInverse itself + and tagging along with it, the variables from its environment ('inverse' in this case).
##   - So when it is called, it modifies the 'inverse' variable 'tagging' along with it.
## - Now consider getInverse function:
##   - When it is called, it retrives value of 'inverse' that is tagging along with it which happens to be the same as the variable
##     tagging along with setInverse. This is because the 'inverse' variable is owned by makeCacheMatrix lexical scope,
##     the parent lexical scope of both getInverse and setInverse.
##   - In effect what setInverse writes into, can later be retrived by getInverse.
## - Same logic works for get and set function pairs.
##
## The cacheSolve function is comparitively simple - it simply checks if the value of inverse is already stored.
## - If yes, it simply returns it
## - If no, then it calculates the invese and stores it back into the matrix+inverse wrapper object using the logic described above.

## Creates an object wrapping a matrix & its inverse. The created object has r/w accessors for the matrix & its inverse.

makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    set <- function(val) {
        x <<- val
        inverse <<- NULL
    }
    get <- function() x
    setInverse <- function(i) inverse <<- i
    getInverse <- function() inverse
    list(set = set,
         get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## Given the object created by makeCacheMatrix, calculate the inverse of the matrix and cache it in the object itself.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inverse <- x$getInverse()
    if(!is.null(inverse)) {
        message("getting cached data")
        return(inverse)
    }
    m <- x$get()
    inverse <- solve(m)
    x$setInverse(inverse)
    inverse
}
