## This function creates a special "matrix" object 
## that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {            ## Function to set the values
                x <<- y
                inv <<- NULL
        }
        get <- function() x  ## Returns the original matrix
        setInverse <- function(inverse) inv <<- inverse  
        getInverse <- function() inv
        list(set = set,
             get = get,
             setInverse = setInverse,
             getInverse = getInverse)    
}

## This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getInverse()
        if (!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        mat <- x$get()
        inv <- solve(mat, ...)  ## Creates the Inverse of the matrix
        x$setInverse(inv)     
        inv                     ## Last line of function so return invere when function is called
}
