## Makes a cached version of the inverse which is really a list.
## The list contains the matrix and its inverse.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setmatrix <- function(solve) m <<- solve
        getmatrix <- function() m
        list(set = set, 
             get = get, 
             setmatrix = setmatrix, 
             getmatrix = getmatrix)
}


## Goes through the actual computation of the inverse.
## First it will check to see if the computation is already complete.
## If so, the cached inverse will be returned.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getmatrix()
        if(!is.null(m)) {
                message("Retrieving cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data,...)
        x$setmatrix(m)
        m
}
