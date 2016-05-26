## These functions calculate and cache the inverse of a matrix



	## Function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {

        s <- NULL
        set <- function(y) {
                x <<- y
                s <<- NULL
        }
        get <- function() x
        setresult <- function(solve) s <<- solve
        getresult <- function() s
        list(set = set, get = get, setresult = setresult,getresult = getresult)
}


	## Function computes the inverse of the special "matrix" returned by 
	## makeCacheMatrix above. If the inverse has already been calculated 
	## (and the matrix has not changed), then the cachesolve will retrieve 
	## the inverse from the cache.

cacheSolve <- function(x, ...) {

        s <- x$getresult()
        if(!is.null(s)) {
                message("getting cached data")
                return(s)
        }
        data <- x$get()
        s <- solve(data, ...)
        x$setresult(s)
        s
        ## Return a matrix that is the inverse of 'x'
}
