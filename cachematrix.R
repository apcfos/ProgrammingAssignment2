## Caching the Inverse of a Matrix

## Matrix inversion is usually a costly computation and there may be some benefit to caching the inverse of a matrix rather than computing it repeatedly
## makeCacheMatrix: This function creates a special matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) 
{
        inv <- NULL
        set <- function(y) 
	{
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinversa <- function(inversa) inv <<- inversa
        getinversa <- function() inv
        list(set = set, get = get, setinversa = setinversa, getinversa = getinversa)
}


## cacheSolve: This function computes the inverse of the special matrix returned by makeCacheMatrix
# If the inverse has already been calculated (and the matrix has not changed), then cacheSolve should retrieve the inverse from the cache

cacheSolve <- function(x, ...) 
{

## Return a matrix that is the inverse of 'x'

	inv <- x$getinversa()
        if (!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        matriz <- x$get()
        inv <- solve(matriz, ...)
        x$setinversa(inv)
        inv
}