## These function are used to create special types of matrices which can cache their inverse, and return the cached value when needed without calculation. 

## Function that takes a matrix as input and creates a special 'object' (data+functions) that can store the cached value of the inverse of the matrix, and have methods to set/get values.

makeCacheMatrix <- function(x = matrix()) {
        invmat <- NULL
        set <- function(y) {
                x <<- y
                invmat <<- NULL
        }
        get <- function() x
	setinverse <- function(inverse) invmat <<- inverse
    	getinverse <- function() invmat
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

## Function that returns the cached valued of the inverse of the nmatrix contained in the special object created by the function makeCacheMatrix,
## if the cached value is not yet present it will be calculated and stored.

cachesolve <- function(x, ...) {
        invmat <- x$getinverse()
        if(!is.null(invmat)) {
                message("getting cached data")
                return(invmat)
        }
        data <- x$get()
        invmat <- solve(data, ...)
        x$setinverse(invmat)
        invmat
}

