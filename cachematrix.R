## The two functions below are used to create a special object 
## that stores a sqaure invertible matrix and caches its inverse.

## NOTE: These functions assumes that the matrix supplied is 
##       ALWAYS invertible!!!

## The first function below creates a special "matrix" object
## that can cache its inverse. A list is created containing a 
## function to
## 1.  set the matrix
## 2.  get the matrix
## 3.  set the inverse of the matrix
## 4.  get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) m <<- inverse
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## This function computes the inverse of the special "matrix"
## returned by `makeCacheMatrix` above. If the inverse has
## already been calculated (and the matrix has not changed), 
## then `cacheSolve` should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        return(m)
}
