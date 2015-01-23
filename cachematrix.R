## The functions allow the user to create an object using makeCacheMatrix that enable a square invertible matrix to have its inverse
## stored in cache memory by assigning it to its own specific environment - that is, the environment of a variable created by calling makeCacheMatrix().
## Then using cacheSolve if the matrix has already been solved, the solved matrix is already located in memory and does not need to 
## be re-calculated - it is simply retrieved from memory.


## makeCacheMatrix takes a matrix and creates an environment with functions to GET the value of the matrix, re-SET the value of the matrix,
## GET the value of the inverted matrix (if the inversion calculation has already been performed), and to SET the value of the inverted matrix.
makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL 
        }
        get <- function() x
        setinverse <- function(inverse) i <<- inverse
        getinverse <- function() i
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

## cacheSolve takes a makeCacheMatrix object and first checks to see if inverted matrix value has already been computed and stored in the 
## environment of that object.  If it has already been computed and stored it simply retrieves the stored value.  If it has not been computed 
## and stored, it computes the inverted matrix value and stores the result in the environment of x - meaning the next time that value is
## required it can simply be retrieved instead of being recomputed.  It also returns the value of the inverted matrix.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
}
