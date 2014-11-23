## makeCacheMatrix() creates a list of 4 functions which either store (set) using the 
## super assignment <<- operator and retrieves (get) a matrix and it's inverse
## (once calculated and stored). cacheSolve() then uses these functions to calculate,
## store and the retrieve the inverse of the defined matrix.

makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL
        ## clears any value assigned as inverse
        set <- function(y) {
                x <<- y
                inverse <<- NULL
        }
        ## superassigns as x the matrix (y) and the empty inverse
        get <- function() x
        ## retrieve x which was superassigned as the original matrix
        setinverse <- function(solve) inverse <<- solve
        ## takes the calculated inverse from cacheSolve() and superassigns it as inverse
        ## which was originally null
        getinverse <- function() inverse
        ## retrieves the stored value of inverse which is either NULL or previously
        ## calculated by cacheSolve() and stored using setinverse()
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
        ## creates the list
}

cacheSolve <- function(x, ...) {
        inverse <- x$getinverse()
        ## uses getinverse() defined in makeCacheMatrix() to return the inverted matrix
        if(!is.null(inverse)) {
                message("getting cached data")
                return(inverse)
        }
        ## if this isn't the first time the function has been run then setinverse() would have
        ## already been used and a value stored as inverse (not null). Prints this value. Otherwise...
        data <- x$get()
        ## uses get() to retieve the matrix
        inverse <- solve(data, ...)
        ## calculates the inverse
        x$setinverse(inverse)
        ## then uses setinverse() to store this calculated value...
        inverse
        ## prints the result
}