## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
        
        ## create a matrix same size as matrix x, called matinv fill matinv with NA's
        colrow <-ncol(x)
        matinv <- matrix(data=NA,nrow=colrow,ncol=colrow)              
        
        ## Create the four desired functions to se and get the matrix an set and get the inverse
        set <- function(y) {
                x <<- y
                matinv <<- matrix(data=NA,nrow=colrow,ncol=colrow)
        }
        get <-function() x
        setinverse <- function(solve) matinv <<- solve
        getinverse <- function(matinv)
        ## Store the desired functions in a list and return the list to the calling function
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

## Return a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) {
        ## get the stored inverted matrix and read it into matinv
        matinv <- getinverse(x)
        ## if there is a stored inverted matrix, i.e. matinv is not filled with NA's, return matinv
        if(!is.NA(matinv)) {
                message("getting cached inverse of matrix")
                return(matinv)
        }
        ## Otherwise perform a solve on the new data
        data <- get()
        matinv <- solve(data, ...)
        setsolve(matinv)
        matinv
}
