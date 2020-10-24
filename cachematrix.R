## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
## cacheSolve: This function computes the inverse of the special "matrix" returned by
## makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed),
## then the cachesolve should retrieve the inverse from the cache.

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
                inverse = NULL
                set = function(y){
                        x <<- y
                        inverse <<- NULL
                }
                get = function() x
                setinverse = function(solve) inverse <<- solve
                getinverse = function() inverse
                list(set = set, get = get, setinverse = setinverse, getinverse = getinverse
                    )
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
                A = makeCacheMatrix(x)
                i = A$getinverse()
                if(!is.null(i)){
                        message("getting cached data")
                        return(i)
                }
                else {
                data = A$get()
                i = solve(data, ...)
                A$setinverse(i)
                }
                i
        
                
        ## Return a matrix that is the inverse of 'x'
}
