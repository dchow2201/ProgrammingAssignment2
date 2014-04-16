#This function creates a special "matrix" object 
#that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    #set the value of the matrix
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    #get the value of the matrix
    get <- function() x
    #set the value of the inverse
    setinverse <- function(inverse) inv<<- inverse
    #get the value of the inverse
    getinverse <- function() inv
    #return a list of functions
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)

}


#calculates the inverse of the special matrix
#created in the makeCacheMatrix function

cacheSolve <- function(x, ...) {
    #checks to see if the inverse has already been calculated
    inv <- x$getinverse()
    if(!is.null(inv)) {
        #if so it gets the inverse from cache and skips the computation
        message("getting cached data")
        return(inv)
    }
    #otherwise it calculates the inverse of the matrix and sets the value
    # of the inverse in the cache via the setinverse function
    data <- x$get()
    inv <- solve(data,...)
    x$setinverse(inv)
    inv
}
