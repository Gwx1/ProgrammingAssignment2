makeCacheMatrix <- function(x = matrix()) {                      ## by using the makeCacheMatrix-function, you have to specify a matrix first
        inv <- NULL                                              ## the function first set the inverse of the matrix (inv) automatically to NULL
        set <- function(y) {                                     ## by using the set-function you can overwrite/rewrite the specified matrix from above
                x <<- y                                          ## here the original matrix is overwritten by the new matrix y
                inv <<- NULL                                     ## set the inverse of the matrix in the parent environment to NULL 
        }      
        get <- function() x                                      ## the get-function allows to get the specified matrix from above
        setinverse <- function(solve) inv <<- solve              ## the setinverse-function allows to overwrite the value of inv in the parent environment 
        getinverse <- function() inv                             ## the getinverse-function returns the inversed matrix, which is saved in the parent environment 
        list(set = set, get = get,                               ## this is the output of the function: the results of the functions in the makeCacheMatrix-function
             setinverse = setinverse,                            ## are printed via list - so you can specifify the output to get only the 
             getinverse = getinverse)                            ## get-function (with x$get) or the setinverse-function (with x$setinverse) etc. 
        
        
}



cacheSolve <- function(x, ...) {                                 ## To inverse the original matrix, you have to specify symbol of the matrix
        inv <- x$getinverse()                                    ## first, this function overwrite the inv-function with the value of x$getinverse 
        if(!is.null(inv)) {                                      ## if the value is not NULL (data is already stored), 
                message("getting cached data")                   ## the message "getting cached data" and
                return(inv)                                      ## the value of inv (that is already stored) is returned from the parent environment
        } else {
                data <- x$get()                                  ## if the condition above is not true, than the function will
                inv <- solve(data, ...)                          ## inverse the data
                x$setinverse(inv) }                              ## saving the inversed matrix with the setinverse-function (from above) in the parent environment
                print(inv)                                       ## Return a matrix that is the inverse of 'x'
}
