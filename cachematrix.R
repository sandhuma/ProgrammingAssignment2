## Put comments here that give an overall description of what your
## functions do

## This function takes a matrix as an input and returns a list of 4 functions
## The 4 functions are to set and get the input matrix data and the inverse of the input matrix

makeCacheMatrix = function(mat = matrix()) {
    
    inv = NULL
    
    ## This function with set the input given to the mat variable and reset the inverse to null
    set <- function(input) {
        mat <<- input
        inv <<- NULL
    }
    
    ## Returns the input matrix
    get <- function() mat
    
    ## Set the setinv with the result of the inverse of the matrix
    setinv <- function(result) inv <<- result
    
    ## retuns the value of inv
    getinv <- function() inv
    
    ## Returns a list of function to ## The 4 functions to set and get the input matrix data and the inverse of the input matrix
    list(myset = set, 
         myget = get,
         mysetinv = setinv,
         mygetinv = getinv)
    
}


## This function computes the inv of a matrix if not already computed and put it in inv variable
## If it was already computed, then it will fetch the results from the variable inv
cacheSolve <- function(i, ...) {
        
    ## Return the inv matrix
    inv <- i$mygetinv()
    
    ## If not null, then display the inverse
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    
    ## Else, get the data and compute its inverse
    mat <- i$myget()
    result <- solve(mat)
    
    ##Set the inv in result and return result
    i$mysetinv(result)
    result
}


a = makeCacheMatrix(matrix(1:4,2,2))
cacheSolve(a)

