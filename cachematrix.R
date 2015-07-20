##/*-----------------------------------------------------------------*\
##|                                                                   |
##|        Function makeCacheMatrix                                   |
##|                                                                   |
##|  It takes an invertible square matrix argument, and returns a list|
##| of four accesor functions, "set", "get", "setinv" and "getinv"    |
##| , allowing protected storage and retrieval of both the matrix     |
##| and its inverse, as calculated by the solve() function, so        |
##| the lengthy solve calculation has to be performed only once,      |
##| and the inverse is stored in the function environement, until     |
##| the matrix is changed, or this function is rerun.                 |
##|                                                                   |
##\*-----------------------------------------------------------------*/
makeCacheMatrix <- function(x = matrix()) {
    mat_inv <- NULL
    set <- function(y){     ## stores a new matrix to be inverted
        x <<- y
        mat_inv <<- NULL       
    }
    get <- function() x     ## returns the stored matrix
    setinv <- function(inv) mat_inv <<- inv ## stores solved inverse
    getinv <- function() mat_inv         ## retrieves solved inverse
    list(set = set, get = get, setinv = setinv, getinv = getinv)
    ## returns a list object with the four accesor functions
}

##/*-----------------------------------------------------------*\
##|                                                             |
##|        function cacheSolve                                  |
##|                                                             |
##|  It takes as argument a list returned by makeCacheMatrix,   |
##| and demonstrates the technique for calculating the inverse  |
##| of a square invertible matrix only once; it is stored by    |
##| the functions contained in the argument, and retrieved as   |
##| well by them. If already computed, does not perform  solve()|
##| but retrieves the inverse matrix in the cached mat_inv.     |                                               |
##| Also it posts a message advising that it is retrieving it   |
##| from cached data                                            |
##|                                                             |
##\*-----------------------------------------------------------*/
cacheSolve <- function(x, ...) {    
    mat_inv <- x$getinv()    ## returns the inverse matrix stored
    if(!is.null(mat_inv)){    ## if not NULL, a valid inverse already computed
        message("getting cached solved inverse matrix")
        return(mat_inv)
    }
    my_matrix <- x$get()     ## if we are here, mat_inv was NULL
    inv <- solve(my_matrix)  ## one and only solve computation
    x$setinv(inv)            ## stores computed inverse matrix
    inv
}
