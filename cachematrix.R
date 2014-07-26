## The combination of functions makeCacheMatrix() and cacheSolve() make it
## possible to calculate the inverse of a matrix while caching the computation
## result in order to save time in consequent quieries of it.

## This function allows the construction of a stateful matrix object
## along with its corresponding cached inversed matrix.
## The function gets an original matrix as input, and returns a list of
## four functions, allowing to get the original matrix (get), set a new one
## (set), get the inversed matrix as it is stores in cache (getInversedMatrix) 
## and set a new calculation of the inversed form of the matrix (setInversedMatrix)

makeCacheMatrix <- function(original_mat = matrix()) {
    # initialize the cached inversed matrix
    inversedMatrix <- NULL
    set <- function(new_mat) {
        original_mat <<- new_mat
        inversedMatrix <<- NULL
    }
    get <- function() original_mat
    setInversedMatrix <- function(inv_mat) inversedMatrix <<- inv_mat
    getInversedMatrix <- function() inversedMatrix
    # return a list of the four functions decalred above, as described in the
    # main comment above
    list(set = set, 
         get = get,
         setInversedMatrix = setInversedMatrix,
         getInversedMatrix = getInversedMatrix)
}


## This function is given a cahced matrix object (represented by a function)
## and checks whether its invrsed matrix has been cached before or not.
## If a chaced inversed matrix exists - it is returned from cache.
## Otherwise, the inversed matrix is calculated (by calling the solve function)
## and then returned.

cacheSolve <- function(cached_mat_obj, ...) {
    ## Return a matrix that is the inverse of 'cached_mat_obj'
    inv_mat <- cached_mat_obj$getInversedMatrix()
    # check if the result has already been cached
    if(!is.null(inv_mat)) {
        # if so - return from cache
        message("getting cached data")
        return(inv_mat)
    }
    # Otherwise - explicitly calculate the inversed matrix, store, and return it
    data <- cached_mat_obj$get()
    inv_mat <- solve(data, ...)
    cached_mat_obj$setInversedMatrix(inv_mat)
    inv_mat
}
