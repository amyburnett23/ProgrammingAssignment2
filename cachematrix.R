
#makeCacheMatrix is a function that creates a cached matrix. If cacheSolve has not returned a value for stored_matrix (the input for makeCacheMatrix), the function will output a null value.  Otherwise, it will output the values from cacheSolve
makeCacheMatrix <- function(stored_matrix = matrix()) {
    stored_inverse <- NULL
    set <- function(new_matrix) {
        stored_matrix <<- new_matrix
        stored_inverse <<- NULL
    }
    get <- function() stored_matrix
    set.inverse <- function(new_inverse_value) stored_inverse <<- new_inverse_value
    get.inverse <- function() stored_inverse
    list(set = set,
         get = get,
         set.inverse = set.inverse,
         get.inverse = get.inverse)
}

#cacheSolve is a function that returns the inverse matrix of special_matrix
cacheSolve <- function(special_matrix, ...) {
    cached_inverse <- special_matrix$get.inverse()
    if(!is.null(cached_inverse)) {
        message("getting cached data")
        return(cached_inverse)
    }
    
    data <- special_matrix$get()
    cached_inverse <- solve(data)
    special_matrix$set.inverse(cached_inverse)
    
    cached_inverse
}


