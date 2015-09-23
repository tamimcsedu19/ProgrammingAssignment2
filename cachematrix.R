## Making a cached matrix inversion data. As matrix inversion is a costly operation , it is 
## Better to save it in first time calculation and reuse it again later on.
## 

## makeCacheMatrix returns a  List of four functions set,get,setInv,getInv
## We can consider this List as a special matrix which caches its inverse 

makeCacheMatrix <- function(mat = matrix()) {
    inv_matrix <- NULL
    set <- function(cur_mat) {
        mat <<- cur_mat
        inv_matrix <<- NULL
    }
    get <- function() mat
    set_inv_mat <- function(inv_mat) inv_matrix <<- inv_mat
    get_inv_mat <- function() inv_matrix
    
    list(set = set, get = get,
         set_inv_mat = set_inv_mat,
         get_inv_mat = get_inv_mat)

}


## This function cache's the Inverse of a matrix
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv <- x$get_inv_mat()
    if(!is.null(inv))
    {
        message("getting cached data")
        return(inv)
    }
    mat <- x$get()
    inv <- solve(mat)
    x$set_inv_mat(inv)
    inv
}

# An example Usage of the functions above
# > mat <- matrix(1:4,2,2)
# > special_mat <- makeCacheMatrix(mat)
# > inv <- cacheSolve(special_mat)
# > inv


