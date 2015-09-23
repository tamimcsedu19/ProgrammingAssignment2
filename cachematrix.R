## Making a cached matrix inversion data. As matrix inversion is a costly operation , it is 
## Better to save it in first time calculation and reuse it again later on.
## 

## makeCacheMatrix returns a  List of four functions set,get,setInv,getInv
## We can consider this List as a special matrix which caches its inverse 

makeCacheMatrix <- function(mat = matrix()) {
    
    inv_matrix <- NULL      #the cache of the inverse matrix

    #function to set the original matrix    
    set <- function(cur_mat) {
        mat <<- cur_mat
        inv_matrix <<- NULL
    }
    #getter of the original matrix
    get <- function() mat
    #setter of the inverse matrix
    set_inv_mat <- function(inv_mat) inv_matrix <<- inv_mat
    #getter of the inverse matrix
    get_inv_mat <- function() inv_matrix
    
    #return the special matrix which is a list of 4 functions
    list(set = set, get = get,
         set_inv_mat = set_inv_mat,
         get_inv_mat = get_inv_mat)

}


## This function cache's the Inverse of a matrix
##During the first run, this function will calculate the inverse matrix
##On subsequent run , it will just return the cached version of the matrix
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    #Check if a cached version of the inverse matrix is available
    inv <- x$get_inv_mat()
    #if available then return it
    if(!is.null(inv))
    {
        message("getting cached data")
        return(inv)
    }
    
    #else get the original matrix
    mat <- x$get()
    #calculate the inverse
    inv <- solve(mat)
    #cache the inverse to the special matrix
    x$set_inv_mat(inv)
    #return the inverse matrix
    inv
}

# An example Usage of the functions above
# > mat <- matrix(1:4,2,2)
# > special_mat <- makeCacheMatrix(mat)
# > inv <- cacheSolve(special_mat)
# > inv


