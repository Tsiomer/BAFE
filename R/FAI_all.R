#' FAI calculation
#'
#' This function generate FAI and rank of each site and DB format using all survey data
#' This fucntion operate without parameter, you just operate function
#' @param filepath  Decide whether to set filepath or not
#' @export
#' @examples
#' FAI_all()
FAI_all <- function(FAI_WS = T, FAI_GR = T){
    filepath.1 <- file.choose()
    if(FAI_WS){
        FAI_WS(filepath = F)
    }
    if(FAI_GR){
        FAI_GR(filepath = F)
    }
}

