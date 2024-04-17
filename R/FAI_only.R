#' FAI calculation
#' 
#' This function generate FAI and rank of each site using all survey data
#' This fucntion operate without parameter, you just operate function 
#' @param filepath  Decide whether to set filepath or not
#' @export
#' @examples 
#' FAI_only()
FAI_only <- function(FAI_WS_only = T, FAI_GR_only = T){
    filepath.1 <- file.choose()
    if(FAI_WS_only){
        FAI_WS_only(filepath = F)
    }
    if(FAI_GR_only){
        FAI_GR_only(filepath = F)
    }
}

