#' Load a colorblind-friendly palette (from Wong 2011)
#'
#' @param list Boolean determining whether output is a named list (TRUE, default) or a named vector (FALSE)
#'
#' @returns if `list=TRUE`: named list with values "black", "red", "orange", "green", "yellow", "darkblue", "pink";
#'          if `list=FALSE`: named vector with same values
#' @export
#'
#' @examples
#' cb_pal <- load_cb_pal()
#' cb_pal$red  # or cb_pal[[2]]
#'
#' cb_pal_vec <- load_cb_pal(list = FALSE)
#' cb_pal_vec["red"] # or cb_pal_vec[2]

load_cb_pal <- function(list = TRUE){

    if(list){
        cb_pal <- list("#000000", "#D55E00",  "#56B4E9", "#E69F00", "#009E73", "#F0E442", "#0072B2",  "#CC79A7")
    } else {
        # define colorblind-friendly palette
        # black, red, sky blue, orange, green, yellow, dark blue, pink
        cb_pal <- c("#000000", "#D55E00",  "#56B4E9", "#E69F00", "#009E73", "#F0E442", "#0072B2",  "#CC79A7")
    }



    names(cb_pal) <- c("black", "red", "skyblue", "orange", "green", "yellow", "darkblue", "pink")

    # return color palette
    cb_pal

}
