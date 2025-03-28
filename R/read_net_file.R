# read .net file in as a whitespace-delimited table;
#   columns have default names (n=5), but can be
#   manually specified if format varies;
#    currently has trouble with `reactions_text` blocks

## NOTE TO SELF: WRITE DOCUMENTATION, TEST, COMMIT
# example will require baked-in dataset


#' Title
#'
#' @param filename Output file name/path
#' @param col_names Vector of column names for imported table; defaults provided
#'
#' @returns a list with elements named with `col_names`
#' @export
#'
#' @examples print("In development...")
read_net_file <- function(filename,
                          col_names = c("index", "name", "value",
                                         "remove", "const_expr")){

    # import net file as a list representing a table
    il6_raw <- read_table(filename,
                          col_names)

    # extract block names and number of blocks
    block_names <- il6_raw %>%
        filter(index == "begin") %>%
        pull(name)
    nblocks <- length(block_names)

    # identify start/stop indices for block extraction
    split_start <- which(il6_raw$index == "begin") + 1
    split_end <- which(il6_raw$index == "end") - 1

    # cross-check dimensions
    test_that("number of splits matches number of blocks", {
        expect_equal(nblocks, length(split_start))
        expect_equal(nblocks, length(split_end))
    })

    # extract each block into a separate list element
    il6_net_list <- lapply(1:nblocks, function(i){
        il6_raw[(split_start[i]):(split_end[i]),]
    })

    # reassign block names
    names(il6_net_list) <- block_names

}
