# NOTE TO SELF 2025-03-26: TEST FUNCTION AND COMMIT
# example will require baked-in dataset

#' Modify a parameter data frame with calculated parameter values
#'
#' @param params data frame extracted from list
#'
#' @returns modified parameter data frame
#' @export
#'
#' @examples print("In development...")

update_param_df <- function(params){

    # minor reformatting
    params <- params %>%
        dplyr::select(-remove)

    # extract rows that have assigned numeric values
    p_in <- params %>%
        dplyr::filter(const_expr == "Constant") %>%
        dplyr::mutate(value = as.numeric(value))

    # extract values into a named vector, p_vec
    p_vec <- p_in$value
    names(p_vec) <- p_in$name

    # extract parameters which need to be calculated from existing p_vec components
    p_calc <- params %>%
        dplyr::filter(const_expr == "ConstantExpression")

    # reformat values to create valid R expressions
    # currently slightly hacky: need to detect "um3_to_L" but not detect "26" or "1"/...
    eqn_expr <- stringr::str_replace_all(p_calc$value, "([A-Za-z_3]+)", "p_vec['\\1']")

    # evaluate expressions and add (varname, value) to p_vec
    for (i in 1:length(eqn_expr)) {
        p_vec[p_calc$name[i]] <- eval(parse(text = eqn_expr[i]))
    }

    # turn calculated parameter info into a data frame
    p_df <- data.frame(name = names(p_vec), val = p_vec)

    # reformat data frame, combine with original parameter info, rearrange into original order
    p_df <- p_df %>%
        right_join(params, by = "name") %>%
        mutate(index = as.numeric(index)) %>%
        select(index, name, val) %>%
        arrange(index)

    # compare input and output names (and therefore order)
    testthat::test_that("parameters remain in original order",{
        expect_equal(params$name, p_df$name)
    })

    # return parameter data frame
    p_df
}
