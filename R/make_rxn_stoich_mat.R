#' Make reaction stoichiometry matrix from reactant and product lists
#'
#' @param reactant_list A list of length n_reactions; list elements are
#'              character vectors of the index of each species consumed by this reaction;
#'              the number of times each reaction occurs matches reaction stoichiometry
#' @param product_list A list of length n_reactions; list elements are
#'              character vectors of the index of each species produced by this reaction;
#'              the number of times each product occurs matches reaction stoichiometry
#' @param species_names A character vector of length n_species mapping index numbers to species
#'
#' @returns matrix with dimensions (rows = n_rxns, cols = n_species);
#'              sparse (mostly 0) with +- 1 (or other low integers) encoding reactions;
#'              for each reaction row, reactants are negative and products positive;
#'              value = net number of that species created/destroyed by that reaction
#' @export
#'
#' @examples print("In development...")
#'
make_rxn_stoich_mat <- function(reactant_list, product_list, species_names){

    rxn_sto_mat <- matrix(0, nrow = length(reactant_list), ncol = length(species_names))

    colnames(rxn_sto_mat) <- species_names

    # note this currently subtracts 1 for each reactant (last line of loop); may need editing later
    for (i in 1:length(reactant_list)) {
        r <- reactant_list[[i]]
        for (j in 1:length(r)) {
            sp_ind <- as.numeric(r[j])
            rxn_sto_mat[i, sp_ind] <- rxn_sto_mat[i, sp_ind] -1
        }
    }

    # note this currently subtracts 1 for each reactant (last line of loop); may need editing later
    for (i in 1:length(product_list)) {
        r <- product_list[[i]]
        for (j in 1:length(r)) {
            sp_ind <- as.numeric(r[j])
            rxn_sto_mat[i, sp_ind] <- rxn_sto_mat[i, sp_ind] +1
        }
    }

    # return reaction stoichiometry matrix
    rxn_sto_mat
}
