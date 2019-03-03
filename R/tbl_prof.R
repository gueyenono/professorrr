#' Create a tbl_prof object
#'
#' The function creates a \code{tbl_prof} object. This is the first step of the workflow as all subsequent
#' operations can only be performed on a \code{tbl_prof} object.
#'
#' @param x A data frame containing evaluation scores.
#' @param evals A character vector of regular expressions used to identify evaluation types in \code{x}.
#' @param max_scores A numeric vector specifying the maximum scores for each evaluation type.
#' @param weights A numeric vector specifying the weights of each evaluation type. Each weight is a decimal number most likely between 0 and 1.
#' @return A \code{tbl_prof} object.
#' 
#' @importFrom dplyr as_tibble select %>%
#' @importFrom stringr str_which str_to_lower
#' @importFrom purrr map set_names
#' @export

tbl_prof <- function(x, evals, max_scores = NULL, weights = NULL){

	if(is.null(max_scores)) max_scores <- rep(100, length(evals))
	if(is.null(weights)) weights <-  rep(1, length(evals)) / length(evals)

	attr(x, "evals") <-
	 	purrr::map(evals, function(eval){
	 		cols <- stringr::str_which(stringr::str_to_lower(colnames(x)), stringr::str_to_lower(eval))
	 		dplyr::select(x, cols)
	 	}) %>%
	 	purrr::set_names(evals) %>%
	 	dplyr::as_tibble()

	attr(x, "max_scores") <- max_scores
	attr(x, "weights") <- weights

	class(x) <- c("tbl_prof", class(x))

	x
}




