#' Calculate percentages, weighted averages and total grade
#'
#' The function computes the percentages, weighted averages and the total grade.
#' The user can specify whether the lowest \code{n} scores of an evaluation type must be dropped.
#'
#' @param x A \code{tbl_prof} object.
#' @param drop_n A numeric vector specifying whether lowest scores should be dropped in specific evaluation types.
#' @return A \code{tbl_prof} object.
#' 
#' @importFrom dplyr as_tibble bind_cols %>%
#' @importFrom purrr map2 map_dfc set_names pmap map2_dfc map_dbl
#' @export

compute_grade <- function(x, drop_n = NULL){
	
	if(!is.tbl_prof(x)) stop("x must be a tbl_prof object. See ?tbl_prof.")

	evals <- attr(x, "evals")
	weights <- attr(x, "weights")
	max_scores <- attr(x, "max_scores")

	percent_full <-
		purrr::map2(evals, max_scores, function(eval, max_score){
			(eval * 100) / max_score
		}) %>%
		purrr::map_dfc(rowMeans) %>%
		purrr::set_names(paste0(names(evals), "_percent"))

	weighted_means_full <- sweep(percent_full, 2, weights, "*") %>%
		purrr::set_names(paste0(names(evals), "_weighted_mean")) %>%
		dplyr::as_tibble()

	total_full <- rowMeans(weighted_means_full)

	attr(x, "percent") <- percent_full
	attr(x, "weighted_mean") <- weighted_means_full
	attr(x, "total") <- total_full

	out <- dplyr::bind_cols(x, percent_full, weighted_means_full, total = total_full)

	if(!is.null(drop_n)){

		if(is.null(drop_n)) drop_n <- rep(0, length(evals))

		evals_dropped <-
			purrr::map2(evals, drop_n, function(eval, drop){
				if(drop != 0){
					purrr::pmap(eval, function(...){
						scores <- unlist(list(...))
						sort(scores)[-(1:drop)]
					})
				} else {
					purrr::pmap(eval, function(...) unlist(list(...)))
				}
			}) %>%
			dplyr::as_tibble()

		percent_dropped <-
			purrr::map2_dfc(evals_dropped, max_scores, function(eval, max_score){
				purrr::map_dbl(eval, ~ mean((.x * 100) / max_score))
			}) %>%
			purrr::set_names(paste0(names(evals), "_dropped_percent"))

		weighted_means_dropped <- sweep(percent_dropped, 2, weights, "*") %>%
			purrr::set_names(paste0(names(evals), "_dropped_weighted_mean")) %>%
			dplyr::as_tibble()

		total_dropped <- rowMeans(weighted_means_dropped)

		attr(x, "evals_dropped") <- evals_dropped
		attr(x, "percent_dropped") <- percent_dropped
		attr(x, "weighted_means_dropped") <- weighted_means_dropped
		attr(x, "total_dropped") <- total_dropped

		out <- dplyr::bind_cols(out, percent_dropped, weighted_means_dropped, total_dropped = total_dropped)
	}

	class(out) <- c("tbl_prof_means", class(out))

	out
}
