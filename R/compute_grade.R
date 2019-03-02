#' Calculate percentages, weighted averages and total grade
#'
#' The function computes the percentages, weighted averages and the total grade.
#' The user can specify whether the lowest \code{n} scores of an evaluation type must be dropped.
#'
#' @param x A \code{tbl_prof} object.
#' @param drop_n A numeric vector specifying whether lowest scores should be dropped in specific evaluation types.
#' @return A \code{tbl_prof} object.

compute_grade <- function(x, drop_n = NULL){

	evals <- attr(x, "evals")
	weights <- attr(x, "weights")
	max_scores <- attr(x, "max_scores")

	percent_full <-
		map2(evals, max_scores, function(eval, max_score){
			(eval * 100) / max_score
		}) %>%
		map_dfc(rowMeans) %>%
		set_names(paste0(names(evals), "_percent"))

	weighted_means_full <- sweep(percent_full, 2, weights, "*") %>%
		set_names(paste0(names(evals), "_weighted_mean")) %>%
		as_tibble()

	total_full <- rowMeans(weighted_means_full)

	attr(x, "percent") <- percent_full
	attr(x, "weighted_mean") <- weighted_means_full
	attr(x, "total") <- total_full

	out <- bind_cols(x, percent_full, weighted_means_full, total_full)

	if(!is.null(drop_n)){

		if(is.null(drop_n)) drop_n <- rep(0, length(evals))

		evals_dropped <-
			map2(evals, drop_n, function(eval, drop){
				if(drop != 0){
					pmap(eval, function(...){
						scores <- unlist(list(...))
						sort(scores)[-(1:drop)]
					})
				} else {
					pmap(eval, function(...) unlist(list(...)))
				}
			}) %>%
			as_tibble()

		percent_dropped <-
			map2_dfc(evals_dropped, max_scores, function(eval, max_score){
				map_dbl(eval, ~ mean((.x * 100) / max_score))
			}) %>%
			set_names(paste0(names(evals), "_dropped_percent"))

		weighted_means_dropped <- sweep(percent_dropped, 2, weights, "*") %>%
			set_names(paste0(names(evals), "_dropped_weighted_mean")) %>%
			as_tibble()

		total_dropped <- rowMeans(weighted_means_dropped)

		attr(x, "evals_dropped") <- evals_dropped
		attr(x, "percent_dropped") <- percent_dropped
		attr(x, "weighted_means_dropped") <- weighted_means_dropped
		attr(x, "total_dropped") <- total_dropped

		out <- bind_cols(out, percent_dropped, weighted_means_dropped, total_dropped)
	}

	class(out) <- c("tbl_prof_means", class(out))

	out
}