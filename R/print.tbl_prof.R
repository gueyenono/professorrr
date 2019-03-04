print.tbl_prof <- function(x){
	cat(paste0("# Evaluation types: ", length(attr(x, "evals")), "\n"))
	tibble:::print.tbl(x)
}
