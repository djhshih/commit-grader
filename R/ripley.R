# estimate Ripley's K
ripley_k <- function(x, rs, area=NULL) {
	d <- sort(dist(x));
	n <- nrow(x);
	if (is.null(area)) {
		xmin <- apply(x, 2, min);
		xmax <- apply(x, 2, max);
		area <- prod(xmax - xmin);
	}
	density <- n / area;
	# we mutiple by 2 because we only counted the lower diagonal
	# of the distance matrix
	k <- 1/density * 2/n *
		vapply(rs, function(r) sum(d < r), 0)
	k
}

# Ripley's K with 1D edge correct correction
# Reference
# Doss, H. 1989. On estimating the dependence between two point processes. The Annals
# of Statistics 17:749-763.
# @param x   vector
# @param rs  grid of distance cutoffs (due to the edge effect, maximum value
#            in the grid should ideally be less than 1/2 of the study interval
ripley_k_1d_corrected <- function(x, rs, interval=NULL) {
	d <- dist(x);
	# edge effect correction
	# traverse lower triangle of distance matrix in column major order
	# row index of lower triangle of distance matrix
	idx.lower <- unlist(lapply(2:length(x), function(i) i:length(x)));
	d.to.bound.lower <- pmin(x[idx.lower], 1 - x[idx.lower]);
	w.lower <- ifelse(d > d.to.bound.lower, 2, 1);
	# traverse upper triangle of distance matrix in row major order
	# row index of upper triangle of distance matrix
	idx.upper <- unlist(lapply(1:length(x), function(j) rep(j, length(x) - j)));
	d.to.bound.upper <- pmin(x[idx.upper], 1 - x[idx.upper]);
	w.upper <- ifelse(d > d.to.bound.upper, 2, 1);
	n <- length(x);
	if (is.null(interval)) {
		interval <- max(x) - min(x);
	}
	density <- n / interval;
	k <- 1/density * 1/n *
		vapply(rs, function(r) {
			idx <- d < r;
			sum(w.lower * idx) + sum(w.upper * idx)
		}, 0)
	k
}

# normalized Ripley's K for 2D space
ripley_l_2d <- function(x, t) {
	k <- ripley_k(x, t);
	sqrt(k / pi);
}

# variance stablized Ripley's L for 1D space
ripley_l_1d <- function(x, rs, ...) {
	k <- ripley_k_1d_corrected(x, rs, ...);
	k / 2
}

discrepancy <- function(x, r) {
	dr <- diff(c(0, r));
	sum( (x - r)^2 * dr )
}

