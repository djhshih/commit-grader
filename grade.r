#!/usr/bin/env Rscript

library(argparser, quietly=TRUE)
library(io)

# Functions

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


# Setup command line arguments

pr <- arg_parser("Committ grader") |>
	add_argument("--url", help="url to Git repository", default="") |>
	add_argument("--path", help="path to local repository directory", default="") |>
	add_argument("--start", help="start date", default=NA) |>
	add_argument("--end", help="end date", default=NA) |>
	add_argument("--plot", help="plot output file", default=NA) |>
	add_argument("--step", help="integration step size", default=0.01) |>
	add_argument("--format", help="output format", default="json")

argv <- parse_args(pr);

repo.url <- argv$url;
repo.dir <- argv$path;
start.date <- as.POSIXct(as.Date(argv$start));
end.date <- as.POSIXct(as.Date(argv$end));
plot.file <- argv$plot;
out.format <- argv$format;

# Process

if (repo.dir == "" && repo.url == "") {
	stop("Repository url or path must be specified.")
}

if (repo.url != "") {
	system(sprintf("git clone %s %s", repo.url, repo.dir));
	repo.dir <- sub("\\.git", "", sub(".*/", "", repo.url));
}

# get git commit log
commit.log <- system(sprintf("cd %s && git log --format=raw", repo.dir), intern=TRUE);

# second last field contains the timestamps
timestamps <- unlist(lapply(
	strsplit(grep("author ", commit.log, value=TRUE), " "),
	function(x) as.integer(x[length(x)-1])
));

dates <- as.POSIXct(timestamps);

if (is.na(start.date)) {
	start.date <- min(dates);
}
if (is.na(end.date)) {
	end.date <- max(dates);
}

times <- pmax(0, dates[dates <= end.date] - start.date);
times <- times / max(times);

rs <- seq(0, 0.5, 0.0001);

lhat <- ripley_l_1d(times, rs)

au <- discrepancy(0, rs);
a <- discrepancy(lhat, rs);
score <- pmax(0, au - a) / au;

if (is.nan(score)) {
	score <- 0;
}

if (!is.na(plot.file)) {
	qdraw(
		{
			par(mar=c(6, 6, 2, 2))
			plot(rs, lhat, las=1, xlab="r", ylab=expression(hat(L)(r)), type="l")
			abline(a=0, b=1, col="grey60")
			text(0.02, 0.48, adj=0, sprintf("score = %f", score))
		},
		file = plot.file
	)
}

if (out.format == "raw") {
	cat(score, "\n")
} else {
	# JSON format
	cat(sprintf('{\n  "commit_score": %f\n}\n', score))
}

