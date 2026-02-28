#!/usr/bin/env Rscript

library(argparser, quietly=TRUE)
library(io)

source("R/ripley.R")

pr <- arg_parser("Committ grader") |>
	add_argument("project", help="path to project directory") |>
	add_argument("--start", help="start date") |>
	add_argument("--end", help="end date") |>
	add_argument("--plot", help="plot output file") |>
	add_argument("--step", help="integration step size", default=0.01)

argv <- parse_args(pr);

project.dir <- argv$project;
start.date <- ifelse(is.null(argv$start), NA, as.POSIXct(as.Date(argv$start)));
end.date <- ifelse(is.null(argv$end), NA, as.POSIXct(as.Date(argv$end)));
plot.file <- argv$plot;

commit.log <- system(sprintf("cd %s && git log --format=raw", project.dir), intern=TRUE);

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

if (!is.null(plot.file)) {
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

message(sprintf('{\n  "commit_score": %f\n}', score))

