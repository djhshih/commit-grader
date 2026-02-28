# Commit grader

Assign a score in [0, 1] to the commit history of a Git repository, by
comparing your commit timestamps against a Poisson point process.

A repository with frequent and consistent commits

## Requirements

`git` must be installed and accessible within your shell environment.

R package dependencies are listed in `requirements-r.txt`.

## Usage

To evaluate the commit history of a repository at `path/to/repo`,

``
./grade.r -p path/to/repo
```

The start and end dates for evaluation can be specified by
```
./grade.r --start 2020-01-01 --end 2026-01-31 -p path/to/repo
```

You can also specify the URL to a remote Git repository:
```
./grade.r -u https://github.com/djhshih/guidelines-genai-usage
```

For more details, see `./grade.r --help`.

