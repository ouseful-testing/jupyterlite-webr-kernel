# Fragments and Notes


R function to try to tidy up a URL

```R
JUPYTERLITE_PATH = "https://ouseful-testing.github.io/jupyterlite-webr-kernel"
trim_url <- function(url) {
  # Only apply the regex if there's a path after the domain
  if (grepl("https?://[^/]+/.+", url)) {
    # Remove files (like index.html) or query strings, but preserve the full path
    url <- sub("/([^/]+\\.[^/]+)([?#].*)?$", "/", url)
  }
  
  # Ensure the URL ends with a trailing slash
  if (!grepl("/$", url)) {
    url <- paste0(url, "/")
  }
  
  return(url)
}
```

Monkey path R `read.csv()` function so we try a local mount and also local browser storage.

The `JUPYTERLITE_PATH` function is set in the `web_kernel.ts` environment startup function.

```R

JUPYTERLITE_FILES_PATH <- get0("JUPYTERLITE_FILES_PATH", ifnotfound = "files/")
LOCAL_FILESHARE_PATH <- get0("LOCAL_FILESHARE_PATH", ifnotfound = "share/")

read.csv <- function (filename, use_browser_storage = FALSE, verbose = F) 
{
    if (!exists("JUPYTERLITE_PATH") | is.null(JUPYTERLITE_PATH) | !nzchar(JUPYTERLITE_PATH) ) {
        # We will assume we are running locally if no path is set
        # BUT we should be able to set the path in the JupyterLite core
        JUPYTERLITE_PATH="http://localhost:8348"
    }

    # First remove query parameters (anything after ?)
    domain_path <- gsub("\\?.*$", "", JUPYTERLITE_PATH)
    # Then remove index.html from end
    domain_path <- gsub("/index\\.html$", "/", domain_path)
    # Then the /lab path element
    domain_path <- gsub("/lab/?$", "/", domain_path)
    # Finally ensure trailing slash (if none of the above added one)
    domain_path <- gsub("/?$", "/", domain_path)

    if (!grepl("^https?://", filename)) {

        path_prefix <- ifelse(use_browser_storage, JUPYTERLITE_FILES_PATH, 
            LOCAL_FILESHARE_PATH)
        full_path <- paste0 (domain_path,
            path_prefix, filename)
        tryCatch({
            data <- utils::read.csv(full_path)
            if (path_prefix == JUPYTERLITE_FILES_PATH) {
                if (verbose) 
                  message("Reading from local browser storage.")
                if (verbose) 
                  message("To persist this data file you will need to download it from the main, rather than Local File System, directory listing, to your shared M348-24J directory.")
            }
            else {
                if (verbose) 
                  message("Reading file from shared 'M348-24J' directory.")
            }
            return(data)
        }, error = function(e) {
            if (path_prefix == LOCAL_FILESHARE_PATH) {
                if (verbose) 
                  message("File not found in shared directory 'M348-24J' in your desktop computer user directory")
                if (verbose) 
                  message("Trying local browser storage...")
                path_prefix <- JUPYTERLITE_FILES_PATH
                full_path <- paste0(domain_path,
                  path_prefix, filename)
                tryCatch({
                  data <- utils::read.csv(full_path)
                  if (verbose) 
                    message("File found in browser storage. To persist this data file you will need to download it from the main, rather than Local File System, directory listing, to your shared M348-24J directory.")
                  return(data)
                }, error = function(e) {
                  if (verbose) 
                    message(paste0("File not found in browser storage: ", full_path))
                })
            }
            else {
                if (verbose) 
                  message(paste0("File not found in browser storage / main Jupyter browser listing: ", full_path))
            }
        })
    }
    else {
        data <- utils::read.csv(filename)
        return(data)
    }
}
```


TRY THE FOLLOWING

```R
#' A function to read .csv files wherever they are in the Jupyterlite environment
#'
#' @param filename the name of the file to be read in
#' @param use_browser_storage indicator of whether to look in browser storage
#' @param verbose should details about where files be given?
#'
#' @return a data frame containing the contents of the file
#'
#' @author Karen Vines
#' 
#' @export


read.csv <- function(filename, use_browser_storage = FALSE, verbose=F) {
	
	JUPYTERLITE_FILES_PATH <- get0("JUPYTERLITE_FILES_PATH", ifnotfound = "files/")
	LOCAL_FILESHARE_PATH <- get0("LOCAL_FILESHARE_PATH", ifnotfound = "share/")

	pathSet <- TRUE
	if (!exists("JUPYTERLITE_PATH")) {
		if (verbose) cat("Warning: JUPYTERLITE_PATH has not been set.\n")
		pathSet <- FALSE 
	} else if (is.null(JUPYTERLITE_PATH)) {
		if (verbose) cat("Warning: JUPYTERLITE_PATH is equal to a null object.\n")
		pathSet <- FALSE 
	} else if (!nzchar(JUPYTERLITE_PATH)){
		if (verbose) cat("Warning: JUPYTERLITE_PATH is equal to an empty string.\n")
		pathSet <- FALSE 
	} else {
#		cat("JUPYTERLITE_PATH OK. \n")
	}

	if (!pathSet) {
		JUPYTERLITE_PATH <<- "http://localhost:8348"
		if (verbose) cat("Please set the JUPYTERLITE_PATH variable to the current page URL.\n")
		if (verbose) cat('For example, if the current page is http://localhost:8348/lab run the following command:\n JUPYTERLITE_PATH <- "http://localhost:8348/lab"\n')
		pathex <- paste('"',JUPYTERLITE_PATH,'"', sep="") 
		if (verbose) cat('In the meantime the value ',pathex, ' is being used.\n')
	} else {
#		cat("Not resetting JUPYTERLITE_PATH\n")
	}

	# First remove query parameters (anything after ?)
	domain_path <- gsub("\\?.*$", "", JUPYTERLITE_PATH)
	# Then remove index.html from end
	domain_path <- gsub("/index\\.html$", "/", domain_path)
	# Then remove the /lab path element
	domain_path <- gsub("/lab/?$", "/", domain_path)	
	# Finally ensure trailing slash (if none of the above added one)
	domain_path <- gsub("/?$", "/", domain_path)

	if (!grepl("^https?://", filename)) {
		path_prefix <- ifelse(use_browser_storage, JUPYTERLITE_FILES_PATH, LOCAL_FILESHARE_PATH)
		full_path <- paste0(domain_path, path_prefix, filename)
		tryCatch({
			data <- utils::read.csv(full_path)
			if (path_prefix == JUPYTERLITE_FILES_PATH){
				if (verbose) message("Reading from local browser storage.")
				if (verbose) message("To persist this data file you will need to download it from the main, rather than Local File System, directory listing, to your shared M348-24J directory.")
			} else {
				if (verbose) message("Reading file from shared 'M348-24J' directory.")
			} #line41?
			return(data)
			}, error = function(e) {
				if (path_prefix == LOCAL_FILESHARE_PATH) {
				if (verbose) message("File not found in shared directory 'M348-24J' in your desktop computer user directory")
				if (verbose) message("Trying local browser storage...")
				path_prefix <- JUPYTERLITE_FILES_PATH
				full_path <- paste0(domain_path, path_prefix, filename)
				tryCatch({ 
					data <- utils::read.csv(full_path)
					if (verbose) message("File found in browser storage. To persist this data file you will need to download it from the main, rather than Local File System, directory listing, to your shared M348-24J directory.")
					return(data)
				}, error = function(e) {
					if (verbose) message(paste0("File not found in browser storage: ", full_path))
				})
			} else {
				if (verbose) message(paste0("File not found in browser storage / main Jupyter browser listing: ", full_path))	
			}
		})
	} else {
		data <- utils::read.csv(filename)
		return(data)
	}
}
```