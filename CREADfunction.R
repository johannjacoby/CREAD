CREAD <- function(path, shortDescription){
	info <- list()
	rawdatapattern <- "^RAWDATA_.*"
	codebookpattern <- "^CODEBOOK_.*"
	
	rds <- list.files(path)[grep(rawdatapattern,list.files(path))]
	cbs <- list.files(path)[grep(codebookpattern,list.files(path))]
	candidates <- sort(intersect(gsub("\\..+$","",gsub("^RAWDATA_","",rds)), gsub("\\..+$","",gsub("^CODEBOOK_","",cbs))))[1]

	if (missing(path)) {
			return_value <- "ERROR"
			stop("No path given. Please correct.")
		} else if (missing(shortDescription)) {
			return_value <- "ERROR"
			if (is.na(candidates)) { 
				return_value <- "ERROR"
				stop("No short description given and no short description could be guessed: There was no matching pair of RAWDATA and CODEBOOK files.\nPlease indicate the shortDescription parameter.") 
			}
			else
				return_value <- "WARNINGGUESS"
				info[["rawdata"]] <- c("Raw data file", rds[grep(candidates, rds)])
				info[["codebook"]] <- c("Code book file", cbs[grep(candidates, cbs)])
				warning("No short description was given but a matching pair of RAWDATA and CODEBOOK files has been found. These will be used.")
		}	
		else {
			if (!is.na(cbs[grep("CODEBOOK_",shortDescription,"\\..+$", cbs)])) {
				info[["rawdata"]] <- c("Raw data file", rds[grep(shortDescription, rds)])
				info[["codebook"]] <- c("Code book file", cbs[grep(shortDescription, cbs)])
				return_value = "huhu"
			} else {
				warning("No codebook was found with a name matching the shortDescription.")
			}
		}
		lapply(info, function(x) cat(paste0(x[1],": ",x[2],"\n")))
		return(return_value)
}

CREAD("./")

print("huhu")