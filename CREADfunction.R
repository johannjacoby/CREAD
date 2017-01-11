CREAD <- function(path, fileid){
	mode <- 0
	info <- list()
	rawdatapattern <- "^RAWDATA_"
	codebookpattern <- "^CODEBOOK_"
	if (missing(path)) {
		return_value <- "ERROR"
		stop(paste0("No path given. Please correct (use './' for the current path ",getwd(),"."))
	}
	
	if (missing(fileid)){
		mode <- 3
		rds <- list.files(path)[grep(rawdatapattern,list.files(path))]
		cbs <- list.files(path)[grep(codebookpattern,list.files(path))]
		candidates <- sort(intersect(gsub("\\..+$","",gsub(rawdatapattern,"",rds)), gsub("\\..+$","",gsub(codebookpattern,"",cbs))))[1]
		guessmode <- "Neither shortDescription nor filenames have been indicated.\n"
		if (is.na(candidates)) { stop(paste0(guessmode,"The function tried to find a matching pair of RAWDATA and CODEBOOK files. But it did not find any. Sorry..."))}
		else { 
			data <- rds[grep(candidates, rds)]
			metadata <- cbs[grep(candidates, cbs)]
			warning(paste0(guessmode, "The function found the following matching files and will use these:\n\t", data,"\n\t", metadata))
		}
	}
	else if (length(fileid)==2) {
		if (!file.exists(fileid[1])) {
			stop(paste0("Parameter 'fileid' has not been correctly specified:\nfile 1 [",fileid[1],"] does not exist."))
		}
		else if (!file.exists(fileid[2])) {
			stop(paste0("Parameter 'fileid' has not been correctly specified:\nfile 2 [",fileid[2],"] does not exist."))			
		}
		else { 
			mode <- 2
			data <- list.files(path)[grep(fileid[1],list.files(path))]
			metadata <- list.files(path)[grep(fileid[2],list.files(path))]
		}
	}
	else if (length(fileid)==1) {
		if (length(grep(paste0(rawdatapattern,fileid,"\\..+$"),list.files(path))) > 0 && length(grep(paste0(codebookpattern,fileid,"\\..+$"),list.files(path)))>0) { 
			mode <- 1
			data <- list.files(path)[grep(paste0(rawdatapattern,fileid,"\\..+$"),list.files(path))]
			metadata <- list.files(path)[grep(paste0(codebookpattern,fileid,"\\..+$"),list.files(path))]
		}
	}
	else { 
			stop(paste0("No matching RAWDATA and CODEBOOK files with the short description ",fileid," have been found in path\n\t",path,"."))
	}
newdata<-read.table(data)
cat(paste0("Raw data file: ",data,"\n"))
cat(paste0("  Variables: ",paste0(names(newdata), collapse=" "),"\n"))
cat(paste0("  Observations (Rows): ",nrow(newdata),"\n"))
cat(paste0("Codebook file: ",metadata,"\n"))

		return(newdata)
}

CREAD("./", c("RAWDATA_Thisshortdesc.csvy","hgdtd"))
