## Exmaple of how to download files from net

# If zip file then it needs to be unziped
# using:
# unzip(zipfile = "outFile") # unpack the files into subdirectories
# 

#
# general downloader that takes lists
#

downloadFile<- function(fileList,outFileList=NULL) {
    if(!file.exists("./data")){dir.create("./data")}
    # configure set download method for windows vs. Mac / Linux
    dlMethod <- "curl"
    if(substr(Sys.getenv("OS"),1,7) == "Windows") dlMethod <- "wininet"
    for (i in 1:length(fileList)) {
        if(is.null(outFileList)){
            outFile <- paste0("./data/",basename(fileList[i]))
        } else {
            outFile <- paste0("./data/",outFileList[i])
        }
        if(!file.exists(outFile)){
            download.file(fileList[i],destfile=outFile,method=dlMethod,mode="wb")
        }
    }
}


