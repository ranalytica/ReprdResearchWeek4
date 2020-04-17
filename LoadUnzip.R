if(!file.exists("data")){
        dir.create("data")
}

download.file(fileUrl,destfile="./data/pamd.zip",method="curl")

dateDownloaded <- date()

maindf <- read_delim(fname, delim = ",", col_names = TRUE)



