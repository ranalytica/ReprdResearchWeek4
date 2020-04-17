if(!file.exists("data")){
        dir.create("data")
}

download.file(fileUrl,destfile="./data/maindf.bz2",method="curl")

dateDownloaded <- date()

maindf <- read_delim(fname, delim = ",", col_names = TRUE)



