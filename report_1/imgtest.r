library("png")
library("EBImage")
source('~/SML/sml-group5/report_1/loadImage2.r')

folder = '/home/bjarkips/SML/trunk/2017/'
DPI = 300
groupNr = 'group5'
groupMemberNr = 1

if(!exists(ciffers)) {
  ciffers <- list(readPNG(paste(c(folder,groupNr,"/member",groupMemberNr,"/Ciphers",DPI,"-0.png"), collapse = "")),
                  readPNG(paste(c(folder,groupNr,"/member",groupMemberNr,"/Ciphers",DPI,"-1.png"), collapse = "")),
                  readPNG(paste(c(folder,groupNr,"/member",groupMemberNr,"/Ciphers",DPI,"-2.png"), collapse = "")),
                  readPNG(paste(c(folder,groupNr,"/member",groupMemberNr,"/Ciphers",DPI,"-3.png"), collapse = "")),
                  readPNG(paste(c(folder,groupNr,"/member",groupMemberNr,"/Ciphers",DPI,"-4.png"), collapse = "")))
}
r <-ciffers[[1]][,,1]
g <-ciffers[[1]][,,2]
b <-ciffers[[1]][,,3]
prepared <- (r+g+b)/3
prepared <- smoothImage(prepared, 250)
display(prepared)
