# json-rpc to bitcoind
bitcoind.rpc <- function(host = getOption("rpchost","127.0.0.1"),
                         user = getOption("rpcuser"),
                         password = getOption("rpcpassword"),
                         port = getOption("rpcport","8332"),
                         id = NA_integer_, method, params = list()){
    stopifnot(is.character(user), is.character(password), is.character(method))
    rpcurl <- paste0("http://",user,":",password,"@",host,":",port)
    req <- httr::POST(rpcurl, body = jsonlite::toJSON(list(jsonrpc = "1.0", id = id, method = method, params = params), auto_unbox=TRUE))
    if(httr::http_status(req)$category != "success"){
        message(jsonlite::fromJSON(httr::content(req, "text"))$error$message)
        httr::stop_for_status(req)
    }
    jsonlite::fromJSON(httr::content(req, "text"))
}

# wrappers
getinfo <- function() bitcoind.rpc(method = "getinfo")$result
getnewaddress <- function() bitcoind.rpc(method = "getnewaddress")$result
getreceivedbyaddress <- function(address, minconf=1L) bitcoind.rpc(method = "getreceivedbyaddress", params = list(address, minconf))$result

# helpers
makepaymenturi <- function(address, amount) paste0("bitcoin:",address,"?amount=",amount)
plotQR <- function(to_encode){
    mar <- par(mar=c(0,0,0,0))
    on.exit(par(mar))
    if(requireNamespace("qrencoder", quietly = TRUE)){
        suppressPackageStartupMessages(require("raster")) # cannot image without attaching raster package
        image(qrencoder::qrencode_raster(to_encode), asp=1, col=c("white", "black"), axes=FALSE, xlab="", ylab="")
    } else {
        plot(c(0, 1), c(0, 1), ann=FALSE, bty="n", type="n", xaxt="n", yaxt="n")
        text(x = 0.5, y = 0.5, "To display QR Code install hrbrmstr/qrencoder package", col = "black")
    }
}
