# Load packages
source("setup-packages.R")  

# Helper function to make JSON-RPC requests
make_rpc_call <- function(url, method, params = list(), rpc_username, rpc_password) {
  body <- toJSON(list(method = method, params = params, id = "1", jsonrpc = "1.0"), auto_unbox = TRUE)
  response <- POST(url, body = body, encode = "json", authenticate(rpc_username, rpc_password), content_type("application/json"))
  if (status_code(response) != 200) {
    print(paste("HTTP error:", status_code(response), "with method", method))
    print(content(response, "text"))
    return(NULL)
  }
  data <- content(response, as = "parsed", type = "application/json")
  if (is.null(data$result)) {
    print(paste("No result found for", method, ": check the JSON-RPC response for errors"))
    print(data$error)
  }
  return(data$result)
}

# Main function to extract data
extract_data <- function() {
  rpc_username <- "bitcoin_user"  
  rpc_password <- "MdkjV34UfFN1kA87y43dDU54nvP6Qie4DM"  
  rpc_host <- "127.0.0.1"
  rpc_port <- "8332"
  
  url <- sprintf("http://%s:%s@%s:%s/", rpc_username, rpc_password, rpc_host, rpc_port)
  
  current_block_hash <- make_rpc_call(url, "getbestblockhash", rpc_username = rpc_username, rpc_password = rpc_password)
  block_data <- make_rpc_call(url, "getblock", list(current_block_hash, 2), rpc_username, rpc_password)
  
  blocks <- list()
  count <- 1
  
  while(count <= 10000 && !is.null(block_data) && !is.null(block_data$previousblockhash)) {
    print(paste("Processing block #", count, ": ", block_data$hash, sep=""))
    block_time <- as_datetime(block_data$time)
    block_fees <- sapply(block_data$tx, function(tx) ifelse("fee" %in% names(tx), tx$fee, NA))
     # in processed data?
    average_fee <- mean(block_fees, na.rm = TRUE)
    
    blocks[[count]] <- list(
      Date = block_time,
      AverageFee = average_fee,
      BlockHash = block_data$hash,
      BlockNumber = block_data$height,
      BlockSize = block_data$size,
      TotalTransactions = length(block_data$tx),
      TotalFees = sum(block_fees, na.rm = TRUE),
      Difficulty = block_data$difficulty  
    )
    
    current_block_hash <- block_data$previousblockhash
    block_data <- make_rpc_call(url, "getblock", list(current_block_hash, 2), rpc_username, rpc_password)
    
    count <- count + 1
  }
  
  if (length(blocks) > 0) {
    df <- do.call(rbind.data.frame, blocks)
    write.csv(df, "/data/raw/block_data.csv", row.names = FALSE)
    print("CSV file saved to /data/raw/block_data.csv")
  } else {
    print("No blocks processed.")
  }
}

# Run the extraction
extract_data()




