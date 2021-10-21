##############  CREATE NEXT BLOCK ##################
CreateNextBlock <- function (proof, previousHash){
  num_blocks <- length(blockchain$block)
  previousHash <- ifelse (is.null(previousHash)
                          , blockchain$hashBlock(blockchain$block[num_blocks])
                          , previousHash)
  nextblock <- list(list('index' = num_blocks + 1, 'timestamp' = Sys.time()
                         , 'transactions' =  blockchain$currentTransactions, 'proof' = proof, 'previousHash' = previousHash))
  #bc$currentTransactions = NULL
  blockchain$block <<- append(blockchain$block, nextblock)
  return (blockchain$block)
}
