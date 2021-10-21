CreateBlockChain <- function(rasters, metadata, annotations){

  block <- list()
  currentTransactions <- list()
  blockchain <- list(block,currentTransactions)

  #################################
  ## GENESIS BLOCK
  previousHash <- 100
  proof <- 100
  CreateNextBlock(proof,previousHash)
  ##########################
  ##############################################
  ##############################################

  CreateBlock(rasters, metadata, annotations)
  lastblock <- GetLastBlock(blockchain)
  lastProof = lastblock$proof
  proof = proofOfWork(lastProof)
  previousHash = hashBlock(lastblock)
  d0 <- CreateNextBlock(proof, previousHash)

return(d0)
}
