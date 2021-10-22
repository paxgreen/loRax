CreateBlockChain <- function(annotation_extent, rasters, metadata, annotations){

CreateBlock <- function (annotation_extent, rasters, metadata, annotations )
{
txn <-  list('transaction'= list(
'annotation_extent' = annotation_extent
,'rasters' = rasters
,'metadata' = metadata
, 'annotations' = annotations
))
blockchain$currentTransactions <<- append(blockchain$currentTransactions,txn)
return(blockchain$currentTransactions)
}

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

#######################  GET BLOCK
GetLastBlock <- function (blockchain) {
  num_blocks <- length(blockchain$block)
  lastblock <<- blockchain$block[num_blocks][[1]]
  return(lastblock)
}
############  HASHBLOCK

hashBlock = function (block) {
  digest::digest(block,algo="sha256")}

proofOfWork <- function (last_proof)
{
  proof <- 0
  while (!validProof(last_proof, proof))
  {
    proof <- proof + 1
  }
  return (proof)
}
###
validProof <- function (last_proof, proof)
{
  guess = paste0(last_proof,proof)
  guess_hash = digest::digest(guess, algo = 'sha256')
  return (gsub('.*(.{2}$)', '\\1',guess_hash) == "00")
}




  block <- list()
  currentTransactions <- list()
  blockchain <- list(block,currentTransactions)




  ## GENESIS BLOCK
  previousHash <- 100
  proof <- 100
  CreateNextBlock(proof,previousHash)


  #################################

  ##########################
  ##############################################
  ##############################################

  CreateBlock(annotation_extent, rasters, metadata, annotations)
  lastblock <- GetLastBlock(blockchain)
  lastProof = lastblock$proof
  proof = proofOfWork(lastProof)
  previousHash = hashBlock(lastblock)
  d0 <- CreateNextBlock(proof, previousHash)

return(d0)
}
