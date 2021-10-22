CreateBlockChain <- function(annotation_extent, rasters, metadata, annotations){


  block <- list()
  currentTransactions <- list()
  blockchain <- list(block,currentTransactions)


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

  #################################
  ## GENESIS BLOCK
  previousHash <- 100
  proof <- 100
  CreateNextBlock(proof,previousHash)
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
