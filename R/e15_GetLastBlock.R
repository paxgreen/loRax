#######################  GET BLOCK
GetLastBlock <- function (blockchain) {
  num_blocks <- length(blockchain$block)
  lastblock <<- blockchain$block[num_blocks][[1]]
  return(lastblock)
}
############  HASHBLOCK
