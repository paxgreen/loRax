

### VALIDATE CHAIN
validateChain <- function (mydata)

  {
  blockchain <- mydata
    i <- 2
    lastblock <- blockchain$block[i][[1]]

    while (i < length(blockchain$block))
    {
      thisblock = blockchain$block[i+1][[1]]
      # checking for valid linking
      if (thisblock$previousHash != hashBlock(lastblock)) {
        print('FALSE')
      }
      # checking for proof validity
      if(!validProof(lastblock$proof, thisblock$proof))
      {
        print ('FALSE')
      }
      lastblock <- thisblock
      i <- i +1
    }
    print('TRUE')
  }
