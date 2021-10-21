
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
