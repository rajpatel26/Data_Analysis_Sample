#Task 1

#ordering of sequence
seq_order = function(x)
{
  N = nrow(x) + 1
  seq_order = rep(0,N)
  seq_order[1] = x[N-1,1]
  seq_order[2] = x[N-1,2]
  n = 2
  for(i in seq(N-2,1))
  {
    for(j in seq(1,n))
    {
      if(seq_order[j] == i)
      {
        seq_order[j] = x[i,1]
        if(j==n)
        {
          n = n + 1
          seq_order[n] = x[i,2]
        } else
        {
          n = n + 1
          for(k in seq(n, j+2)) seq_order[k] = seq_order[k-1]
          seq_order[j+1] = x[i,2]
        }
      }
    }
  }
  -seq_order
}