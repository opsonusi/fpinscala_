object Fibonacci {
  def fib(n: Int): Int = {
    def fibHelper(i: Int, p: Int, nxt: Int) : Int = {
      if(i == 0) p
      else fibHelper(i-1, nxt, p + nxt)  
    }

    fibHelper(n, 0, 1)
  }
}
