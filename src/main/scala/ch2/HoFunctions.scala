object HoFunctions {
  
  def isSorted[A](as: Array[A], ordered: (A, A) => Boolean) : Boolean = {
    def sorted(idx: Int, prev: Boolean): Boolean = {
      if(idx+1 == as.length) prev
      else sorted(idx+1, ordered(as(idx), as(idx+1)))
    }
    
    sorted(0, true)
  }
}
