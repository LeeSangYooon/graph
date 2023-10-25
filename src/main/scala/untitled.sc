import collection.mutable
def a(s: mutable.Set[Int]): Unit = {
  s += 1
}
val set=  mutable.Set(2,3)
a(set)
println(set)