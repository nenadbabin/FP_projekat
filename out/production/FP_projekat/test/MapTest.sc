import scala.collection.mutable

val backups: mutable.HashMap[String, List[Int]] = new mutable.HashMap[String, List[Int]]()

backups.addOne(("1", List(1)))

val current: List[Int] = backups("1")

backups.addOne(("1",  2 :: current))

println(backups)


