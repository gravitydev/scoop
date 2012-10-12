import com.gravitydev.scoop._, query._
import Repo._

object scoop {
  
  val q = from(issues)
    .where(issues.assigned_to === Option(3L))
    .select(issues.id)                            //> java.lang.NoSuchMethodError: com.gravitydev.scoop.package$.SqlNullable(Lcom/
                                                  //| gravitydev/scoop/SqlType;)Lcom/gravitydev/scoop/SqlBasicType;
                                                  //| 	at scoop$$anonfun$main$1.apply$mcV$sp(scoop.scala:7)
                                                  //| 	at scala.runtime.WorksheetSupport$$anonfun$$execute$1.apply$mcV$sp(Works
                                                  //| heetSupport.scala:75)
                                                  //| 	at scala.runtime.WorksheetSupport$.redirected(WorksheetSupport.scala:64)
                                                  //| 
                                                  //| 	at scala.runtime.WorksheetSupport$.$execute(WorksheetSupport.scala:74)
                                                  //| 	at scoop$.main(scoop.scala:4)
                                                  //| 	at scoop.main(scoop.scala)
  q.map(x => x)(null)
}