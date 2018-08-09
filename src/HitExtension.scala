import org.nlogo.api
import org.nlogo.api.{Argument, Context, ExtensionException, Turtle}
import org.nlogo.core
import org.nlogo.core.{AgentKind, Syntax}
import api.ScalaConversions._
import scala.collection.JavaConverters._

class HitExtension extends api.DefaultClassManager {
  def load(manager: api.PrimitiveManager): Unit = {
    manager.addPrimitive("ing?", Hitting)
    manager.addPrimitive("ing-turtles?", HittingAgents)
  }
}

object Hitting extends api.Reporter {
  override def getSyntax: Syntax = Syntax.reporterSyntax(
    right = List(Syntax.TurtleType),
    agentClassString = "-T--",
    ret = Syntax.BooleanType
  )

  def checkCollision(thisTurtle: Turtle, thatTurtle: Turtle): Boolean = {
    val distance = StrictMath.pow(thisTurtle.xcor - thatTurtle.xcor, 2) +
      StrictMath.pow(thisTurtle.ycor - thatTurtle.ycor, 2)
    val combinedSize = StrictMath.pow(thisTurtle.size / 2.0 + thatTurtle.size / 2.0, 2)
    (thisTurtle != thatTurtle) && (distance < combinedSize)
  }


  override def report(args: Array[Argument], context: Context): AnyRef = {
    val thisTurtle = context.getAgent.asInstanceOf[Turtle]
    val thatTurtle = args(0).getTurtle
    checkCollision(thisTurtle, thatTurtle).toLogoObject
  }
}

object HittingAgents extends api.Reporter {
  override def getSyntax: Syntax = Syntax.reporterSyntax(
    right = List(Syntax.AgentsetType),
    agentClassString = "-T--",
    ret = Syntax.BooleanType
  )

  override def report(args: Array[Argument], context: Context): AnyRef = {
    val thisTurtle = context.getAgent.asInstanceOf[Turtle]
    val turtles = args(0).getAgentSet
    if (turtles.kind != AgentKind.Turtle)
      throw new ExtensionException("Expected a turtle set")

    turtles.agents.asScala.exists(t => Hitting.checkCollision(thisTurtle, t.asInstanceOf[Turtle])).toLogoObject
  }
}
