import org.nlogo.api
import org.nlogo.api.{Argument, Context, ExtensionException, Turtle}
import org.nlogo.core
import org.nlogo.core.{AgentKind, Syntax}
import api.ScalaConversions._
import org.nlogo.agent.World

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

  def checkCollision(world: World, thisTurtle: Turtle, thatTurtle: Turtle): Boolean = {
    if (thisTurtle == thatTurtle)
      false
    else {
      val combinedSize = thisTurtle.size / 2.0 + thatTurtle.size / 2.0
      world.protractor.distance(thisTurtle, thatTurtle, true) < combinedSize
    }
  }


  override def report(args: Array[Argument], context: Context): AnyRef = {
    val thisTurtle = context.getAgent.asInstanceOf[Turtle]
    val thatTurtle = args(0).getTurtle
    checkCollision(context.world.asInstanceOf[World], thisTurtle, thatTurtle).toLogoObject
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

    turtles.agents.asScala.exists { t =>
      Hitting.checkCollision(context.world.asInstanceOf[World], thisTurtle, t.asInstanceOf[Turtle])
    }.toLogoObject
  }
}
