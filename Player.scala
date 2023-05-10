import BusterState.BusterState

import math._
import scala.util._
import scala.io.StdIn._

object EntityType {
  type EntityType = Int

  val Ghost = -1
}

object BusterState extends Enumeration {

  val EMPTY = Value(0) // Buster ne transportant pas de fantôme.
  val CARRYING = Value(1) // Buster transportant un fantôme.
  val STUNNED = Value(2) // Buster assommé.
  val BUSTING = Value(3) // Buster en train de viser un fantôme.

  type BusterState = Value
}

case class Entity(
                   entityId: Int,
                   position: Point,
                   entityType: Int,
                   state: BusterState,
                   stamina: Int,
                   value: Int
                 ) {
  def isInBustRangeOf(e: Entity): Boolean = {
    val dist2 = position.dist2(e.position)
    System.err.println(s"$dist2 ${Constants.BUST_MIN_RANGE2} ${Constants.BUST_MAX_RANGE2}")
    dist2 > Constants.BUST_MIN_RANGE2 && dist2 < Constants.BUST_MAX_RANGE2

  }

  def isInCollectRangeOf(base: Point): Boolean = {
    val dist2 = position.dist2(base)
    System.err.println(s"$dist2 ${Constants.BASE_BUST_RANGE2}")
    dist2 < Constants.BASE_BUST_RANGE2
  }
}

object Entity {
  def apply(
             entityId: Int,
             position: Point,
             entityType: Int,
             stateOrStamina: Int,
             value: Int
           ): Entity = {
    if (entityType == EntityType.Ghost) {
      Entity(
        entityId = entityId,
        position = position,
        entityType = entityType,
        state = BusterState.EMPTY,
        stamina = stateOrStamina,
        value = value
      )
    } else {
      Entity(
        entityId = entityId,
        position = position,
        entityType = entityType,
        state = BusterState(stateOrStamina),
        stamina = 0,
        value = value
      )
    }
  }
}

case class Point(
                  x: Int,
                  y: Int
                ) {
  override def toString = s"$x $y"

  def dist2(p: Point): Int = {
    val dy = y - p.y
    val dx = x - p.x
    dx * dx + dy * dy
  }
}

object Constants {

  val X_MAX = 16000
  val Y_MAX = 9000
  val LEFT_CORNER = Point(0, 0)
  val RIGHT_CORNER = Point(X_MAX, Y_MAX)

  val SIGHT_RANGE = 2200

  val BUST_MIN_RANGE = 900
  val BUST_MAX_RANGE = 1760
  val BUST_MIN_RANGE2 = BUST_MIN_RANGE * BUST_MIN_RANGE
  val BUST_MAX_RANGE2 = BUST_MAX_RANGE * BUST_MAX_RANGE
  val BASE_BUST_RANGE = 1600
  val BASE_BUST_RANGE2 = BASE_BUST_RANGE * BASE_BUST_RANGE
}

/**
 * Send your busters out into the fog to trap ghosts and bring them home!
 * */
object Player extends App {

  val bustersPerPlayer = readLine.toInt // the amount of busters you control
  val ghostCount = readLine.toInt // the amount of ghosts on the map
  val myTeamId = readLine.toInt // if this is 0, your base is on the top left of the map, if it is one, on the bottom right

  val (myBase, enemyBase) = if (myTeamId == 0) {
    (Constants.LEFT_CORNER, Constants.RIGHT_CORNER)
  } else {
    (Constants.RIGHT_CORNER, Constants.LEFT_CORNER)
  }

  // game loop
  while (true) {
    val entitiesCount = readLine.toInt // the number of busters and ghosts visible to you
    val entities = for (i <- 0 until entitiesCount) yield {
      // entityId: buster id or ghost id
      // y: position of this buster / ghost
      // entityType: the team id if it is a buster, -1 if it is a ghost.
      // state: For busters: 0=idle, 1=carrying a ghost. For ghosts: remaining stamina points.
      // value: For busters: Ghost id being carried/busted or number of turns left when stunned. For ghosts: number of busters attempting to trap this ghost.
      val Array(entityId, x, y, entityType, stateOrStamina, value) = (readLine split " ").filter(_ != "").map(_.toInt)
      Entity(
        entityId,
        Point(x, y),
        entityType,
        stateOrStamina,
        value
      )
    }

    val myTeam = entities.filter(_.entityType == myTeamId)

    val maybeNextGhost = entities.find(_.entityType == EntityType.Ghost)
    val ghostInSight = maybeNextGhost.isDefined
    val nextGhost = maybeNextGhost.getOrElse(null)

    // MOVE x y | BUST id | RELEASE | STUN id
    myTeam.foreach { buster =>
      buster.state match {
        case BusterState.EMPTY =>
          if (maybeNextGhost.isEmpty) {
            println(s"MOVE $enemyBase")
          } else if (buster.isInBustRangeOf(nextGhost)) {
            println(s"BUST ${nextGhost.entityId}")
          } else {
            println(s"MOVE ${nextGhost.position}")
          }
        case BusterState.CARRYING =>
          if (buster.isInCollectRangeOf(myBase)) {
            println("RELEASE")
          } else {
            println(s"MOVE $myBase")
          }
        case BusterState.STUNNED =>
          println("MOVE 8000 4500") // MOVE x y | BUST id | RELEASE | STUN id

        case BusterState.BUSTING =>
          if (ghostInSight) {
            println(s"BUST ${nextGhost.entityId}")
          } else {
            println("MOVE 8000 4500") // MOVE x y | BUST id | RELEASE | STUN id
          }
      }
    }
  }
}
