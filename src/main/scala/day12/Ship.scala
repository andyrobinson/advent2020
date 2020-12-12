package day12

case class Ship(coordinates: (Int,Int) = (0,0), direction: Direction = Direction.east, waypoint: (Int, Int) = (10,1)) {

  def manhattanDistance: Int = Math.abs(coordinates._1) + Math.abs(coordinates._2)

  private def rotate(move: Move) : Ship =
    this.copy(direction = Direction.compassPoints.find(_.bearing == newBearing(move)).get)

  private def newBearing(move: Move) =
    if (move.cmd == 'L')
      (360 + this.direction.bearing - move.amount) % 360
    else
      (this.direction.bearing + move.amount) % 360

  private def rotateWaypoint(move: Move): Ship = {
    val amount = if (move.cmd == 'L') (360 - move.amount) else move.amount
    amount match {
      case 0 => this
      case 90 => this.copy(waypoint = (this.waypoint._2, -this.waypoint._1))
      case 180 => this.copy(waypoint = (-this.waypoint._1, -this.waypoint._2))
      case 270 => this.copy(waypoint = (-this.waypoint._2, this.waypoint._1))
      case _ => throw new RuntimeException("Unknown rotation")
    }
  }
}

object Ship {
  def follow(moves: List[Move], ship: Ship, moveFn: (Move, Ship) => Ship) : Ship = moves match {
    case Nil => ship
    case hd::tl => follow(tl, moveFn(hd, ship), moveFn)
  }

  def move(move: Move, ship: Ship): Ship = {
    move.cmd match {
      case 'L'|'R' => ship.rotate(move)
      case 'F' => ship.copy(coordinates = moveInDirection(ship.coordinates, ship.direction.moveVector, move.amount))
      case dir if Direction.compassPoints.exists(_.name == dir) => ship.copy(coordinates = compassPointMove(move, ship.coordinates))
      case _ => throw new RuntimeException("Unknown move")
    }
  }

  def moveWaypointorShip(move: Move, ship: Ship): Ship = {
    move.cmd match {
      case 'L'|'R' => ship.rotateWaypoint(move)
      case 'F' => ship.copy(coordinates = moveInDirection(ship.coordinates, ship.waypoint, move.amount))
      case dir if Direction.compassPoints.exists(_.name == dir) => ship.copy(waypoint = compassPointMove(move, ship.waypoint))
      case _ => throw new RuntimeException("Unknown move")
    }
  }

  private def compassPointMove(move: Move, startingPoint: (Int, Int)) = {
    val moveDirection = Direction.compassPoints.find(_.name == move.cmd).get
    moveInDirection(startingPoint, moveDirection.moveVector, move.amount)
  }

  private def moveInDirection(currentPostion: (Int,Int), vector: (Int,Int), amount: Int): (Int,Int) =
    (currentPostion._1 + amount * vector._1,
      currentPostion._2 + amount * vector._2)


}
