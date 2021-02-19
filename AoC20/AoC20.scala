import scala.collection.immutable.LazyList._

sealed trait Direction { def unary_-(): Direction }
case object N extends Direction { lazy val unary_- = S }
case object S extends Direction { lazy val unary_- = N }
case object W extends Direction { lazy val unary_- = E }
case object E extends Direction { lazy val unary_- = W }

case class Tile(val tile: Vector[Vector[Char]]) {                                           // è un val!
    override def toString = tile map (_.mkString) mkString "\n"
    def flip = Tile(tile.reverse)                                                           // Queste due generano D_8
    def swap = Tile(tile.transpose)
    def side(t: Direction) = t match {
        case N => tile.head
        case S => tile.last
        case W => tile map (_.head)
        case E => tile map (_.last)
    }
    val orbit: LazyList[Tile] =
        this #::
        this.flip #::
        this.swap #::
        orbit(1).swap #::
        orbit(2).flip #::
        orbit(3).flip #::
        orbit(4).swap #::
        orbit(5).swap #:: LazyList.empty
    def fits(frame: Map[Direction, Vector[Char]]) = frame forall { case (direction, frameSide) => side(direction) sameElements frameSide }
    def orient(frame: Map[Direction, Vector[Char]]): Option[Tile] = orbit find (_ fits frame)

    def count(c: Char) = (tile map (x => x count (_ == c))).sum

    def searchAndReplace(pattern: Tile) = {
        var tileCopy = (tile map (_.toArray)).toArray
        val ph = pattern.tile.length
        val pw = pattern.tile.head.length                                                   // unsafe, pattern deve avere un elemento
        val th = tileCopy.length
        val tw = tileCopy.head.length
        def testAndReplace(i0: Int, j0: Int): Unit = {                                      // poco parametrico
            0 until ph foreach { i => 
                0 until pw foreach { j =>
                    if (pattern.tile(i)(j) == '#' && tileCopy(i0+i)(j0+j) != '#')
                        return
                }
            }
            0 until ph foreach { i => 
                0 until pw foreach { j =>
                    if (pattern.tile(i)(j) == '#')
                        tileCopy(i0+i)(j0+j) = 'O'
                }
            }
        }
        for (i0 <- 0 to th-ph; j0 <- 0 to tw-pw)
            testAndReplace(i0, j0)
        Tile((tileCopy map (_.toVector)).toVector)
    }
    
    def strip = Tile(tile drop 1 dropRight 1 map (_ drop 1 dropRight 1))
    def vcat(other: Tile) = Tile(tile ++ other.tile)
    def hcat(other: Tile) = Tile(tile zip other.tile map { case (x, y) => x ++ y })
}
object Tile {
    def apply(tileString: String) = new Tile((tileString split '\n' map (_.toVector)).toVector)
}

case class Position(val row: Int, val col: Int) {
    def nbHoodMap: Map[Direction, Position] = Map(
        N -> Position(row - 1, col),
        S -> Position(row + 1, col),
        W -> Position(row, col - 1),
        E -> Position(row, col + 1)
    )
}

case class Puzzle(val tiles: Map[Position, Tile] = Map(), val border: Set[Position] = Set()) {
    // porre border = Position(0,0) e togliere il caso eccezionale in addTile
    def frameAt(pos: Position) = for ((direction, position) <- pos.nbHoodMap if tiles contains position) yield (direction -> (tiles(position) side -direction))
    def addTile(tile: Tile, pos: Position): Option[Puzzle] = tile.orbit collectFirst (_ orient frameAt(pos)) getOrElse None match {
        case None => None
        case Some(orientedTile) => Some(Puzzle(tiles + (pos -> orientedTile), border union pos.nbHoodMap.values.toSet diff tiles.keys.toSet - pos))
    }
    def addTile(tile: Tile): Option[Puzzle] = if (border.isEmpty)
            Some(Puzzle(Map(Position(0,0) -> tile), Position(0,0).nbHoodMap.values.toSet))
        else
            border map { case pos => addTile(tile, pos) } find (_.isDefined) getOrElse None
    def searchTile(tileSeq: List[Tile], discarded: List[Tile] = Nil): (Option[Puzzle], List[Tile]) = tileSeq match {        // forse option è superfluo adesso
        case Nil  => (None, discarded)
        case tile::otherTiles => {
            val puz = addTile(tile)
            if (puz.isDefined) (puz, otherTiles ++ discarded) else searchTile(otherTiles, tile::discarded)
        }
    }
    def solve(tileSeq: List[Tile]): Option[Puzzle] = if (tileSeq.isEmpty) Some(this) else {
        searchTile(tileSeq) match {
            case (None, _) => Some(this)            // poi cambiare
            case (Some(next), otherTiles) => next solve otherTiles
        }
    }

    def solve_unsafe(tileSeq: List[Tile]): Puzzle = if (tileSeq.isEmpty) this else searchTile(tileSeq) match { case (next, otherTiles) => next.get solve_unsafe otherTiles }

    lazy val glued = {
        val firstPos = tiles.keys minBy { case Position(x,y) => x + y }
        val lastPos = tiles.keys maxBy { case Position(x,y) => x + y }
        firstPos.row to lastPos.row map { case row => 
            firstPos.col to lastPos.col map { case col => tiles(Position(row, col)).strip } reduce (_ hcat _)
        } reduce (_ vcat _)
    }
}

object Tiles {
    def apply(tilesString: String): List[Tile] = (for (t <- tilesString split "\n\n") yield Tile((t dropWhile (_ != '\n')).tail)).toList
}