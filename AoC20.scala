import scala.collection.mutable.{Map, ArrayBuffer}

sealed trait Direction { def unary_-(): Direction }
case object N extends Direction { lazy val unary_- = S }
case object S extends Direction { lazy val unary_- = N }
case object W extends Direction { lazy val unary_- = E }
case object E extends Direction { lazy val unary_- = W }

trait Piece {
    lazy val neighbours = Map[Direction, Piece](N -> End, S -> End, W -> End, E -> End)
    val isTerminal: Boolean
}
case object End extends Piece { val isTerminal = true }
case class Tile(val tile: Array[Array[Char]]) extends Piece {
    val isTerminal = false
    override def toString = tile map (_.mkString) mkString "\n"
    def flip = Tile(tile.reverse)                          // Queste due generano D_8
    def swap = Tile(tile.transpose)
    var freeSides = Set[Direction](N,S,W,E)
    def side(t: Direction) = t match {
        case N => tile.head
        case S => tile.last
        case W => tile map (_.head)
        case E => tile map (_.last)
    }
    val orbit: LazyList[Tile] =                            // valutare l'utilizzo di una view. Adesso uso calcoli incrementali ma è meno leggibile.
        this #::                                           // sto ingobrando la memoria ?
        this.flip #::
        this.swap #::
        orbit(1).swap #::
        orbit(2).flip #::
        orbit(3).flip #::
        orbit(4).swap #::
        orbit(5).swap #:: LazyList.empty
    def glue(other: Tile): Unit = {                         // forse non serve il valore di ritorno
        var gluedSides = Set[Direction]()                   // in effetti è al più uno per volta
        for (otherTr <- other.orbit; fSide <- freeSides) {
            if (otherTr.side(-fSide) sameElements side(fSide)) {
                gluedSides += fSide
                neighbours += fSide -> otherTr
                otherTr.neighbours += -fSide -> this
            }
        }
        freeSides = freeSides diff gluedSides
    }
    def goTowards(direction: Direction) = new Iterable[Tile] {
        def iterator = new Iterator[Tile] {
            var current:Piece = Tile.this
            def hasNext = !current.isTerminal
            def next = {
                val oldOne = current
                current = current neighbours direction
                oldOne.asInstanceOf[Tile]
            }
        }
    }
    def strip = Tile(tile drop 1 dropRight 1 map (_ drop 1 dropRight 1))
    def vcat(other: Tile) = Tile(tile ++ other.tile)
    def hcat(other: Tile) = Tile(tile zip other.tile map { case (x, y) => x ++ y })
}
object Tile {
    def apply(tileString: String) = new Tile(tileString split '\n' map (_.toArray))
}
class Puzzle(val tiles: Seq[Tile]) {
    lazy val solved = {
        for (i <- tiles; j <- tiles) if (i != j) i glue j
        var first: Tile = ((tiles(0) goTowards N).last goTowards W).last
        (for (lineFirst <- first goTowards S) yield
            (for(current <- lineFirst goTowards E) yield
                current).toVector).toVector
    }
}
object Puzzle {
    def apply(tilesString: String): Puzzle = new Puzzle(for (t <- tilesString split "\n\n") yield Tile((t dropWhile (_ != '\n')).tail))
}

    val puz = Puzzle("""Tile 2311:
..##.#..#.
##..#.....
#...##..#.
####.#...#
##.##.###.
##...#.###
.#.#.#..##
..#....#..
###...#.#.
..###..###

Tile 1951:
#.##...##.
#.####...#
.....#..##
#...######
.##.#....#
.###.#####
###.##.##.
.###....#.
..#.#..#.#
#...##.#..

Tile 1171:
####...##.
#..##.#..#
##.#..#.#.
.###.####.
..###.####
.##....##.
.#...####.
#.##.####.
####..#...
.....##...

Tile 1427:
###.##.#..
.#..#.##..
.#.##.#..#
#.#.#.##.#
....#...##
...##..##.
...#.#####
.#.####.#.
..#..###.#
..##.#..#.

Tile 1489:
##.#.#....
..##...#..
.##..##...
..#...#...
#####...#.
#..#.#.#.#
...#.#.#..
##.#...##.
..##.##.##
###.##.#..

Tile 2473:
#....####.
#..#.##...
#.##..#...
######.#.#
.#...#.#.#
.#########
.###.#..#.
########.#
##...##.#.
..###.#.#.

Tile 2971:
..#.#....#
#...###...
#.#.###...
##.##..#..
.#####..##
.#..####.#
#..#.#..#.
..####.###
..#.#.###.
...#.#.#.#

Tile 2729:
...#.#.#.#
####.#....
..#.#.....
....#..#.#
.##..##.#.
.#.####...
####.#.#..
##.####...
##..#.##..
#.##...##.

Tile 3079:
#.#.#####.
.#..######
..#.......
######....
####.#..#.
.#...#.##.
#.#####.##
..#.###...
..#.......
..#.###...""")