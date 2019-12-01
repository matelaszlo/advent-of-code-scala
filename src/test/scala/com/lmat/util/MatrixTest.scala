package com.lmat.util

import com.lmat.util.Matrix._
import org.scalatest.funsuite.AnyFunSuite

class MatrixTest extends AnyFunSuite {
  val matrix: Matrix[Int] = Matrix(Vector(
    Vector(0,  1,  2,  3),
    Vector(4,  5,  6,  7),
    Vector(8,  9,  10, 11),
    Vector(12, 13, 14, 15)
  ))

  val smallMatrices = Matrix(Vector(
    Vector(
      Matrix(Vector(
        Vector(0, 1),
        Vector(4, 5))),
      Matrix(Vector(
        Vector(2, 3),
        Vector(6, 7)))),
    Vector(
      Matrix(Vector(
        Vector(8, 9),
        Vector(12, 13))),
      Matrix(Vector(
        Vector(10, 11),
        Vector(14, 15))))
  ))

  test("Flips") {
    assert(flipHorizontal(matrix) == Matrix(Vector(
      Vector(3,  2,  1,  0),
      Vector(7,  6,  5,  4),
      Vector(11, 10, 9,  8),
      Vector(15, 14, 13, 12)
    )))

    assert(flipVertical(matrix) == Matrix(Vector(
      Vector(12, 13, 14, 15),
      Vector(8,  9,  10, 11),
      Vector(4,  5,  6,  7),
      Vector(0,  1,  2,  3)
    )))
  }

  test("Rotations") {
    assert(rotateLeft(matrix) == Matrix(Vector(
      Vector(3,  7,  11, 15),
      Vector(2,  6,  10, 14),
      Vector(1,  5,  9,  13),
      Vector(0,  4,  8,  12)
    )))

    assert(rotateRight(matrix) == Matrix(Vector(
      Vector(12,  8,  4,  0),
      Vector(13,  9,  5,  1),
      Vector(14,  10, 6,  2),
      Vector(15,  11, 7,  3)
    )))
  }

  test("Break -> Merge") {
    assert(break(matrix, 2, 2)  == smallMatrices)
    assert(merge(smallMatrices) == matrix)
  }
}
