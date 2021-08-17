package problems.medium

object SetMatrixZeroes {
  def main(args: Array[String]): Unit = {
    val matrix: Array[Array[Int]] = Console.in.readLine().split(" ")
      .map(_.split(",").map(_.toIntOption).collect { case Some(x) => x })
    matrix.foreach(row => println(row.mkString("[", ",", "]")))
    setZeroes(matrix)
    println()
    matrix.foreach(row => println(row.mkString("[", ",", "]")))
  }

  def setZeroes(matrix: Array[Array[Int]]): Unit = {
    var isCol = false
    val R = matrix.length
    val C = matrix.head.length

    (0 until R).foreach { i =>
      // Since first cell for both first row and first column is the same i.e. matrix[0][0]
      // We can use an additional variable for either the first row/column.
      // For this solution we are using an additional variable for the first column
      // and using matrix[0][0] for the first row.
      if (matrix(i)(0) == 0) {
        isCol = true
      }

      (1 until C).foreach { j =>
        // If an element is zero, we set the first element of the corresponding row and column to 0
        if (matrix(i)(j) == 0) {
          matrix(0)(j) = 0
          matrix(i)(0) = 0
        }
      }
    }

    // Iterate over the array once again and using the first row and first column,
    // update the elements.
    (1 until R).foreach { i =>
      (1 until C).foreach { j =>
        if (matrix(i)(0) == 0 || matrix(0)(j) == 0) {
          matrix(i)(j) = 0
        }
      }
    }

    // See if the first row needs to be set to zero as well
    if (matrix(0)(0) == 0) (0 until C).foreach { j => matrix(0)(j) = 0 }

    // See if the first column needs to be set to zero as well
    if (isCol) (0 until R).foreach { i => matrix(i)(0) = 0 }
  }

}
