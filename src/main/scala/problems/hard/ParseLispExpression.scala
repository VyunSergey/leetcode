package problems.hard

import scala.annotation.tailrec
import scala.collection.mutable

object ParseLispExpression {
  def main(args: Array[String]): Unit = {
    val expression: String = Console.in.readLine()
    val res: Int = evaluate(expression)
    println(res)
  }

  def evaluate(expression: String): Int = {
    val letPattern = "\\(let (.+)\\)".r
    val addPattern = "\\(add (.+)\\)".r
    val multPattern = "\\(mult (.+)\\)".r
    val digitPattern = "([+|-]?\\d+)".r
    val variablePattern = "(\\w[\\w|\\d]*)".r

    def eval(expression: String, globalVariables: Map[String, Int]): Int = {
      val localVariables = mutable.HashMap.empty[String, Int]
      // println(s"\nEVAL |$expression| GLOBAL VARS: $globalVariables")

      expression match {
        case letPattern(str) =>
          // println(s"LET: |$str|")
          val (vars, expr) = parseVariables(str)
          // println(s"LET: ${vars.mkString("|", ", ", "|")} |$expr|")
          vars.foreach { case (x, expr) =>
            localVariables += x -> eval(expr, globalVariables ++ localVariables.toMap)
          }
          eval(expr, globalVariables ++ localVariables.toMap)
        case addPattern(str) =>
          // println(s"ADD: |$str|")
          val (leftExpr, rightExpr) = parseOperands(str)
          // println(s"ADD: |$leftExpr| + |$rightExpr|")
          operator(_ + _)(leftExpr, rightExpr, globalVariables)
        case multPattern(str) =>
          // println(s"MULT: |$str|")
          val (leftExpr, rightExpr) = parseOperands(str)
          // println(s"MULT: |$leftExpr| * |$rightExpr|")
          operator(_ * _)(leftExpr, rightExpr, globalVariables)
        case digitPattern(str) =>
          // println(s"DIGIT: |$str|")
          str.toInt
        case variablePattern(str) if globalVariables.contains(str) =>
          // println(s"VAR: |$str|")
          globalVariables(str)
        case str => throw new IllegalArgumentException(s"Can`t parse $str")
      }
    }

    @tailrec
    def parseVariables(str: String,
                       res: (Array[(String, String)], String) = (Array.empty[(String, String)], "")
                      ): (Array[(String, String)], String) = {
      // println(s"PARSE VARS str=|$str| res=(${res._1.mkString("|", ", ", "|")}, |${res._2}|)")
      if (str == "") res
      else {
        val (first, i) = parseExpression(str)
        // println(s"FIRST |$first| i=$i REST |${str.drop(i)}| len=${str.length}")
        if (i == str.length) (res._1, res._2 + first)
        else {
          val (second, j) = parseExpression(str.drop(i + 1))
          // println(s"SECOND |$second| j=$j REST |${str.drop(i + 1 + j + 1)}| len=${str.drop(i + 1).length}")
          parseVariables(str.drop(i + 1 + j + 1), (res._1 :+ (first, second), res._2))
        }
      }
    }

    def parseOperands(str: String): (String, String) = {
      val (left, i) = parseExpression(str)
      (left, str.drop(i + 1))
    }

    def parseExpression(str: String): (String, Int) = {
      var i = 0
      var brackets = 0
      if (str.head == '(') {
        brackets += 1
        i = 1
        while(i < str.length && brackets != 0) {
          if (str(i) == '(') brackets += 1
          if (str(i) == ')') brackets -= 1
          i += 1
        }

      } else {
        i = 0
        while(i < str.length && str(i) != ' ') i += 1
      }
      // println(s"PARSE |${str.take(i)}| i=$i")
      (str.take(i), i)
    }

    def operator(f: (Int, Int) => Int)
                (leftExpression: String,
                 rightExpression: String,
                 variables: Map[String, Int]): Int = {
      f(eval(leftExpression, variables), eval(rightExpression, variables))
    }

    eval(expression, Map.empty[String, Int])
  }
}
