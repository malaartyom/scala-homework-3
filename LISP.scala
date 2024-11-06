// It's important to keep this import, since default available `Map` type
// defined in Predef.scala is an alias for `scala.immutable.Map`.
//
// Therefore, with this import we make it possible to assign
// instances of both `mutable.Map` and `immutable.Map` to the
// variables of type `Map` (`scala.collection.Map`).
import LISP.Expression.*
import LispDSL.*

import scala.collection.Map


/** LISP -- LISP Interpreter for Scala Programmers.
  *
  * You should finish the implementation of this module according
  * to the reference described in `README.md` to complete this assignment.
  */
object LISP {
  
  private def print(args: Seq[Expression]): Expression = {
    
    Expression.List(Seq.empty)
  }

  private def plus(numbers: Seq[Expression]): Double = {
    var s = 0.0
    for (x <- numbers) s += x.asInstanceOf[Number].value
    s
  }

  private def multiply(numbers: Seq[Expression]): Double = {
    var s = 1.0
    for (x <- numbers) s *= x.asInstanceOf[Number].value
    s
  }


  /** Possible symbolic expressions (or simply S-expressions)
    * of our LISP dialect.
    *
    * *Feel free to add your own cases if necessary*.
    */
  enum Expression {
    /** Just a string, e.g. "hello". */
    case String(value: Predef.String)

    /** Just a numeric value, e.g. `42.0`.
      * We use only floating-point numbers for convenience.
      */
    case Number(value: Double)

    /** Some identifier. It is implied that symbol should be associated with some
      * other expression, available in current scope.
      * 
      * E.g., `nil` should be associated with empty list (`List(Seq.empty)`)
      * in any scope.
      */
    case Symbol(value: Predef.String)

    /** Sequence of expressions. */
    case List(value: Seq[Expression])

    /** "Quoted" expression that should provide its value as-is
      * during the evaluation.
      *
      * Examples:
      * 1. `Quote(Number(42.0))` should be evaluated to `Number(42.0)`
      * 2. `Quote(Symbol("nil"))` should be evaluated to `Symbol("nil")`
      *    (not `List(Seq.empty)`).
      * 3. `Quote(List(Seq(Symbol("+"), Number(21.0), Number(21.0))))`
      *    should be evaluated to
      *    `(List(Seq(Symbol("+"), Number(21.0), Number(21.0)))`,
      *    *not* `Number(42.0)`.
      */
    case Quote(value: Expression)

    /** Function with given `params`, `body` and associated lexical scope `env`.
      */
    case Lambda(params: Seq[Expression.Symbol], body: Expression, env: Environment)

    case Plus
    case Multiply
    case Print
  }

  /** Mapping between symbols and expressions (variable identifiers and their values). */
  type Environment = Map[Expression.Symbol, Expression]

  /** Global scope with definitions that should be visible from any point of the LISP program,
    * if they're not redefined in some local scope.
    *
    * For example, if symbol `nil` is occured in your program during evaluation, its value
    * should be obtained from this environment or the refined local one.
    */
  val globalEnvironment: Environment = {

    /** LispDSL helper functions are available here.
      * Look `utils.scala` to find the implementation,
      * and `sanity_check.scala` to find use cases.
      */

    Map(
      "nil".s -> l(), //
      "nil?".s -> lam(Seq("x".s), l("==".s, "x".s, l().q), globalEnvironment),
      "*".s -> Multiply,
      "quote".s -> lam(Seq("x".s), "x".s.q, globalEnvironment),
      "+".s -> Plus,
      "print".s -> Print
      /** Implement other definitions here. */)
  }
  /** Evaluates some expression `expr` knowing about given environment `env`.
    *
    * You should implement this method according to the specification written in
    * `README.md`.
    */
  def evaluate(expr: Expression, env: Environment = globalEnvironment): Expression = {
   expr match {
    case String(_) => expr
    case Number(_) => expr
    case Print => expr
    case Plus => expr
    case Multiply => expr
    case Quote(value) => value
    case Symbol(value) => env(Symbol(value))
    case Lambda(_, _, _) => ???
    case List(value) =>
      var x = scala.collection.immutable.List[Expression]()
      for (y <- value) {
        x = x.appended(evaluate(y))
      }
      val head = x.head
      head match {
        case Plus => Number(plus(value.drop(1)))
        case Multiply => Number(multiply(value.drop(1)))
        case Print => print(value.drop(1))
      }
   }
  }

  /** Expands macrodefinitions in given expression.
    * 
    * You should implement this method according to the specification written in
    * `README.md`.
    */
  def macroexpand(expr: Expression): Expression = ???
}
