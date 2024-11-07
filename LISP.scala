// It's important to keep this import, since default available `Map` type
// defined in Predef.scala is an alias for `scala.immutable.Map`.
//
// Therefore, with this import we make it possible to assign
// instances of both `mutable.Map` and `immutable.Map` to the
// variables of type `Map` (`scala.collection.Map`).

import LISP.Expression
import LISP.Expression.*

import scala.annotation.tailrec
import scala.collection.Map


/** LISP -- LISP Interpreter for Scala Programmers.
 *
 * You should finish the implementation of this module according
 * to the reference described in `README.md` to complete this assignment.
 */
object LISP {

  private def printCmd(args: Seq[Expression]): Expression = {
    @tailrec
    def innerPrint(arg: Expression): Unit = {
      arg match {
        case Number(value) => Predef.print(value)
        case String(value) => print(value)
        case Symbol(value) => print(value)
        case List(value) => printCmd(value)
        case Quote(value) => innerPrint(value)
        case Plus => print("<plus>")
        case Multiply => print("<minus>")
        case Print => print("<print>")
        case Lambda(args, body, env) => print("<lambda>")
      }
    }

    args.foreach(* => innerPrint(*))
    Expression.List(Seq.empty)
  }

  private def equals(args: Seq[Expression]): Expression = {
    if (args.length != 2) throw RuntimeException("Too many arguments fro function equals")
    if (args.head.equals(args(1))) return Expression.String("t") else Expression.List(Seq.empty)
  }

  private def plus(numbers: Seq[Expression]): Expression = {
    var s = 0.0
    for (x <- numbers) s += x.asInstanceOf[Number].value
    Number(s)
  }

  private def multiply(numbers: Seq[Expression]): Expression = {
    var s = 1.0
    for (x <- numbers) s *= x.asInstanceOf[Number].value
    Number(s)
  }

  private def lambda(args: Seq[Expression], body: Expression, environment: Environment, value: Seq[Expression]) = {
    var tmp = environment
    if (value.length != args.length) {
      throw RuntimeException(s"Different length of args!")
    }
    for (i <- 0 until args.length) {
      tmp += (args(i).asInstanceOf[Symbol] -> evaluate(value(i), environment))
    }
    evaluate(body, tmp)
  }

  private def create_lambda(args: Seq[Expression], environment: Environment): Expression = {
    if (args.length != 2) throw RuntimeException("Lambda has too many arguments") // TODO: Maybe some inheritance??
    val first = args.head.asInstanceOf[Expression.List].value
    var lambda_args = scala.collection.immutable.List[Symbol]()
    for (i <- 0 to first.length) {
      lambda_args = lambda_args.appended(first(i).asInstanceOf[Symbol])
    }
    Expression.Lambda(lambda_args, args(1), environment)
  }

  private def create_if(args: Seq[Expression]): Expression = {
    if (args.length != 3) throw RuntimeException("Too many arguments for If")
    if (args.head == List(Seq.empty)) {
      args(2)
    } else {
      args(1)
    }
  }

  private def define(args: Seq[Expression]): Expression = {
    if (args.length != 1) throw RuntimeException("To many arguments for DEFINE")
    args.head
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
     * (not `List(Seq.empty)`).
     * 3. `Quote(List(Seq(Symbol("+"), Number(21.0), Number(21.0))))`
     * should be evaluated to
     * `(List(Seq(Symbol("+"), Number(21.0), Number(21.0)))`,
     * *not* `Number(42.0)`.
     */
    case Quote(value: Expression)

    /** Function with given `params`, `body` and associated lexical scope `env`.
     */
    case Lambda(params: Seq[Expression.Symbol], body: Expression, env: Environment)

    case Plus
    case Multiply
    case Print
    case Lam
    case Equals
    case If
    case Define
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
    var nested: Map[Expression.Symbol, Expression] = Map(
      "nil".s -> l(),
      "*".s -> Multiply,
      "+".s -> Plus,
      "==".s -> Equals,
      "print".s -> Print,
      "lambda".s -> Lam,
      "if".s -> If,
      "define".s -> Define

      /** Implement other definitions here. */)
    nested += ("quote".s -> lam(Seq("x".s), "x".s.q, nested))
    nested += ("nil?".s -> lam(Seq("x".s), l("==".s, "x".s, l().q), nested))
    nested += ("not".s -> lam(Seq("x".s), l("if".s, l("nil?".s, "x"), "t", "nil".s), nested))
    nested
  }

  /** Evaluates some expression `expr` knowing about given environment `env`.
   *
   * You should implement this method according to the specification written in
   * `README.md`.
   */
  def evaluate(expr: Expression, environment: Environment = globalEnvironment): Expression = {
    var env = environment
    expr match {
      case String(_) => expr
      case Number(_) => expr
      case Print => expr
      case Plus => expr
      case Multiply => expr
      case Lam => expr // TODO: Remove all this cases
      case Equals => expr
      case Lambda(_, _, _) => expr
      case Quote(value) => value
      case Symbol(value) => env(Symbol(value))
      case List(value) =>
        if (value.isEmpty) {
          return expr
        }
        var x = scala.collection.immutable.List[Expression]()
        x = x.appended(evaluate(value.head, env))
        for (y <- value) {
          x = x.appended(evaluate(y, env))
        }
        val head = x.head
        head match {
          case Plus => plus(x.drop(1))
          case Multiply => multiply(x.drop(1))
          case Equals => equals(x.drop(1))
          case Lam => create_lambda(x.drop(1), env)
          case If => create_if(x.drop(1))
          case Define => env += (head.asInstanceOf[Symbol] -> define(x.drop(1))); Expression.List(Seq.empty)
          case Print => printCmd(x.drop(1))
          case Lambda(args, body, env) => lambda(args, body, env, value.drop(1))
          case Expression.String(_) => ???
          case Expression.Number(_) => ???
          case Expression.Symbol(_) => ???
          case Expression.List(_) => ???
          case Expression.Quote(_) => ???

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
