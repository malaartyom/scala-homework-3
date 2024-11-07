import scala.language.implicitConversions

object LispDSL {

  /** Constructs List expression from given expressions. */
  def l(exprs: Expression*): Expression = Expression.List(exprs)

  def lam(args: Seq[Expression.Symbol], body: Expression, env: Environment) = Expression.Lambda(args, body, env)

  extension (expr: Expression) {
    /** Quotes given expression. */
    def q: Expression.Quote = Expression.Quote(expr)
  }

  extension (str: String) {
    /** Creates symbol expression from given string. */
    def s: Expression.Symbol = Expression.Symbol(str)
  }


  // Implicit conversions from Double and String to Expression.Number and Expression.String respectively.
  implicit def doubleToExpression(value: Double): Expression.Number = Expression.Number(value)
  implicit def stringToExpression(value: String): Expression.String = Expression.String(value)
  
}

object TestingDSL {
  def test(name: String, disabled: Boolean = false)(action: => Unit) = try {
    print(s"Test \"$name\" ")
    if (disabled) {
      println(s"is disabled. Remove \", disabled = true\" at the test definition to enable it.")
    } else {
      action
      println("passed.")
    }

  } catch {
    case x =>
      println("failed.")
      x.printStackTrace()
      println()
  }

  def shouldFail(action: => Unit) = {
    var failed = false
    try action catch {
      case _ => failed = true
    }
    assert(failed, "statement should fail with an exception")
  }

  def shouldPass(action: => Unit) = {
    var failed = false
    try action catch {
      case _ => failed = true
    }
    assert(!failed, "statement should not raise an exception")
  }
}

