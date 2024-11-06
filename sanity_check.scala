import TestingDSL.test
import LispDSL.*

@main def runSanityCheck = {
  import LISP.{Expression, macroexpand, evaluate as eval}

  /** This modules are defined in `utils.scala`. Look this file to find their implementation. */

  test("Evaluation") {
    // Trivial values construction
    assert(eval(42.0) == Expression.Number(42.0))
    assert(eval("hello") == Expression.String("hello"))

    // Symbolic access to global scope
    assert(eval("nil".s) == Expression.List(Seq.empty))

    // Trivial value quotation
    assert(eval(42.0) == eval(42.0.q))
    assert(eval("hello") == eval("hello".q))

    // Quotation of symbol produces symbol itself
    assert(eval("nil".s.q) == "nil".s)

    // Quotation of list produces list
    assert(eval(l("+".s, 21.0, 21.0).q) == l("+".s, 21.0, 21.0))

    // Quotation of quotation produces quotation
    assert(eval("hello".q.q) == "hello".q)

    // +begin List evaluation
    // Special forms
    assert(eval(l("+".s, 21.0, 21.0)) == Expression.Number(42.0))
    assert(eval(l("*".s, 21.0, 2)) == Expression.Number(42.0))
    assert(eval(l("print".s, "hello")) == Expression.List(Seq.empty))

    // Conditionals and predicates
    assert(eval(l("nil?".s, "nil".s)) == Expression.String("t"))
    assert(
      eval(
        l("not".s,
          l("nil?".s, "nil".s))) == Expression.List(Seq.empty))

    assert(
      eval(
        l("if".s,
          l("nil?".s, "nil".s),
          l("print".s, "true!"),
          l("print".s, "false!"))) == Expression.List(Seq.empty))

    assert(
      eval(
        l("if".s,
          l("not".s, l("nil?".s, "nil".s)),
          l("print".s, "true!"),
          l("print".s, "false!"))) == Expression.List(Seq.empty))

    // Functions
    assert(
      eval(
        l("lambda".s, l(),
          l("print".s, "hello"))).isInstanceOf[Expression.Lambda])

    assert(
      eval(
        l(
          l("lambda".s, l(),
            l("print".s, "hello")))) == Expression.List(Seq.empty))
    // +end List evaluation

    // More tests are on the way
  }

  test("Macroexpansion", disabled = true) {
    ??? // Tests are coming soon
  }

  test("Lazy sequences", disabled = true) {
    ??? // Tests are coming soon
  }

  test("Stackless recursion with trampolines", disabled = true) {
    ??? // Tests are coming soon
  }
}
