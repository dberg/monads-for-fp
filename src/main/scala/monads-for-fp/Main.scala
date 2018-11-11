package monadsforfp

/** Based on Monads for functional programming:
  * http://homepages.inf.ed.ac.uk/wadler/papers/marktoberdorf/baastad.pdf
  */
object Main {

  def main(args: Array[String]): Unit = {
    // Intro
    VariationZeroBasicEval.run()
    VariationOneExceptions.run()
    VariationTwoState.run()
    VariationThreeOutput.run()

    // Laws
    Laws.run()

    // Arrays
    Arrays.run()
  }

}
