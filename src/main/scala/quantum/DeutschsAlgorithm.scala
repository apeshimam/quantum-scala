// src/main/scala/quantum/DeutschAlgorithm.scala
package quantum

import spire.math.Complex
import spire.implicits._

// Represents a boolean function f: {0,1} -> {0,1}
sealed trait BooleanFunction {
  def apply(input: Int): Int
  def name: String
  def isConstant: Boolean
}

object BooleanFunction {
  // Constant functions
  case object ConstantZero extends BooleanFunction {
    def apply(input: Int): Int = 0
    def name: String = "f(x) = 0"
    def isConstant: Boolean = true
  }

  case object ConstantOne extends BooleanFunction {
    def apply(input: Int): Int = 1
    def name: String = "f(x) = 1"
    def isConstant: Boolean = true
  }

  // Balanced functions
  case object Identity extends BooleanFunction {
    def apply(input: Int): Int = input
    def name: String = "f(x) = x"
    def isConstant: Boolean = false
  }

  case object Negation extends BooleanFunction {
    def apply(input: Int): Int = 1 - input
    def name: String = "f(x) = ¬x"
    def isConstant: Boolean = false
  }
}

// Oracle gate that implements f(x) as a quantum operation
// Transforms |x,y⟩ → |x, y ⊕ f(x)⟩
case class Oracle(f: BooleanFunction) extends Gate {
  def name: String = s"Oracle(${f.name})"

  def apply(state: QuantumState): QuantumState = {
    val newAmplitudes = state.unsafeAmplitudes.map { case (bitString, amplitude) =>
      val inputBit = bitString(0)        // First qubit is input
      val outputBit = bitString(1)       // Second qubit is output  
      val fResult = f(inputBit)
      val newOutputBit = outputBit ^ fResult  // XOR operation

      // Create new bitstring with transformed output
      val newBitString = bitString.updated(1, newOutputBit)
      newBitString -> amplitude
    }
    QuantumState.unsafe(newAmplitudes, state.unsafeNumQubits)
  }
}

// Measurement that returns the classical bit value
case class Measure(qubit: Int) {
  def apply(state: QuantumState): (Int, Double) = {
    val prob0 = state.unsafeAmplitudes
      .filter(_._1(qubit) == 0)
      .values
      .map(_.abs.pow(2))
      .sum

    val prob1 = 1.0 - prob0

    // For demonstration, return the most likely outcome
    if (prob0 > prob1) (0, prob0) else (1, prob1)
  }
}

object DeutschAlgorithm {

  def run(f: BooleanFunction, debug: Boolean = true): Boolean = {
    if (debug) println(s"\n=== Deutsch's Algorithm: ${f.name} ===")

    // Step 1: Start with |00⟩
    val state0 = QuantumState.zero(2).right.get
    if (debug) println(s"Step 1 - Initial state |00⟩: ${state0}")

    // Step 2: Apply X to output qubit → |01⟩  
    val state1 = X(1)(state0)
    if (debug) println(s"Step 2 - After X(1) → |01⟩: ${state1}")

    // Step 3: Apply H to both qubits
    val state2 = Circuit(H(0), H(1))(state1)
    if (debug) println(s"Step 3 - After H(0), H(1): ${state2}")

    // Step 4: Apply the oracle (function f)
    val state3 = Oracle(f)(state2)
    if (debug) println(s"Step 4 - After Oracle: ${state3}")

    // Step 5: Apply H to input qubit again
    val state4 = H(0)(state3)
    if (debug) println(s"Step 5 - After final H(0): ${state4}")

    // Step 6: Measure input qubit
    val (measurement, probability) = Measure(0)(state4)
    if (debug) {
      println(s"Step 6 - Measurement result: $measurement (probability: ${probability})")
      println(s"Prediction: ${if (measurement == 0) "CONSTANT" else "BALANCED"}")
      println(s"Actual: ${if (f.isConstant) "CONSTANT" else "BALANCED"}")
      val correct = (measurement == 0) == f.isConstant
      println(s"Result: ${if (correct) "✓ CORRECT" else "✗ WRONG"}")
    }

    // Return whether function is constant (measurement = 0)
    measurement == 0
  }

  def testAllFunctions(): Unit = {
    import BooleanFunction._
    val functions = List(ConstantZero, ConstantOne, Identity, Negation)

    println("Testing Deutsch's Algorithm on all possible functions:")
    functions.foreach { f =>
      val prediction = run(f, debug = true)
      println()
    }
  }
}

// Example usage and testing
object DeutschDemo extends App {
  import BooleanFunction._

  // Test individual function
  DeutschAlgorithm.run(ConstantZero)

  // Test all functions
  // DeutschAlgorithm.testAllFunctions()
}