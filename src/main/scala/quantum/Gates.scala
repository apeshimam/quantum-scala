// src/main/scala/quantum/Gates.scala
package quantum

import spire.math.Complex
import spire.implicits._

// Base trait for all quantum gates
trait Gate {
  def apply(state: QuantumState): QuantumState
  def name: String
}

// Circuit composition
case class Circuit(gates: List[Gate]) extends Gate {
  def apply(state: QuantumState): QuantumState =
    gates.foldLeft(state)((s, gate) => gate(s))

  def name: String = gates.map(_.name).mkString(" → ")
}

object Circuit {
  def apply(gates: Gate*): Circuit = Circuit(gates.toList)
}

// X Gate (NOT/Pauli-X) - flips a qubit
case class X(qubit: Int) extends Gate {
  def name: String = s"X($qubit)"

  def apply(state: QuantumState): QuantumState = {
    val newAmplitudes = state.unsafeAmplitudes.map { case (bitString, amplitude) =>
      val newBitString = bitString.flip(qubit)
      newBitString -> amplitude
    }
    QuantumState.unsafe(newAmplitudes, state.unsafeNumQubits)
  }
}

// Hadamard Gate - creates superposition
case class H(qubit: Int) extends Gate {
  def name: String = s"H($qubit)"

  def apply(state: QuantumState): QuantumState = {
    // This is the complex one - it maps one amplitude to two
    val newAmplitudes = collection.mutable.Map[BitString, Complex[Double]]()
    val sqrt2 = math.sqrt(2.0)

    state.unsafeAmplitudes.foreach { case (bitString, amplitude) =>
      // Each bitstring becomes two: one with qubit=0, one with qubit=1  
      val bit0String = bitString.updated(qubit, 0)
      val bit1String = bitString.updated(qubit, 1)

      val coeff0 = amplitude / sqrt2
      val coeff1 = if (bitString(qubit) == 0) {
        amplitude / sqrt2  // positive 
      } else {
        -amplitude / sqrt2  // negative (this creates interference)
      }

      // Accumulate amplitudes (in case of overlap)
      newAmplitudes(bit0String) = newAmplitudes.getOrElse(bit0String, Complex.zero[Double]) + coeff0
      newAmplitudes(bit1String) = newAmplitudes.getOrElse(bit1String, Complex.zero[Double]) + coeff1
    }

    QuantumState.unsafe(newAmplitudes.toMap, state.unsafeNumQubits)
  }
}

// CX Gate (CNOT) - controlled NOT
case class CX(control: Int, target: Int) extends Gate {
  def name: String = s"CX($control,$target)"

  def apply(state: QuantumState): QuantumState = {
    val newAmplitudes = state.unsafeAmplitudes.map { case (bitString, amplitude) =>
      val newBitString = if (bitString(control) == 1) {
        // If control is 1, flip the target
        bitString.flip(target)
      } else {
        // If control is 0, leave unchanged
        bitString
      }
      newBitString -> amplitude
    }
    QuantumState.unsafe(newAmplitudes, state.unsafeNumQubits)
  }
}

// S Gate - phase gate (multiplies |1⟩ amplitude by i)
case class S(qubit: Int) extends Gate {
  def name: String = s"S($qubit)"

  def apply(state: QuantumState): QuantumState = {
    val newAmplitudes = state.unsafeAmplitudes.map { case (bitString, amplitude) =>
      val newAmplitude = if (bitString(qubit) == 1) {
        amplitude * Complex.i[Double]  // multiply by i
      } else {
        amplitude  // leave unchanged
      }
      bitString -> newAmplitude
    }
    QuantumState.unsafe(newAmplitudes, state.unsafeNumQubits)
  }
}

// T Gate - π/8 phase gate
case class T(qubit: Int) extends Gate {
  def name: String = s"T($qubit)"

  def apply(state: QuantumState): QuantumState = {
    val tPhase = Complex(1.0/math.sqrt(2), 1.0/math.sqrt(2))  // (1+i)/√2 = e^(iπ/4)

    val newAmplitudes = state.unsafeAmplitudes.map { case (bitString, amplitude) =>
      val newAmplitude = if (bitString(qubit) == 1) {
        amplitude * tPhase
      } else {
        amplitude
      }
      bitString -> newAmplitude
    }
    QuantumState.unsafe(newAmplitudes, state.unsafeNumQubits)
  }
}