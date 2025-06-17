package quantum

import spire.math.Complex
import spire.implicits._
import scala.util.{Try, Success, Failure}

// Error types for quantum state creation
sealed trait QuantumStateError
object QuantumStateError {
  case class InvalidNormalization(actual: Double, expected: Double = 1.0) extends QuantumStateError
  case class InvalidBitStringLength(bitString: BitString, expectedLength: Int) extends QuantumStateError
  case class EmptyState() extends QuantumStateError
  case class InvalidAmplitude(amplitude: Complex[Double], reason: String) extends QuantumStateError
  case class MismatchedQubitCount(expected: Int, actual: Int) extends QuantumStateError
}

// The main quantum state representation
case class QuantumState private(
                                 amplitudes: Map[BitString, Complex[Double]],
                                 numQubits: Int
                               ) {
  // Performance-critical unsafe operations for inner loops
  def unsafeAmplitudes: Map[BitString, Complex[Double]] = amplitudes
  def unsafeNumQubits: Int = numQubits

  // Safe accessor methods
  def amplitude(bitString: BitString): Complex[Double] =
    amplitudes.getOrElse(bitString, Complex.zero[Double])

  def probability(bitString: BitString): Double =
    amplitude(bitString).abs.pow(2)

  def totalProbability: Double =
    amplitudes.values.map(_.abs.pow(2)).sum

  override def toString: String = {
    val entries = amplitudes.toSeq.sortBy(_._1.bits)
      .map { case (bs, amp) => s"${bs.toString} -> $amp" }
      .mkString(", ")
    s"QuantumState($entries)"
  }
}

object QuantumState {
  import QuantumStateError._

  // Smart constructor - validates state
  def apply(
             amplitudes: Map[BitString, Complex[Double]],
             numQubits: Int
           ): Either[QuantumStateError, QuantumState] = {

    // Validation checks
    for {
      _ <- validateNonEmpty(amplitudes)
      _ <- validateQubitCount(amplitudes, numQubits)
      _ <- validateBitStringLengths(amplitudes, numQubits)
      _ <- validateAmplitudes(amplitudes)
      _ <- validateNormalization(amplitudes)
    } yield new QuantumState(amplitudes, numQubits)
  }

  // Unsafe constructor for performance-critical code
  def unsafe(amplitudes: Map[BitString, Complex[Double]], numQubits: Int): QuantumState =
    new QuantumState(amplitudes, numQubits)

  // Helper validation functions
  private def validateNonEmpty(amplitudes: Map[BitString, Complex[Double]]): Either[QuantumStateError, Unit] =
    if (amplitudes.nonEmpty) Right(()) else Left(EmptyState())

  private def validateQubitCount(amplitudes: Map[BitString, Complex[Double]], numQubits: Int): Either[QuantumStateError, Unit] = {
    val maxExpectedStates = 1L << numQubits
    if (amplitudes.size <= maxExpectedStates) Right(())
    else Left(MismatchedQubitCount(numQubits, amplitudes.size))
  }

  private def validateBitStringLengths(amplitudes: Map[BitString, Complex[Double]], numQubits: Int): Either[QuantumStateError, Unit] = {
    amplitudes.keys.find(_.length != numQubits) match {
      case Some(invalidBitString) => Left(InvalidBitStringLength(invalidBitString, numQubits))
      case None => Right(())
    }
  }

  private def validateAmplitudes(amplitudes: Map[BitString, Complex[Double]]): Either[QuantumStateError, Unit] = {
    amplitudes.values.find(amp => amp.real.isNaN || amp.imag.isNaN || amp.real.isInfinity || amp.imag.isInfinity) match {
      case Some(invalidAmp) => Left(InvalidAmplitude(invalidAmp, "Contains NaN or Infinity"))
      case None => Right(())
    }
  }

  private def validateNormalization(amplitudes: Map[BitString, Complex[Double]]): Either[QuantumStateError, Unit] = {
    val totalProb = amplitudes.values.map(_.abs.pow(2)).sum
    val tolerance = 1e-10
    if (math.abs(totalProb - 1.0) < tolerance) Right(())
    else Left(InvalidNormalization(totalProb))
  }

  // Convenience constructors
  def zero(numQubits: Int): Either[QuantumStateError, QuantumState] = {
    val zeroState = BitString.zero(numQubits)
    apply(Map(zeroState -> Complex.one[Double]), numQubits)
  }

  def one(numQubits: Int): Either[QuantumStateError, QuantumState] = {
    val oneState = BitString((1L << numQubits) - 1, numQubits)  // All bits set to 1
    apply(Map(oneState -> Complex.one[Double]), numQubits)
  }
}
