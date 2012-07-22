package edu.jwang.reedsolomon

import java.io.PrintWriter
import java.util.Random

/**
 * Simulate transmission of RS codewords over a noisy channel to determine the
 * error rate for various parameters.
 */
object Simulation extends App {
  val NumPsValues = 200
  val MaxPs = 0.5
  val MinPs = 0.001
  val MaxNumTrials = 10000
  val MinFail = 100
  val MinCorrect = 100
  val rnd = new Random

  // for calculation of ps values
  // make uniform on log scale
  val base = math.pow(MaxPs / MinPs, 1. / NumPsValues)
  val constant = MinPs

  val psValues = (0 until NumPsValues) map { constant * math.pow(base, _) }

  for (t <- List(3, 7)) { // two different t
    printf("t = %d%n", t)
    val printWriter = new PrintWriter(t + ".csv")
    val rs = new ReedSolomon(GF32.Alpha, 31, t, GF32.Zero, GF32.One)

    // parallel loop over all Ps
    psValues.par foreach { ps =>
      var numFail = 0
      var numCorrect = 0
      var num = 0

      while (num < MaxNumTrials
        && (numFail < MinFail || numCorrect < MinCorrect)) {
        // This generates different random numbers, not just the same thing
        // filled over and over
        val msg = Vector.fill(rs.k)(GF32(rnd.nextInt(32)))
        val codeword = rs.encode(msg)
        val errors = Vector.fill(rs.n)(
          if (rnd.nextDouble() < ps) GF32(rnd.nextInt(31) + 1)
          else GF32.Zero)
        val numErrors = errors.count(!_.isZero)

        val corruptedCodeword = (codeword zip errors) map { t => t._1 + t._2 }

        val decodedMsg = rs.decode(corruptedCodeword)
        if (decodedMsg == null || decodedMsg != msg) {
          if (numErrors <= t) // should have been able to correct
            throw new AssertionError
          numFail += 1
        } else
          numCorrect += 1
        num += 1
      }
      printWriter.synchronized {
        printWriter.println("%f,%f,%d,%d".format(
          ps, numFail / num.asInstanceOf[Double], numFail, numCorrect))
      }
      //printf("Ps = %.5f%n", ps)
    }
    printWriter.close()
  }

  printf("Time: %.1f s%n", (System.currentTimeMillis - executionStart) / 1000.)
}