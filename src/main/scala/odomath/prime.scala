package odomath

import collection.mutable

/**
 * Created with IntelliJ IDEA.
 * User: Oleg
 * Date: 07.10.12
 * Time: 13:06
 * To change this template use File | Settings | File Templates.
 */
package object prime {
  /**
   * generate ascending list of prime numbers
   * @param bound upper bound of generated number
   * @return Indexed sequence of prime numbers
   */
  def genPrimes(bound: Int) = {
    val sieve = mutable.BitSet()

    def findPrimes(num: Int, primes: List[Int]): List[Int] = {
      def collectPrimes(num: Int, primes: List[Int]): List[Int] = {
        if (num > bound) primes
        else collectPrimes(num + 2, if (sieve(num)) primes else num :: primes)
      }
      if (num * num > bound) collectPrimes(num, primes)
      else findPrimes(num + 2,
        if (sieve(num)) primes
        else {
          for (i <- num * num to bound by (num * 2)) sieve += i
          num :: primes
        })
    }

    findPrimes(3, List(2)).reverse.toIndexedSeq
  }

  def genFactorizations[X](handle: (Int /*last number */ , Int /*last prime*/ , Int /*new prime*/ , X /*last result*/ ) => X /*new result*/ ,
                           default: X)
                          (bound: Int)(primeBound: Int = bound) {
    val primes = genPrimes(primeBound)

    type MeshElem = (Int /*number*/ , Int /*last prime index*/ , X /*result*/ )
    type Mesh = List[MeshElem]

    def produce(mesh: Mesh): Unit = if (!mesh.isEmpty) {
      val (num, lastPIndex, x) :: nextMesh = mesh;
      val mulBound = bound / num
      def handleResult(pIndex: Int, mesh: Mesh): Mesh = {
        if (pIndex == primes.size) mesh
        else {
          val prime = primes(pIndex)
          if (prime > mulBound) mesh
          else
            handleResult(pIndex + 1, (num * prime, pIndex, handle(num, primes(lastPIndex), prime, x)) :: mesh)
        }
      }

      produce(handleResult(lastPIndex, nextMesh))
    }

    produce((0 until primes.length) map (i => (primes(i), i, handle(1, 1, primes(i), default))) toList)
  }


  //Pure functional implementation
  def genFactorizationsFunc[X, V](handle: (Int, Int, Int, X, V) => (X, V), default: X, start: V)
                                 (bound: Int)(primeBound: Int = bound): V = {
    val primes = genPrimes(primeBound)

    type MeshElem = (Int /*number*/ , Int /*last prime index*/ , X /*result*/ )
    type Mesh = List[MeshElem]

    def produce(mesh: Mesh, v: V): V = if (!mesh.isEmpty) {
      val (num, lastPIndex, x) :: nextMesh = mesh;
      val mulBound = bound / num
      def handleResult(pIndex: Int, mesh: Mesh, v: V): (Mesh, V) = {
        if (pIndex == primes.size) (mesh, v)
        else {
          val prime = primes(pIndex)
          if (prime > mulBound) (mesh, v)
          else {
            val (iterX, iterV) = handle(num, primes(lastPIndex), prime, x, v)
            handleResult(pIndex + 1, (num * prime, pIndex, iterX) :: mesh, iterV)
          }
        }
      }
      val (iterMesh, iterV) = handleResult(lastPIndex, nextMesh, v)
      produce(iterMesh, iterV)
    } else v
    def primesMesh(i: Int, mesh: Mesh, v: V): (Mesh, V) = if (i == primes.length) (mesh, v)
    else {
      val (x, iterV) = handle(1, 1, primes(i), default, v)
      primesMesh(i + 1, (primes(i), i, x) :: mesh, iterV)
    }
    val (startMesh, startV) = primesMesh(0, Nil, start)
    produce(startMesh, startV)
  }

  type HandleStepTuple[X, V] = (Int, Int, Int, X, V)

  trait FactorizationStep[X, V] {
    def stepTuple: HandleStepTuple[X, V]

    val (lastNum, lastPrime, prime, feature, value) = stepTuple

    def num = lastNum * prime

    def newFeature: X

    def newValue: V
  }


  type HandleStep[X, V] = (HandleStepTuple[X, V]) => _ <: FactorizationStep[X, V] with Object

  def genFacts[X, V](handleStep: HandleStep[X, V], default: X, start: V)(bound: Int)
  = genFactorizationsFunc((lastNum, lastPrime, prime, feature: X, value: V) => {
    val step = handleStep((lastNum, lastPrime, prime, feature, value))
    (step.newFeature, step.newValue)
  }, default, start)(bound)()

}
