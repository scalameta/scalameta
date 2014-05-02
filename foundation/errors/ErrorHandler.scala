package org.scalareflect.errors

import scala.language.higherKinds
import scala.util.{Try, Success => TrySuccess, Failure => TryFailure}

trait ErrorHandler {
  type Result[+S, +E]
  implicit class MonadicOps[+S, +E](r: Result[S, E]) {
    def map[S1](f: S => S1): Result[S1, E] = ErrorHandler.this.map(r, f)
    def flatMap[S1, E1](f: S => Result[S1, E1]): Result[S1, E1] = ErrorHandler.this.flatMap(r, f)
  }
  type Success[+S] <: Result[S, Nothing]
  type Failure[+E] <: Result[Nothing, E]
  def succeed[S](x: S): Success[S]
  def fail[E <: Exception](x: E): Failure[E]
  def map[S, S1, E](r: Result[S, E], f: S => S1): Result[S1, E]
  def flatMap[S, S1, E, E1](r: Result[S, E], f: S => Result[S1, E1]): Result[S1, E1]
}

object handlers {
  implicit object throwExceptions extends ErrorHandler {
    type Result[+S, +E] = S
    type Success[+S] = S
    type Failure[+E] = Nothing
    def succeed[S](x: S) = x
    def fail[E <: Exception](x: E) = throw x
    def map[S, S1, E](r: Result[S, E], f: S => S1): Result[S1, E] = f(r)
    def flatMap[S, S1, E, E1](r: Result[S, E], f: S => Result[S1, E1]): Result[S1, E1] = f(r)
  }

  implicit object returnTries extends ErrorHandler {
    type Result[+S, +E] = Try[S]
    type Success[+S] = TrySuccess[S]
    type Failure[+E] = TryFailure[Nothing]
    def succeed[S](x: S) = TrySuccess[S](x)
    def fail[E <: Exception](x: E) = TryFailure[Nothing](x)
    def map[S, S1, E](r: Result[S, E], f: S => S1): Result[S1, E] = r.map(f)
    def flatMap[S, S1, E, E1](r: Result[S, E], f: S => Result[S1, E1]): Result[S1, E1] = r.flatMap(f)
  }
}