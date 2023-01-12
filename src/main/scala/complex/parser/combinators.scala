package complex.parser

import complex.tokenizer.Token

// Scala's parser combinators could work as well, but
// I'm intentionally implementing this ourselves just to see
// how it can work

sealed trait ParseResult[+A]
case class Success[+A](result: A, rest: List[Token]) extends ParseResult[A]
case class Failure(why: String) extends ParseResult[Nothing]

case class ~[+A, +B](_1: A, _2: B)

class ParseException(message: String) extends Exception(message)

trait Parser[+A] extends ((List[Token] => ParseResult[A])) {
  self =>

  // throws an exception if it failed to parse or tokens remained
  def parseWhole(tokens: List[Token]): A = {
    self.apply(tokens) match {
      case Success(a, Nil) => a
      case Success(_, head :: _) =>
        throw new ParseException("Remaining tokens starting with: " + head)
      case Failure(message) => throw new ParseException(message)
    }
  }
  
  def flatMap[B](f: A => Parser[B]): Parser[B] = {
    new Parser[B] {
      def apply(tokens: List[Token]): ParseResult[B] = {
        self(tokens) match {
          case Success(a, rest) => f(a)(rest)
          case f@Failure(_) => f
        }
      }
    }
  }

  def map[B](f: A => B): Parser[B] = {
    new Parser[B] {
      def apply(tokens: List[Token]): ParseResult[B] = {
        self(tokens) match {
          case Success(a, rest) => Success(f(a), rest)
          case f@Failure(_) => f
        }
      }
    }
  }

  def ~[B](other: => Parser[B]): Parser[~[A, B]] = {
    lazy val evaluatedOther = other
    self.flatMap(a =>
      other.map(b =>
        new ~(a, b)))
  }

  def |[B >: A](other: => Parser[B]): Parser[B] = {
    lazy val evaluatedOther = other
    new Parser[B] {
      def apply(tokens: List[Token]): ParseResult[B] = {
        self(tokens) match {
          case s@Success(_, _) => s
          case Failure(_) => evaluatedOther(tokens)
        }
      }
    }
  }

  def ^^[B](f: A => B): Parser[B] = map(f)

  def ^^^[B](b: => B): Parser[B] = {
    lazy val evaluatedB = b
    map(_ => evaluatedB)
  }
} // Parser

object Parser {
  def lift[A](pf: PartialFunction[Token, A], expectedName: String): Parser[A] = {
    new Parser[A] {
      def apply(tokens: List[Token]): ParseResult[A] = {
        tokens match {
          case head :: tail if pf.isDefinedAt(head) => {
            Success(pf(head), tail)
          }
          case head :: _ => Failure("Expected " + expectedName + "; found: " + head)
          case Nil => Failure("Out of tokens")
        }
      }
    }
  } // lift

  def success[A](around: A): Parser[A] = {
    new Parser[A] {
      def apply(tokens: List[Token]): ParseResult[A] = {
        Success(around, tokens)
      }
    }
  }

  def failure[A](errorMessage: String): Parser[A] = {
    new Parser[A] {
      def apply(tokens: List[Token]): ParseResult[A] = {
        Failure(errorMessage)
      }
    }
  }

  def token(t: Token): Parser[Unit] = {
    lift( { case `t` => () }, t.toString)
  } // token

  def opt[A](p: => Parser[A]): Parser[Option[A]] = {
    lazy val evaluatedP = p
    new Parser[Option[A]] {
      def apply(tokens: List[Token]): ParseResult[Option[A]] = {
        evaluatedP(tokens) match {
          case Success(a, rest) => Success(Some(a), rest)
          case Failure(_) => Success(None, tokens)
        }
      }
    }
  } // opt

  // p*
  def rep[A](p: => Parser[A]): Parser[List[A]] = {
    lazy val evaluatedP = p
    new Parser[List[A]] {
      @scala.annotation.tailrec
      def inner(tokens: List[Token], accum: List[A]): (List[Token], List[A]) = {
        evaluatedP(tokens) match {
          case Success(a, rest) => inner(rest, a :: accum)
          case Failure(_) => (tokens, accum.reverse)
        }
      }

      def apply(tokens: List[Token]): ParseResult[List[A]] = {
        val (rest, as) = inner(tokens, List())
        Success(as, rest)
      }
    }
  } // rep

  def filter[A](p: => Parser[A])(predicate: A => Boolean, errorMessage: String): Parser[A] = {
    lazy val evaluatedP = p
    new Parser[A] {
      def apply(tokens: List[Token]): ParseResult[A] = {
        evaluatedP(tokens) match {
          case s@Success(a, _) if predicate(a) => s
          case _ => Failure(errorMessage)
        }
      }
    }
  } // filter

  def rep1[A](p: => Parser[A], errorMessage: String): Parser[List[A]] = {
    filter(rep(p))(_.nonEmpty, errorMessage)
  } // rep1

  def repsep[A](p: => Parser[A], delim: => Parser[Any]): Parser[List[A]] = {
    opt(p ~ rep(delim ~ p)) ^^ ((op) => op.map( { case a ~ list => a :: list.map(_._2) }).getOrElse(List()))
  } // repsep

  def rep1sep[A](p: => Parser[A], delim: => Parser[Any], errorMessage: String): Parser[List[A]] = {
    filter(repsep(p, delim))(_.nonEmpty, errorMessage)
  } // rep1sep
}
