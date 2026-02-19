package ch6

object GenericExcercise {

//  type State[S, +A] = S => (A, S)

  case class State[S, +A](run: S => (A, S))

  def unit[S, A](a: A): State[S, A] =
    State[S, A] { s => (a, s) }

  extension [S, A](action: State[S, A])

    def map[B](f: A => B): State[S, B] =
      State[S, B] { s =>
        val (content, nxtState) = action.run(s)
        (f(content), nxtState)
      }

    def map2[B, C](action2: State[S, B])(f: (A, B) => C): State[S, C] =
      State[S, C] { s =>
        val (content1, state1) = action.run(s)
        val (content2, state2) = action2.run(state1)
        (f(content1, content2), state2)
      }

    def flatMap[B](f: A => State[S, B]): State[S, B] =
      State { s =>
        val (content, nxtState) = action.run(s)
        f(content).run(nxtState)
      }

    def mapWFlatMap[B](f: A => B): State[S, B] =
      flatMap(a => unit(f(a)))

    def map2WFlatMap[B, C](action2: State[S, B])(f: (A, B) => C): State[S, C] =
      flatMap(a => action2.map(b => f(a, b)))
}
