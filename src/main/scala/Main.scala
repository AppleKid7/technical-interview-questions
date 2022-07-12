package example

import scala.annotation.tailrec

@main def hello: Unit = 
  println("Hello world!")
  println(msg)

def msg = "I was compiled by Scala 3. :)"

// object Hello extends Greeting with App {
//   println(greeting)
// }

// trait Greeting {
//   lazy val greeting: String = "hello"
// }

object challenge1 extends App {
  enum Tree[+A]:
    case Empty
    case Leaf(value: A)
    case Fork(left: Tree[A], right: Tree[A])

  def length[A](list: List[A]): Int =
    @tailrec
    def lengthHelper[A](l: List[A], acc: Int): Int =  
      list match {
        case Nil => acc
        case _ :: list => lengthHelper(list, acc + 1)
      }
    lengthHelper(list, 0)

  def fib(n: Int): Int =
    @tailrec
    def fibHelper(n: Int, prev: Int, current: Int): Int =
      if (n <= 0) current
      else fibHelper(n - 1,  prev + current, prev)
    fibHelper(n, 1, 0)

  /**
    * CHALLENGE
    * 
    * Define a lazy, stack-safe iterator for `Tree[A]`.
    */
  def toIterable[A](initial: Tree[A]): Iterable[A] = 
    new Iterable[A] {
      import Tree._
      
      // Step 1: Find simplest solution that is not stack-safe
      // Step 2: Find the core recursive function here and make it
      // tail-recursive.
      // override def foreach[U](f: A => U): Unit = {
      //   def loop(node: Tree[A]): Unit =
      //     node match {
      //       case Empty =>
      //       case Leaf(value) => f(value)
      //       case Fork(left, right) =>
      //         loop(left) // not tailrecursive because after calling loop, we're doing something else.
      //         loop(right)
      //     }
      //   loop(initial)
      // }

      // override def iterator: Iterator[A] =
      //   new Iterator[A] {
      //     override def hasNext: Boolean = ???
      //     override def next(): A = ???
      //   }

      // Depth-first
      @tailrec
      def loopDFS(todo: List[Tree[A]]): Unit =
        todo match
          case Nil => ()

          case Empty :: todo => loopDFS(todo)

          case Leaf(value) :: todo =>
            println(value)
            loopDFS(todo)

          case Fork(left, right) :: todo =>
            loopDFS(left :: right :: todo)

      @tailrec
      def loopBFS(todo: List[Tree[A]]): Unit =
        todo match
          case Nil => ()

          case Empty :: todo => loopBFS(todo)

          case Leaf(value) :: todo =>
            println(value)
            loopBFS(todo)
          
          case Fork(left, right) :: todo =>
            loopBFS(left +: todo :+ right)

      // fixed Depth-First solution:
      def foreach2[U](f: A => U): Unit =
        @tailrec
        def loop(node: Tree[A], todo: List[Tree[A]]): Unit = // this is O(n) where n is the number of nodes in the tree
          node match
            case Empty =>
              todo match
                case Nil => ()
                case node :: todo => loop(node, todo)
              // nextNode match {
              //   case None => ()
              //   case Some(node) => loop(node, None)
              // }
              // nextNode.foreach(loop(_, None))

            case Leaf(value) => 
              f(value)
              todo match
                case Nil => ()
                case node :: todo => loop(node, todo)

            case Fork(left, right) => loop(left, right :: todo)
        loop(initial, Nil)

      override def foreach[U](f: A => U): Unit =
        @tailrec
        def loop(todo: List[Tree[A]]): Unit = // this is O(n) where n is the number of nodes in the tree
          todo match
            case Nil => ()

            case Empty :: todo => loop(todo)

            case Leaf(value) :: todo =>
              f(value)
              loop(todo)

            case Fork(left, right) :: todo => loop(left :: right :: todo)
        loop(List(initial))

      override def iterator: Iterator[A] =
        new Iterator[A] {
          var _todo: List[Tree[A]] = List(initial)
          var _next: Option[A] = None

          advancedNext()

          override def hasNext: Boolean = _next.nonEmpty

          @tailrec
          def getNext(todo: List[Tree[A]]): Option[(A, List[Tree[A]])] = // this is O(n) where n is the number of nodes in the tree
            todo match
              case Nil => None

              case Empty :: todo => getNext(todo)

              case Leaf(value) :: todo =>
                Some((value, todo))

              case Fork(left, right) :: todo =>
                getNext(left :: right :: todo)

          def advancedNext(): Unit =
            getNext(_todo) match
              case None =>
                _todo = Nil
                _next = None
              case Some((a, todo)) =>
                _todo = todo
                _next = Some(a)

          override def next(): A =
            val a = _next.get
            advancedNext()
            a
        }
    }

  // 1, 2, 3
  val tree = Tree.Fork(Tree.Leaf(1), Tree.Fork(Tree.Leaf(2), Tree.Leaf(3)))

  val iterator = toIterable(tree).iterator

  //List(1, 2, 3).foreach(println(_))
  val it = List(1, 2, 3).iterator 

  it.next()

  iterator.hasNext // true 
  iterator.next()  // 1
  iterator.next()  // 2
  iterator.next()  // 3
}

object challenge2 extends App {
  /**
    * CHALLENGE 
    * 
    * Determine if the specified strings can be laid out end to end such that the final letter
    * of one string is equal to the initial letter of the next string.
    * 
    * "foo", "bar", "orb", "rob"
    * "foo", "orb", "bar", "rob"
    * 
    * "a" -> ('a', 'a')
    * "" -> we just discard these
    * 
    * (Char, Char)
    *
    */
  def circular(strings: List[String]): Boolean = {
    simplified(strings.map(_.toList).collect {
      case head :: Nil => (head, head) 
      case head :: (tail @ (_ :: _)) =>  (head, tail.last)
    })
  }

  // List[A]
  // size: n
  // List(_, _, _, _, ..., _, _, _) - n blanks
  // choices: n!
  // n * (n - 1) *  (n - 2) * ... 1 = n!
  // (A, B)
  // |A| * |B| 

  def interleave(x:Int, l:List[Int]): List[List[Int]] = {
    l match { 
      case Nil => List(List(x))
      case (head::tail) =>
        (x :: head :: tail) :: interleave(x, tail).map(head :: _)
    }
  }

  // list.zipwith(list.drop(1))
  // (1, 2, 3)
  // (1, 2)(2, 3)
  def permutations[A](list: List[A]): List[List[A]] =
    list match {
      case Nil => Nil
      case x :: Nil => ???
      case x :: xs => ???
    }

  def swapElements[A](list: List[A], first: Int, second: Int): List[A] = {
    list.updated(first,list(second)).updated(second,list(first))
  }
  def permutationsLoop[A](list: List[A], l: Int): List[List[A]] = {
    var elementsToPermute = list
    var lengthOfArray = l
    var generatedPermutations: List[List[A]] = Nil
    if(lengthOfArray == 1) {
      generatedPermutations = elementsToPermute :: generatedPermutations
      generatedPermutations
    }
    else {
      lengthOfArray = lengthOfArray - 1
      generatedPermutations = generatedPermutations ++ permutationsLoop(elementsToPermute, lengthOfArray)
      var i = 0
      while(i < lengthOfArray) {
        if(lengthOfArray % 2 == 0){
          elementsToPermute = swapElements(elementsToPermute, i, lengthOfArray)
        } else {
          elementsToPermute = swapElements(elementsToPermute, 0, lengthOfArray)
        }
        generatedPermutations = generatedPermutations ++ permutationsLoop(elementsToPermute, lengthOfArray)
        i += 1
      }
      generatedPermutations
    }
  }

    // list match
    //   case Nil => List(Nil)
    //   case x :: Nil => List(x)
    //   case x :: xs =>
        
        

  // private def toTuple(string: String): (Char, Char) = (string.charAt(0), string.charAt(string.length - 1))

  // we can throw away all the middle characters. 
  def simplified[A](pairs: List[(A, A)]): Boolean = {
    type Solution = List[(A, A)]

    // def findSolution(current: (A, A), workingSet: List[(A, A)]): List[Solution] = {
    //   if (workingSet.isEmpty) List(current :: Nil)
    //   else {
    //     val target = current._2
    //     val (matches, remainder) = workingSet.partition(_._1 == target)

    //     val prefixSolutions = matches.map(m => current :: m :: Nil)

    //     ???
    //   }
    // }
    def findSolution(remaining: List[(A, A)]): Set[Solution] = {
      remaining match {
        case Nil => Set()

        case x :: Nil => Set(x :: Nil)

        case (pair @ (_, x)) :: xs =>
          val subsolutions = findSolution(xs)

          def loop(pair: (A, A), prefix: List[(A, A)], suffix: List[(A, A)]): Set[Solution] = ???
            // prefix match {
            //   case Nil => 
            //     suffix match {
            //       case Nil => List(pair)
            //       case (p @ (x, _)) :: suffix2 if pair._2 == x =>
            //         List(prefix ++ List(pair) ++ suffix2) ++ loop(pair, prefix ++ List(p), suffix2)
            //       case p :: suffix2 => loop(pair, prefix ++ List(p), suffix2)
            //     }
            //   case x :: xs =>
            //     suffix match {
            //       case Nil if pair._1 == x._2
            //     }
            // }

          subsolutions.flatMap { solution =>
            loop(pair, Nil, solution)
            // List(1, 2, 3)
            // List((None, Some(1), 0), (Some(1), Some(2), 1), (Some(2), Some(3), 3))
            // solution.headOption match {
            //   case None => false

            //   case Some((y, _)) => x == y
            // }
          }  
          // }.map { solution =>
          //   pair :: solution
          // }
      }
    }

    // pairs match {
    //   case current :: rest => findSolution(current, rest).nonEmpty
    //   case Nil => true
    // }
    findSolution(pairs).nonEmpty
  }
}
