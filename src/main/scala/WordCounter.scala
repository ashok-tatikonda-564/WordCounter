import WordCounterActor.StartMatchingProcessForLine
import akka.actor.{Actor, ActorRef, ActorSystem, Props}

import scala.io.Source.fromFile

case class MatchWordInLine(word: String, inputLine: String)

case class WordMatchStatus(found: Boolean)

class WordMatcherActor extends Actor {

  def receive = {
    case MatchWordInLine(word, inputLine) => {

      val wordLength = word.length

      val possibleScrambledWords = for {
        i <- 0 until inputLine.length
        if (inputLine(i) == word.head)
        if (i + wordLength - 1 < inputLine.length)
        if (inputLine(i + wordLength - 1) == word.last)
      } yield inputLine.slice(i, i + wordLength)

      val exists = possibleScrambledWords exists {
        isScrambledString(_, word)
      }
      sender ! WordMatchStatus(exists)
    }
    case _ => println("Error: message not recognized")
  }

  def isScrambledString(scrambledWord: String, originalWord: String): Boolean = {

    if (scrambledWord == originalWord) true
    else {
      val scrambledWordCharCountsMap = charsCounterInString(scrambledWord.drop(1).dropRight(1))
      val originalWordCharCountsMap = charsCounterInString(originalWord.drop(1).dropRight(1))
      (originalWordCharCountsMap -- scrambledWordCharCountsMap.keySet).isEmpty
    }

  }

  def charsCounterInString(str: String) = {
    str.foldLeft(Map[Char, Int]() withDefaultValue 0) { (acc, c) => acc updated(c, acc(c) + 1) }
  }
}



object WordCounterActor {
  def props(fileName:String) = Props(classOf[WordCounterActor], fileName)
  case class StartMatchingProcessForLine(inputLine: String)
}

class WordCounterActor(filename: String) extends Actor {

  private var running = false
  private var totalLines = 0
  private var linesProcessed = 0
  private var result = 0
  private var fileSender: Option[ActorRef] = None

  def receive = {
    case StartMatchingProcessForLine(inputLine) => {
      if (running) {
        println("Warning: duplicate start message received")
      } else {
        running = true
        fileSender = Some(sender) // save reference to process invoker
        import scala.io.Source._
        fromFile(filename).getLines.foreach { line =>
          context.actorOf(Props[WordMatcherActor]) ! MatchWordInLine(line, inputLine)
          totalLines += 1
        }
      }
    }
    case WordMatchStatus(found) => {
      if (found) result += 1
      linesProcessed += 1
      if (linesProcessed == totalLines) {
        fileSender.map(_ ! result)
      }
    }
    case _ => println("message not recognized!")
  }
}

object SelfTest {

  import akka.dispatch.ExecutionContexts._
  import akka.pattern.ask
  import akka.util.Timeout

  import scala.concurrent.duration._
  import scala.language.postfixOps

  implicit val ec = global

  def main(args: Array[String]) {
    val system = ActorSystem("System")
    val iLines = fromFile("/home/developer/Desktop/WordCounter/src/main/resources/inputFile").getLines.toList
    iLines foreach { line =>
      val actor = system.actorOf(Props(new WordCounterActor("/home/developer/Desktop/WordCounter/src/main/resources/dictionaryFile")))
      implicit val timeout = Timeout(25 seconds)
      val future = actor ? StartMatchingProcessForLine(line)
      future.map { result =>
        println(s"Case #${iLines.indexOf(line) + 1} : $result")
      }
    }

    system.terminate()
  }
}
