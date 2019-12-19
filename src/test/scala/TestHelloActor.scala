import akka.actor.{ActorSystem, Props}
import akka.dispatch.ExecutionContexts._
import akka.pattern.ask
import akka.testkit.{ImplicitSender, TestKit}
import akka.util.Timeout
import org.scalatest.{BeforeAndAfterAll, Matchers, WordSpecLike}
import scala.concurrent.Await
import scala.concurrent.duration._
import scala.language.postfixOps

class TestWordCounter()
  extends TestKit(ActorSystem("TestWordCounter"))
    with ImplicitSender
    with WordSpecLike
    with Matchers
    with BeforeAndAfterAll {

  implicit val timeout = Timeout(25 seconds)
  implicit val ec = global

  override def afterAll: Unit = {
    TestKit.shutdownActorSystem(system)
  }

  "An WordCounter actor" must {

    "send back messages unchanged" in {
      val wordCounterActorRef = system.actorOf(Props(new WordCounterActor("/home/developer/Desktop/WordCounter/src/test/resources/dictionaryFile")), "helloactor")

      val future = wordCounterActorRef ? StartMatchingProcessForLine("abd")
      val res = Await.result(future, Duration.Inf)
      assert(res == 1)
    }

  }
}