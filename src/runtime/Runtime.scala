package looneesha

import akka.actor.{ActorRef, Actor, Props, ActorSystem}
import akka.io.{IO, Tcp}
import akka.util.ByteString
import java.net.InetSocketAddress
import scala.xml._

class Worker extends Actor {
  def receive = {
    case cf: AtomCF => cf.run foreach (sender ! _)
  }
}

class Runtime(graph: Graph, needed: List[AtomDF]) extends Actor {
  var cfs: List[(AtomCF, Boolean)] = Nil
  var kdfs: List[AtomDF] = Nil
  val worker = context.actorOf(Props[Worker], name = "worker")
  var sdfs: List[AtomDF] = Nil
  var client: ActorRef = null

  def tryCompute = {
    var acfs = cfs.toArray
    cfs filter (_._2 == false) foreach (cf => {
      val dfs = cf._1.in
      if(dfs == Nil) {
        val i = acfs.indexOf(cf)
        acfs.update(i, cf._1 -> true)
        val rcf = cf._1
        worker ! rcf
      } else {
        var fdfs: List[AtomDF] = Nil
        dfs forall (df => kdfs.find(_ == df) match {
          case None => false
          case Some(x) => fdfs ::= x; true
        }) match {
          case false => ()
          case true => {
            val i = acfs.indexOf(cf)
            acfs.update(i, cf._1 -> true)
            val rcf = cf._1.set(fdfs)
            worker ! rcf
          }
        }
    }})
    cfs = acfs.toList
  }

  def receive = {
    case df: AtomDF => {
      println("Look! " + df + " = " + df.value)
      if(!sdfs.contains(df)) {
        kdfs ::= df
        if(client != null) {
          sdfs ::= df
          client ! df
          tryCompute
        }
      } else {
        sdfs = sdfs filterNot (df == _)
      }
    }

    case Runtime.Start => {
      tryCompute
    }

    case x: ActorRef => client = x; tryCompute
  }

  override def preStart = graph.subgraph(needed) foreach (cf => cfs ::= cf -> false)


}

object Runtime {
  case object Start
  case object Init
}

object Server {
  case class Connect(host: String, port: Int)
  case object Start
  case object RequestRuntime
}

class Server(host: String, port: Int, g: GraphBuilder, m: List[AtomDF]) extends Actor {
  import Tcp._
  import context.system
  var runtime: ActorRef = context.actorOf(Props(classOf[Runtime], g.get, m))
  var client: ActorRef = null

  IO(Tcp) ! Bind(self, new InetSocketAddress(host, port))

  def receive = {
    case b @ Bound(localAddress) =>
    // do some logging or setup ...

    case CommandFailed(_: Bind) => context stop self

    case c @ Connected(remote, local) => {
      val handler = context.actorOf(Props(classOf[SimplisticHandler], self, runtime))
      val connection = sender()
      connection ! Register(handler)
    }
    case Server.Connect(h, p) =>
      println("Try to connect to " + h + ":" + p)
      client = context.actorOf(Props(classOf[Client], new InetSocketAddress(h, p)))
      Thread.sleep(1000)
      runtime ! client
    case Server.RequestRuntime => sender ! runtime
    case Server.Start => runtime ! Runtime.Start
  }
}

class SimplisticHandler(server: ActorRef, runtime: ActorRef) extends Actor {
  import Tcp._

  def receive = {
    case Received(data) => {
      val str = data.utf8String
      val realstr = "<hm>" + str + "</hm>"
      val xml = XML.loadString(realstr)
      (xml \ "message").foreach(a => { println(a.toString); (a \ "type")(0).text match {
        case "connect" => {
          val host = (a \ "host")(0).text
          val port = (a \ "port")(0).text.toInt
          server ! Server.Connect(host, port)
        }

        case "start" => server ! Server.Start; context stop self
        case "df" => {
          val dfname: String = (a \ "name")(0).text
          val dfindex: Int = (a \ "index")(0).text.toInt
          val dfval: Double = (a \ "val")(0).text.toDouble

          val df = AtomDF(dfname, dfindex, dfval, true)

          runtime ! df
        }

        case "init" => server ! Server.RequestRuntime
        case x: String => {
          println("WUT? " + x)
        }
      }})
    }
    case PeerClosed => context stop self
  }
}

class Client(remote: InetSocketAddress) extends Actor {

  import Tcp._
  import context.system

  IO(Tcp) ! Connect(remote)
  def receive = {
    case CommandFailed(_: Connect) =>
      println("connect failed")
      context stop self

    case c @ Connected(remote, local) =>
      val connection = sender()
      connection ! Register(self)
      println("Ok, I'm here")
      context become {
        case CommandFailed(w: Write) =>
          // O/S buffer was full
          println("write failed")

        case df: AtomDF => connection ! Write(ByteString("<message><type>df</type><name>" + df.name + "</name><index>" + df.index + "</index><val>" + df.value + "</val></message>"))

        case Runtime.Init =>
          val string = "<message><type>init</type></message>"
          val lowmessage = ByteString(string)
          connection ! Write(lowmessage)

        case "close" =>
          connection ! Close

        case x: String => connection ! Write(ByteString(x))

        case _: ConnectionClosed =>
          println("connection closed")
          context stop self
      }
  }
}




