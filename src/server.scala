package scala.meta.telemetry.server

import cats.effect._
import cats.effect.kernel.Resource
import com.comcast.ip4s.{port, ipv4}
import org.http4s._
import org.http4s.ember.server._
import fs2.io.net.tls.TLSContext
import scala.meta.internal.{telemetry => api}

import com.sksamuel.elastic4s.ElasticClient
import com.sksamuel.elastic4s.ElasticDsl._
import com.sksamuel.elastic4s.cats.effect.instances._

import java.time.OffsetDateTime
import com.github.plokhotnyuk.jsoniter_scala.core.*
import sttp.tapir.server.http4s.Http4sServerInterpreter
import model.given_Conversion_Unit_Void

object Endpoints:
  import sttp.tapir.*
  import sttp.model.Method

  private inline def endpointFor[In, Out](
      prototype: api.ServiceEndpoint[In, Out]
  ) = {
    def gson = api.GsonCodecs.gson
    endpoint
      .method(Method.unsafeApply(prototype.getMethod()))
      .in("v1" / "telemetry" / "sendReportEvent" / stringJsonBody)
      .mapIn[In] { plainJson =>
        val res = scala.util.Try{
          println(plainJson)
          gson.fromJson(plainJson, prototype.getInputType())
        }
        println(res)
        res.get
      } { gson.toJson(_) }
      .out(stringJsonBody)
      .mapOut { plainJson =>
        gson.fromJson(plainJson, prototype.getOutputType())
      } { gson.toJson(_) }
  }

  val sendReportEvent: PublicEndpoint[api.ReportEvent, Unit, Void, Any] = endpointFor(
    api.TelemetryService.SendReportEventEndpoint
  )
  println(sendReportEvent.show)

object Main extends IOApp:
  lazy val keystorePassword = sys.env.getOrElse("KEYSTORE_PASSWORD", "changeit").toCharArray()
  def tlsContext = TLSContext.Builder
    .forAsync[IO]
    .fromKeyStoreFile(
      file = java.nio.file.Paths.get(sys.env("KEYSTORE_PATH")),
      keyPassword = keystorePassword,
      storePassword = keystorePassword
    )

  override def run(args: List[String]): IO[ExitCode] = Routes.telemetryRoutes
    .flatMap { routes =>
      // .both(tlsContext.toResource)
      // .flatMap { (routes, tlsContext) =>
      EmberServerBuilder
        .default[IO]
        .withPort(port"8081")
        .withHost(ipv4"0.0.0.0")
        .withHttpApp(routes.orNotFound)
        // .withTLS(tlsContext)
        .withErrorHandler { case ex: Throwable =>
          IO.println(s"Failed to handle request: ${ex.getMessage()}") *>
            IO.pure(Response(status = Status.InternalServerError))
        }
        .build
    }
    .use(_ => IO.never)
    .as(ExitCode.Success)
end Main

class TelemetryServerImpl(esClient: ElasticClient) {
  def sendReportEvent(event: api.ReportEvent): IO[Void] = {
    for
      transformedEvent <- IO.pure {
        model.ReportEventOf(event, receivedAt = OffsetDateTime.now())
      }
      eventId = model.EventId(transformedEvent)
      asyncIndex = esClient
        .execute(
          indexInto(elasticsearch.index.TelemetryReportsIndex(transformedEvent.receivedAt))
            .doc(writeToString(transformedEvent))
            .withId(eventId)
        )
        .timed
        .flatMap((took, response) =>
          IO.println(
            s"Indexed $eventId: status=${response.status}/${response
                .fold(err => s"${err.error.`type`}-${err.error.reason}", _.result)}, took=${took.toMillis}ms"
          )
        )
        .handleErrorWith { err => IO.println(s"Failed to index event $eventId: $err") }
        .uncancelable
        .background
      _ <- asyncIndex.use { fiber =>
        IO.println(s"Acknowledged, eventId=$eventId, receivedAt=${transformedEvent.receivedAt}")
      }
    yield ()
  }
}

object Routes {
  val telemetryRoutes: Resource[IO, HttpRoutes[IO]] =
    for
      esClient <- Resource.fromAutoCloseable {
        IO.delay(elasticsearch.getClient())
          .flatTap(elasticsearch.checkConnection)
          // .flatTap(elasticsearchAdmin.createIndices)
          .flatTap(_ => IO.println("Elasticsearch client ok"))
      }
      logic = TelemetryServerImpl(esClient)
      routes = Http4sServerInterpreter[IO]().toRoutes(
        Endpoints.sendReportEvent.serverLogicSuccess[IO](logic.sendReportEvent)
      )
    yield routes
}
