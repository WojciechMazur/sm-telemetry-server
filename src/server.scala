package scala.meta.telemetry.server

import scala.language.implicitConversions

import cats.effect.*
import cats.effect.kernel.Resource
import com.comcast.ip4s.{port, ipv4}
import org.http4s.*
import org.http4s.ember.server.*
import fs2.io.net.tls.TLSContext

import com.sksamuel.elastic4s.ElasticClient

import sttp.tapir.server.http4s.Http4sServerInterpreter
import scribe.cats.{io => scribeIO}
import scribe.format.*
import cats.effect.std.Env
import java.nio.file.Paths

object Main extends IOApp {
  scribe.Logger.root
    .clearHandlers()
    .withHandler(formatter = formatter"${levelColored} $date [$threadName] - $messages")
    .withMinimumLevel(sys.env.get("LOG_LEVEL").flatMap(scribe.Level.get).getOrElse(scribe.Level.Debug))
    .replace()

  override def run(args: List[String]): IO[ExitCode] = {
    val server = for {
      esClient <- Resource.fromAutoCloseable {
        IO(elasticsearch.getClient())
          .flatTap(elasticsearch.checkConnection)
          .handleErrorWith { err =>
            IO.raiseError(InitializationException("Failed to connect with Elasticsearch"))
          }
          .flatTap(_ => scribeIO.info("Elasticsearch connection: ok"))
          .flatTap(client => IO.whenA(args.contains("--setup"))(elasticsearchAdmin.createIndices(client)))
      }
      server <- endpoint(
        isUnsafe = args.contains("--unsafe"),
        routes = Http4sServerInterpreter[IO]()
          .toRoutes(
            TelemetryServiceEndpoints.routes(TelemetryServiceImpl(esClient))
          )
          .orNotFound
      ).toResource
    } yield server

    server
      .use(_ => IO.never)
      .onError(err => scribeIO.error(s"Server critical failure: $err"))
      .as(ExitCode.Success)
  }

  private def endpoint(isUnsafe: Boolean, routes: HttpApp[IO]) = {
    for
      base <- IO.pure(
        EmberServerBuilder
          .default[IO]
          .withPort(port"8081")
          .withHost(ipv4"0.0.0.0")
          .withHttpApp(routes)
          .withErrorHandler { case ex: Throwable =>
            scribeIO.error(s"Failed to handle request: ${ex.getMessage()}") *>
              IO.pure(Response(status = Status.InternalServerError))
          }
      )
      withTLSSetup <-
        if (isUnsafe) scribeIO.warn("Running in unsafe mode, TLS context disabled") *> IO.pure(base.withoutTLS)
        else getTLSContext.map(base.withTLS(_))
    yield withTLSSetup.build
  }

  private def getTLSContext = {
    val KeystorePathEnv = "KEYSTORE_PATH"
    val KeystorePasswordEnv = "KEYSTORE_PASSWORD"
    for {
      keystorePathEnv <- Env[IO].get(KeystorePathEnv)
      keystorePath <- IO
        .fromOption(keystorePathEnv)(
          InitializationException(s"Keystore file not defined, use env variable $KeystorePathEnv")
        )
        .map(Paths.get(_))

      keystorePasswordEnv <- Env[IO].get(KeystorePasswordEnv)
      keystorePassword <- IO
        .fromOption(keystorePasswordEnv)(
          InitializationException(s"Keystore password not defined, use env variable $KeystorePasswordEnv")
        )
        .map(_.toCharArray())
      context <- TLSContext.Builder
        .forAsync[IO]
        .fromKeyStoreFile(
          file = keystorePath,
          keyPassword = keystorePassword,
          storePassword = keystorePassword
        )
    } yield context
  }
}

class InitializationException(message: String) extends RuntimeException(message) with scala.util.control.NoStackTrace
