package scala.meta.telemetry.server

import scala.language.implicitConversions
import cats.effect._
import scala.meta.internal.{telemetry => api}
import com.sksamuel.elastic4s.ElasticClient
import com.sksamuel.elastic4s.ElasticDsl._
import com.sksamuel.elastic4s.cats.effect.instances._
import java.time.OffsetDateTime
import com.github.plokhotnyuk.jsoniter_scala.core.*
import model.given_Conversion_Unit_Void
import scala.meta.internal.telemetry.TelemetryService
import scribe.cats.{io => scribeIO}
import scala.meta.telemetry.server.model.Identified
import scala.meta.telemetry.server.model.Indexed
import com.github.plokhotnyuk.jsoniter_scala.core.JsonValueCodec

class TelemetryServiceImpl(esClient: ElasticClient) extends TelemetryService {
  // Unused implementation, intrduced just to ensure full implementation of TelemetryService API
  private def handleUsing[T](handler: IO[T]) = handler.unsafeRunSync()(cats.effect.unsafe.implicits.global)
  override def sendCrashReport(report: api.CrashReport): Unit = handleUsing(onCrashReport(report))
  override def sendErrorReport(report: api.ErrorReport): Unit = handleUsing(onErrorReport(report))

  def onErrorReport(report: api.ErrorReport): IO[Void] =
    for
      event <- IO.pure {
        model.ErrorReportOf(report, receivedAt = OffsetDateTime.now())
      }
      _ <- index(event).uncancelable.background.use { fiber =>
        scribeIO.trace(s"Acknowledged: ${event}")
      }
    yield ()

  def onCrashReport(report: api.CrashReport): IO[Void] =
    for
      event <- IO.pure {
        model.CrashReportOf(report, receivedAt = OffsetDateTime.now())
      }
      _ <- index(event).uncancelable.background.use { fiber =>
        scribeIO.trace(s"Acknowledged: ${event}")
      }
    yield ()

  private def index[T: JsonValueCodec: Identified: Indexed](event: T): IO[Unit] = {
    val id = summon[Identified[T]].id(event)
    val index = summon[Indexed[T]].index(event)
    val json = writeToString(event)
    esClient
      .execute(
        indexInto(index)
          .doc(json)
          .withId(id)
      )
      .timed
      .flatMap { (took, response) =>
        if (response.isSuccess) scribeIO.trace(s"Indexed, index=${index}, id=$id: took=${took.toMillis}ms")
        else
          val errorInfo = response.fold(err => s"${err.error.`type`}-${err.error.reason}", _.result)
          scribeIO.error(s"Indexing failed, index=${index}, id=$id, body=${json}, error=$errorInfo")
      }
      .handleErrorWith { err => scribeIO.error(s"Indexing failed, index=${index}, id=${id}, body=${json}: $err") }
  }

}