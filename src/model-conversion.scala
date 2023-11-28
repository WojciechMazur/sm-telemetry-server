package scala.meta.telemetry.server.model

import scala.meta.internal.{telemetry => api}
import scala.meta.telemetry.server.model

import io.scalaland.chimney.dsl.*
import io.scalaland.chimney.javacollections.*
import io.scalaland.chimney.Transformer

import java.time.OffsetDateTime
import scala.jdk.OptionConverters.*
import scala.jdk.CollectionConverters.*

given Conversion[Unit, Void] = _ => null.asInstanceOf[Void]
given Conversion[Void, Unit] = _ => ()

def ReportEventOf(event: api.ReportEvent, receivedAt: OffsetDateTime) = event
  .into[model.ReportEvent]
  .withFieldConst(_.receivedAt, receivedAt)
  .withFieldComputed(_.reporter, _.getReporterContext().transformInto[model.ReporterWrapper])
  .transform

transparent inline given TransformerConfiguration[?] =
  TransformerConfiguration.default.enableBeanGetters.enableMacrosLogging

given Transformer[api.ReportedError, model.ReportedError] = _.into[model.ReportedError].enableBeanGetters.transform

extension (reporter: api.ReporterContext) {
  inline def projection[T <: api.ReporterContext]: Option[T] = reporter match {
    case impl: T => Some(impl)
    case _       => None
  }
}

given Transformer[api.ReporterContextUnion, model.ReporterWrapper] = reporter =>
  model.ReporterWrapper(
    metalsLSP = reporter
      .getMetalsLSP()
      .asScala
      .map(
        _.into[model.Reporter.MetalsLSP]
          .withFieldComputed(
            _.buildServerConnections,
            _.getBuildServerConnections().asScala.map(_.transformInto[model.BuildServerConnection]).toList
          )
          .transform
      ),
    scalaPresentationCompiler = reporter
      .getScalaPresentationCompiler()
      .asScala
      .map(_.transformInto[model.Reporter.ScalaPresentationCompiler]),
    unknown = reporter
      .getUnknown()
      .asScala
      .map(_.transformInto[model.Reporter.Unknown])
  )
