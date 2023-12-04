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

def ErrorReportOf(report: api.ErrorReport, receivedAt: OffsetDateTime) = report
  .into[model.ErrorReport]
  .withFieldConst(_.receivedAt, receivedAt)
  .withFieldComputed(_.reporter, _.getReporterContext().transformInto[model.ReporterWrapper])
  .transform

def CrashReportOf(report: api.CrashReport, receivedAt: OffsetDateTime) = report
  .into[model.CrashReport]
  .withFieldConst(_.receivedAt, receivedAt)
  .withFieldComputed(
    _.component,
    v => Component(name = v.getComponentName(), version = v.getComponentVersion().toScala)
  )
  .withFieldComputed(_.reporter, _.getReporterContext().toScala.map(_.transformInto[model.ReporterWrapper]))
  .transform

transparent inline given TransformerConfiguration[?] =
  TransformerConfiguration.default.enableBeanGetters.enableMacrosLogging

given Transformer[api.ExceptionSummary, model.ExceptionSummary] =
  _.into[model.ExceptionSummary].enableBeanGetters.transform

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
      .toScala
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
      .toScala
      .map(_.transformInto[model.Reporter.ScalaPresentationCompiler]),
    unknown = reporter
      .getUnknown()
      .toScala
      .map(_.transformInto[model.Reporter.Unknown])
  )
