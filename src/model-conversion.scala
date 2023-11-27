package scala.meta.telemetry.server.model

import scala.meta.internal.{telemetry => api}
import scala.meta.telemetry.server.model

import io.scalaland.chimney.dsl._
import io.scalaland.chimney.Transformer
import java.time.OffsetDateTime

def ReportEventOf(event: api.ReportEvent, receivedAt: OffsetDateTime) = event
  .into[model.ReportEvent]
  .withFieldConst(_.receivedAt, receivedAt)
  .withFieldRenamed(_.reporterContext, _.reporter)
  .transform

given Transformer[api.ReporterContext, model.ReporterWrapper] = reporter =>
  model.ReporterWrapper(
    metalsLSP = reporter.project.metalsLsp
      .map(
        _.into[model.Reporter.MetalsLSP]
          .withFieldComputed(_.userConfig.testUserInterface, _.userConfig.testUserInterface.value)
          .transform
      ),
    scalaPresentationCompiler = reporter.project.scalaPresentationCompiler
      .map(_.transformInto[model.Reporter.ScalaPresentationCompiler]),
    unknown = reporter.project.unknown
      .map(_.transformInto[model.Reporter.Unknown])
  )
