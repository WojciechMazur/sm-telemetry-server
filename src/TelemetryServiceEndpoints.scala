package scala.meta.telemetry.server

import scala.language.implicitConversions
import scala.meta.internal.{telemetry => api}
import api.GsonCodecs.gson
import sttp.tapir.EndpointInput.FixedPath
import com.google.gson.JsonSyntaxException
import sttp.tapir.*
import sttp.model.Method
import sttp.tapir.server.ServerEndpoint

object TelemetryServiceEndpoints:
  val sendErrorReport: PublicEndpoint[api.ErrorReport, Unit, Void, Any] = endpointFor(
    api.TelemetryService.SendErrorReportEndpoint
  )

  val sendCrashReport: PublicEndpoint[api.CrashReport, Unit, Void, Any] = endpointFor(
    api.TelemetryService.SendCrashReportEndpoint
  )

  def routes(impl: TelemetryServiceImpl): List[ServerEndpoint[Any, cats.effect.IO]] = List(
    sendErrorReport.serverLogicSuccess(impl.onErrorReport),
    sendCrashReport.serverLogicSuccess(impl.onCrashReport)
  )

  private def decodeGson[T](json: String, tpe: Class[T]) = {
    val decodingResult =
      try Right(gson.fromJson(json, tpe))
      catch { case err: JsonSyntaxException => Left(err.getMessage()) }
    DecodeResult.fromEitherString(json, decodingResult)
  }

  private inline def endpointFor[In, Out](
      prototype: api.ServiceEndpoint[In, Out]
  ) = {
    endpoint
      .method(Method.unsafeApply(prototype.getMethod()))
      .in(
        prototype
          .getUri()
          .split("/")
          .map(_.trim())
          .filter(_.nonEmpty)
          .map(stringToPath)
          .reduce[EndpointInput[Unit]](_ / _) / stringJsonBody
      )
      .mapInDecode[In](decodeGson(_, prototype.getInputType())) { gson.toJson(_) }
      .out(stringJsonBody)
      .mapOutDecode(decodeGson(_, prototype.getOutputType())) { gson.toJson(_) }
  }
