package scala.meta.telemetry.server.model

import com.github.plokhotnyuk.jsoniter_scala.core.JsonValueCodec
import com.github.plokhotnyuk.jsoniter_scala.macros.JsonCodecMaker

import java.time.OffsetDateTime
import java.nio.charset.StandardCharsets
import java.util.UUID
import com.sksamuel.elastic4s.Index
import scala.meta.telemetry.server.elasticsearch

trait Identified[T]:
  def id(v: T): Id[T]

opaque type Id[T] <: String = String
object Id:
  def of[T](obj: Product): Id[T] =
    UUID.nameUUIDFromBytes(obj.toString().getBytes(StandardCharsets.UTF_8)).toString()
  def of[T <: Product](obj: T)(selector: T => Product): Id[T] = of(selector(obj))

trait Indexed[T]:
  def index(v: T): Index

case class ErrorReport(
    receivedAt: OffsetDateTime,
    id: Option[String],
    name: String,
    text: Option[String],
    error: Option[ExceptionSummary],
    env: Environment,
    reporterName: String,
    reporter: ReporterWrapper
)
object ErrorReport:
  given JsonValueCodec[ErrorReport] = JsonCodecMaker.make
  given Identified[ErrorReport] = Id.of(_)(e => (e.id, e.name, e.text, e.error, e.env, e.reporter))
  given Indexed[ErrorReport] = v => elasticsearch.index.TelemetryErrorReportsIndex(v.receivedAt)

case class CrashReport(
    receivedAt: OffsetDateTime,
    component: Component,
    error: ExceptionSummary,
    env: Environment,
    reporter: Option[ReporterWrapper]
)
object CrashReport:
  given JsonValueCodec[CrashReport] = JsonCodecMaker.make
  given Identified[CrashReport] = Id.of(_)(e => (e.error, e.component, e.env, e.reporter))
  given Indexed[CrashReport] = v => elasticsearch.index.TelemetryCrashReportsIndex(v.receivedAt)

case class Component(name: String, version: Option[String])
case class Environment(java: JavaInfo, system: SystemInfo)

case class ExceptionSummary(
    exceptions: List[String],
    stacktrace: String
)

case class JavaInfo(version: String, distribution: Option[String])
case class SystemInfo(architecture: String, name: String, version: String)
case class ReporterWrapper(
    metalsLSP: Option[Reporter.MetalsLSP],
    scalaPresentationCompiler: Option[Reporter.ScalaPresentationCompiler],
    unknown: Option[Reporter.Unknown]
)
enum Reporter:
  case MetalsLSP(
      metalsVersion: String,
      userConfig: MetalsUserConfiguration,
      serverConfig: MetalsServerConfiguration,
      buildServerConnections: List[BuildServerConnection]
  )
  case ScalaPresentationCompiler(scalaVersion: String, options: List[String], config: PresentationCompilerConfig)
  case Unknown(name: String, version: String, properties: Map[String, String])

case class BuildServerConnection(name: String, version: String, isMain: Boolean)
case class MetalsServerConfiguration(
    executeClientCommand: String,
    snippetAutoIndent: Boolean,
    isHttpEnabled: Boolean,
    isInputBoxEnabled: Boolean,
    askToReconnect: Boolean,
    allowMultilineStringFormatting: Boolean,
    compilers: PresentationCompilerConfig
)
case class MetalsUserConfiguration(
    symbolPrefixes: Map[String, String],
    bloopSbtAlreadyInstalled: Boolean,
    superMethodLensesEnabled: Boolean,
    showImplicitArguments: Boolean,
    showImplicitConversionsAndClasses: Boolean,
    enableStripMarginOnTypeFormatting: Boolean,
    enableIndentOnPaste: Boolean,
    enableSemanticHighlighting: Boolean,
    testUserInterface: String,
    bloopVersion: Option[String],
    bloopJvmProperties: Option[List[String]],
    ammoniteJvmProperties: Option[List[String]],
    showInferredType: Option[String],
    remoteLanguageServer: Option[String],
    excludedPackages: Option[List[String]],
    fallbackScalaVersion: Option[String]
)
case class PresentationCompilerConfig(
    symbolPrefixes: Map[String, String],
    overrideDefFormat: String,
    isDefaultSymbolPrefixes: Boolean,
    isCompletionItemDetailEnabled: Boolean,
    isStripMarginOnTypeFormattingEnabled: Boolean,
    isCompletionItemDocumentationEnabled: Boolean,
    isHoverDocumentationEnabled: Boolean,
    snippetAutoIndent: Boolean,
    isSignatureHelpDocumentationEnabled: Boolean,
    isCompletionSnippetsEnabled: Boolean,
    semanticdbCompilerOptions: List[String],
    completionCommand: Option[String],
    parameterHintsCommand: Option[String]
)
