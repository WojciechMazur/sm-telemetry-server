package scala.meta.telemetry.server.model

import com.github.plokhotnyuk.jsoniter_scala.core.JsonValueCodec
import com.github.plokhotnyuk.jsoniter_scala.macros.JsonCodecMaker

import java.time.OffsetDateTime
import java.nio.charset.StandardCharsets
import java.util.UUID

opaque type EventId <: String = String
object EventId:
  private def productId(obj: Product): EventId =
    UUID.nameUUIDFromBytes(obj.toString().getBytes(StandardCharsets.UTF_8)).toString()
  private def productId[T <: Product](obj: T)(selector: T => Product): EventId = productId(selector(obj))

  def apply(event: ReportEvent): EventId =
    productId(event)(e => (e.id, e.name, e.text, e.shortSummary, e.error, e.env, e.reporter))

case class ReportEvent(
    receivedAt: OffsetDateTime,
    id: Option[String],
    name: String,
    text: String,
    shortSummary: String,
    error: Option[ReportedError],
    env: Environment,
    reporterName: String,
    reporter: ReporterWrapper
)

object ReportEvent:
  given JsonValueCodec[ReportEvent] = JsonCodecMaker.make

case class Environment(java: JavaInfo, system: SystemInfo)

case class ReportedError(
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
