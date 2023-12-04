package scala.meta.telemetry.server

import com.sksamuel.elastic4s.ElasticClient
import com.sksamuel.elastic4s.ElasticDsl._
import com.sksamuel.elastic4s.cats.effect.instances._
import com.sksamuel.elastic4s.fields._

import cats.effect.IO
import java.time.OffsetDateTime
import com.sksamuel.elastic4s.requests.indexes.TemplateAlias
import com.sksamuel.elastic4s.ElasticRequest
import com.sksamuel.elastic4s.HttpEntity

import scala.concurrent.Promise
import elasticsearch.*
import com.sksamuel.elastic4s.requests.mappings.MappingDefinition
import com.sksamuel.elastic4s.requests.indexes.CreateIndexTemplateRequest

object elasticsearchAdmin {

  def createIndices(esClient: ElasticClient): IO[Unit] = for
    _ <- IO.println("create policy")
    createPolicy <- IO.fromFuture {
      IO {
        val done = Promise[Int]()
        esClient.client.send(
          config.CreateTelemetryReportsLifecyclePolicy,
          result => {
            done.complete(result.toTry.map(_.statusCode))
            assert(result.map(_.statusCode).contains(200), s"failed to create policy: $result")
          }
        )
        done.future
      }
    }
    _ <- IO.println("policy:" -> createPolicy)
    erroReportsTemplate <- esClient.execute(config.CreateTelemetryErrorReportsTemplate)
    _ <- IO.println(erroReportsTemplate)
    crashReportsTemplate <- esClient.execute(config.CreateTelemetryCrashReportsTemplate)
    _ <- IO.println(crashReportsTemplate)
  yield ()

  private[elasticsearchAdmin] object config {
    // Use overloaded versions to ensure compile time checks that given field actually exists in the model
    import compiletimeMappings.{in, inNestedField, keywordField, dateField, textField, booleanField, nestedField}
    extension (field: ObjectField)
      def fields(props: ElasticField*): ObjectField = field.copy(properties = props)
      def dynamic(value: Boolean): ObjectField = field.copy(dynamic = Some(value.toString()))

    val TelemetryReportsLifecyclePolicyName = "telemetry-reports-lifecycle-policy"
    val CreateTelemetryReportsLifecyclePolicy = ElasticRequest(
      "PUT",
      s"_ilm/policy/$TelemetryReportsLifecyclePolicyName",
      HttpEntity("""{
      |  "policy": {
      |    "phases": {
      |      "delete": {
      |        "min_age": "180d",
      |        "actions": {
      |          "delete": {}
      |        }
      |      }
      |    }
      |  }
      |}""".stripMargin)
    )

    import compiletimeMappings.Ctx
    type TrackedMapping[T, Field] = Ctx[T] ?=> Field => Field
    type ComponentMapping[Field] = Field => Field
    type ObjectFieldMapping = ComponentMapping[ObjectField]

    inline def presentationCompilerConfigsMappings(using
        ctx: Ctx[model.PresentationCompilerConfig]
    ): ObjectFieldMapping =
      _.fields(nestedField("symbolPrefixes").dynamic(false))

    inline def environmentMapping: TrackedMapping[model.Environment, ObjectField] = _.fields(
      inNestedField[model.JavaInfo](objectField("java")):
        _.fields(
          keywordField("version"),
          keywordField("distribution")
        )
      ,
      inNestedField[model.SystemInfo](objectField("system")):
        _.fields(
          keywordField("architecture"),
          keywordField("name"),
          keywordField("version")
        )
    )

    inline def reporterMapping: TrackedMapping[model.ReporterWrapper, ObjectField] =
      _.fields(
        inNestedField[model.Reporter.MetalsLSP](objectField("metalsLSP")):
          _.fields(
            keywordField("metalsVersion"),
            inNestedField[model.BuildServerConnection](nestedField("buildServerConnections")):
              _.fields(
                keywordField("name"),
                keywordField("version")
              )
            ,
            inNestedField[model.MetalsUserConfiguration](objectField("userConfig")):
              _.fields(nestedField("symbolPrefixes").dynamic(false))
            ,
            inNestedField[model.MetalsServerConfiguration](objectField("serverConfig")):
              _.fields(
                inNestedField[model.PresentationCompilerConfig](objectField("compilers")):
                  presentationCompilerConfigsMappings(_)
              )
          )
        ,
        inNestedField[model.Reporter.ScalaPresentationCompiler](objectField("scalaPresentationCompiler")):
          _.fields(
            keywordField("scalaVersion"),
            keywordField("options"),
            inNestedField[model.PresentationCompilerConfig](objectField("config")):
              presentationCompilerConfigsMappings(_)
          )
        ,
        inNestedField[model.Reporter.Unknown](objectField("unknown")):
          _.fields(
            keywordField("name"),
            keywordField("version"),
            nestedField("properties").dynamic(false)
          )
      )

    inline def exceptionSummaryMapping: TrackedMapping[model.ExceptionSummary, ObjectField] =
      _.fields(
        keywordField("exceptions"),
        textField("stacktrace")
      )

    val CreateTelemetryErrorReportsTemplate: CreateIndexTemplateRequest =
      createIndexTemplate("telemetry-error-reports-template", "telemetry-error-reports-*_*")
        .aliases(TemplateAlias(index.TelemetryReportsAlias), TemplateAlias(index.TelemetryErrorReportsAlias))
        .create(false) // Allow to override
        .version(1)
        .settings(
          Map(
            "index.lifecycle.name" -> TelemetryReportsLifecyclePolicyName
          )
        )
        .mappings(
          in[model.ErrorReport](
            properties(
              DateField("@timestamp"),
              dateField("receivedAt").copy(copyTo = Seq("@timestamp")),
              keywordField("id"),
              keywordField("name"),
              textField("text"),
              inNestedField[model.ExceptionSummary](objectField("error"))(exceptionSummaryMapping),
              inNestedField[model.Environment](objectField("env"))(environmentMapping),
              keywordField("reporterName"),
              inNestedField[model.ReporterWrapper](objectField("reporter"))(reporterMapping)
            )
          )
        )
    end CreateTelemetryErrorReportsTemplate

    val CreateTelemetryCrashReportsTemplate: CreateIndexTemplateRequest =
      createIndexTemplate("telemetry-crash-reports-template", "telemetry-crash-reports-*_*")
        .aliases(TemplateAlias(index.TelemetryReportsAlias), TemplateAlias(index.TelemetryCrashReportsAlias))
        .create(false) // Allow to override
        .version(1)
        .settings(
          Map(
            "index.lifecycle.name" -> TelemetryReportsLifecyclePolicyName
          )
        )
        .mappings(
          in[model.CrashReport](
            properties(
              DateField("@timestamp"),
              dateField("receivedAt").copy(copyTo = Seq("@timestamp")),
              inNestedField[model.Component](objectField("component")):
                _.fields(
                  keywordField("name"),
                  keywordField("version")
                )
              ,
              inNestedField[model.ExceptionSummary](objectField("error"))(exceptionSummaryMapping),
              inNestedField[model.Environment](objectField("env"))(environmentMapping),
              inNestedField[model.ReporterWrapper](objectField("reporter"))(reporterMapping)
            )
          )
        )
    end CreateTelemetryCrashReportsTemplate
  }

  private object compiletimeMappings {
    import scala.deriving.Mirror
    import scala.compiletime.{constValue, error}

    type Equals[x, expected <: String] <: Boolean = x match {
      case expected => true
      case _        => false
    }
    type HasField[Name <: StringLiteral, Tup <: Tuple] <: Boolean =
      Tuple.Filter[Tup, [field] =>> Equals[field, Name]] match {
        case EmptyTuple => false
        case _          => true
      }
    type StringLiteral = String & Singleton
    inline def fieldOf[T: Ctx, Name <: StringLiteral](inline name: Name)(using mirror: Mirror.Of[T]): String =
      inline compiletime.constValue[HasField[Name, mirror.MirroredElemLabels]] match {
        case true => constValue[Name]
        case false =>
          error(
            "Not found field with name `" + name + "` in type:" + constValue[
              mirror.MirroredLabel
            ]
          )
      }
    trait Ctx[T] {
      val mirror: Mirror.Of[T]
      type MirroredElemLabels = Mirror.Of[T]#MirroredElemLabels
    }
    object Ctx {
      inline def of[T] = new Ctx[T] {
        val mirror = scala.compiletime.summonInline[Mirror.Of[T]]
      }
    }

    transparent inline def in[T: Mirror.Of](
        inline body: Ctx[T] ?=> MappingDefinition
    ): Any = {
      body(using Ctx.of[T])
    }

    trait WithContext[T, Out] {
      inline def apply()(using inline ctx: Ctx[T]): Out
    }

    transparent inline def inNestedField[Inner: Mirror.Of](nestedField: NestedField)(
        inline body: Ctx[Inner] ?=> NestedField => NestedField
    ): NestedField = body(using Ctx.of[Inner])(nestedField)
    transparent inline def inNestedField[Inner: Mirror.Of](nestedField: ObjectField)(
        inline body: Ctx[Inner] ?=> ObjectField => ObjectField
    ): ObjectField = body(using Ctx.of[Inner])(nestedField)

    inline def keywordField[T: Ctx: Mirror.Of, Name <: StringLiteral](inline name: Name) = KeywordField(fieldOf(name))
    inline def dateField[T: Ctx: Mirror.Of, Name <: StringLiteral](inline name: Name) = DateField(fieldOf(name))
    inline def textField[T: Ctx: Mirror.Of, Name <: StringLiteral](inline name: Name) = TextField(fieldOf(name))
    inline def booleanField[T: Ctx: Mirror.Of, Name <: StringLiteral](inline name: Name) = BooleanField(fieldOf(name))
    inline def nestedField[T: Ctx: Mirror.Of, Name <: StringLiteral](inline name: Name) = NestedField(fieldOf(name))
    inline def objectField[T: Ctx: Mirror.Of, Name <: StringLiteral](inline name: Name) = ObjectField(fieldOf(name))
  }
}
