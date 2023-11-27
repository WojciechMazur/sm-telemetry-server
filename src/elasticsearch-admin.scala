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

object elasticsearchAdmin {

  def createIndices(esClient: ElasticClient): IO[Unit] = for
    _ <- IO.println("create policy")
    createPolicy <- IO.fromFuture {
      IO {
        val done = Promise[Int]()
        esClient.client.send(
          config.CreateTelemetryRaportsLifecyclePolicy,
          result => {
            done.complete(result.toTry.map(_.statusCode))
            assert(result.right.map(_.statusCode).contains(200), s"failed to create policy: $result")
          }
        )
        done.future
      }
    }
    _ <- IO.println("policy:" -> createPolicy)
    result <- esClient.execute(config.CreateTelemetryReportsTemplate)
    _ <- IO.println(result)
  yield ()

  private[elasticsearchAdmin] object config {
    // Use overloaded versions to ensure compile time checks that given field actually exists in the model
    import compiletimeMappings.{in, inNestedField, keywordField, dateField, textField, booleanField, nestedField}
    extension (field: ObjectField)
      def fields(props: ElasticField*): ObjectField = field.copy(properties = props)
      def dynamic(value: Boolean): ObjectField = field.copy(dynamic = Some(value.toString()))

    val TelemetryRaportsLifecyclePolicyName = "telemetry-raports-lifecycle-policy"
    val CreateTelemetryRaportsLifecyclePolicy = ElasticRequest(
      "PUT",
      s"_ilm/policy/$TelemetryRaportsLifecyclePolicyName",
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

    inline def presentationCompilerConfigsMappings(obj: ObjectField)(using
        compiletimeMappings.Ctx[model.PresentationCompilerConfig]
    ): ObjectField =
      obj.fields(nestedField("symbolPrefixes").dynamic(false))

    val CreateTelemetryReportsTemplate =
      createIndexTemplate("telemetry-raports-template", "telemetry-raports-*_*")
        .aliases(TemplateAlias(index.TelemetryReportersAlias))
        .create(false) // Allow to override
        .version(1)
        .settings(
          Map(
            "index.lifecycle.name" -> TelemetryRaportsLifecyclePolicyName
          )
        )
        .mappings(
          in[model.ReportEvent](
            properties(
              DateField("@timestamp"),
              dateField("receivedAt").copy(copyTo = Seq("@timestamp")),
              keywordField("id"),
              keywordField("name"),
              textField("text"),
              textField("shortSummary"),
              inNestedField[model.ReportedError](objectField("error")):
                _.fields(
                  keywordField("exceptions"),
                  textField("stacktrace")
                )
              ,
              inNestedField[model.Environment](objectField("env")):
                _.fields(
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
              ,
              keywordField("reporterName"),
              inNestedField[model.ReporterWrapper](objectField("reporter")):
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
            )
          )
        )
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
    inline def fieldOf[T, Name <: StringLiteral](inline name: Name)(using
        ctx: Ctx[T]
    ): String =
      inline constValue[HasField[Name, ctx.MirroredElemLabels]] match {
        case true => constValue[Name]
        case false =>
          error(
            "Not found field with name `" + name + "` in type:" + constValue[
              ctx.MirroredLabel
            ]
          )
      }
    transparent inline def in[T: Ctx](
        inline body: Ctx[T] ?=> Any
    ): Any = body

    type Ctx[T] = Mirror.Of[T]
    transparent inline def inNestedField[Inner: Ctx](nestedField: NestedField)(
        inline body: Ctx[Inner] ?=> NestedField => NestedField
    ): NestedField = body(nestedField)
    transparent inline def inNestedField[Inner: Ctx](nestedField: ObjectField)(
        inline body: Ctx[Inner] ?=> ObjectField => ObjectField
    ): ObjectField = body(nestedField)

    inline def keywordField[T: Ctx, Name <: StringLiteral](inline name: Name) = KeywordField(fieldOf(name))
    inline def dateField[T: Ctx, Name <: StringLiteral](inline name: Name) = DateField(fieldOf(name))
    inline def textField[T: Ctx, Name <: StringLiteral](inline name: Name) = TextField(fieldOf(name))
    inline def booleanField[T: Ctx, Name <: StringLiteral](inline name: Name) = BooleanField(fieldOf(name))
    inline def nestedField[T: Ctx, Name <: StringLiteral](inline name: Name) = NestedField(fieldOf(name))
    inline def objectField[T: Ctx, Name <: StringLiteral](inline name: Name) = ObjectField(fieldOf(name))
  }
}
