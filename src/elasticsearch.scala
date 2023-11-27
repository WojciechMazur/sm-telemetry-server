package scala.meta.telemetry.server

import com.sksamuel.elastic4s.http.JavaClient
import com.sksamuel.elastic4s.ElasticClient
import com.sksamuel.elastic4s.ElasticDsl._
import com.sksamuel.elastic4s.cats.effect.instances._
import com.sksamuel.elastic4s.requests.cat.CatHealth
import com.sksamuel.elastic4s.Index

import org.elasticsearch.client.RestClientBuilder.HttpClientConfigCallback
import org.elasticsearch.client.RestClient
import org.apache.http.impl.nio.client.*
import org.apache.http.impl.client.BasicCredentialsProvider
import org.apache.http.auth.*
import org.apache.http.HttpHost

import cats.effect.IO
import java.time.OffsetDateTime

object elasticsearch {
  object index {
    def TelemetryReportsIndex(receivedAt: OffsetDateTime) = {
      val year = receivedAt.getYear()
      val month = receivedAt.getMonthValue()
      Index(s"telemetry-raports-${year}_${month}")
    }

    val TelemetryReportersAlias = "telemetry-raports"
  }

  def getClient(): ElasticClient = {
    val Host = sys.env.getOrElse("ELASTICSEARCH_HOST", "localhost")
    val HostPathPrefix = sys.env.get("ELASTICSEARCH_PATH_PREFIX")
    val HostSchema = sys.env.getOrElse("ELASTICSEARCH_SCHEMA", "https")
    val Port = sys.env.get("ELASTICSEARCH_PORT").flatMap(_.toIntOption).getOrElse(-1)
    val Credentials = new UsernamePasswordCredentials(
      sys.env.getOrElse("ELASTICSEARCH_USERNAME", "elastic"),
      sys.env.getOrElse("ELASTICSEARCH_PASSWORD", "changeme")
    )
    val clientConfig = new HttpClientConfigCallback {
      override def customizeHttpClient(
          httpClientBuilder: HttpAsyncClientBuilder
      ): HttpAsyncClientBuilder = {
        val creds = new BasicCredentialsProvider()
        creds
          .setCredentials(AuthScope.ANY, Credentials)
        httpClientBuilder
          .setDefaultCredentialsProvider(creds)
      }
    }
    ElasticClient(
      JavaClient.fromRestClient {
        val base = RestClient
          .builder(HttpHost(Host, Port, HostSchema))
          .setHttpClientConfigCallback(clientConfig)
        HostPathPrefix
          .foldLeft(base)(_.setPathPrefix(_))
          .build()
      }
    )
  }

  def checkConnection(esClient: ElasticClient): IO[Unit] =
    for
      response <- esClient.execute(CatHealth())
      _ <- IO.raiseWhen(response.isError)(
        new IllegalStateException(
          s"Failed to connect with ElasticSearch",
          response.error.asException
        )
      )
      _ <- IO.println(response.body)
    yield ()
}
