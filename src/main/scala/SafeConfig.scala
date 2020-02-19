import com.typesafe.config.{Config, ConfigFactory}

import scala.util.Try
import scala.collection.JavaConverters._

object SafeConfig {
  sealed trait FunctionK[F[_], G[_]] {
    def apply[A](fa: F[A]): G[A]
  }

  implicit val functionKOption = new FunctionK[Try, Option] {
    override def apply[T](t: Try[T]): scala.Option[T] = {
      t.toOption
    }
  }
  type ErrorOf[T] = Either[Throwable, T]

  implicit val functionKEither = new FunctionK[Try, ErrorOf] {
    override def apply[T](t: Try[T]): ErrorOf[T] = {
      t.toEither
    }
  }

  sealed trait Get[T] {
    def get(c: Config, path: String): Try[T]
  }

  implicit val stringGet: Get[String] = new Get[String] {
    override def get(c: Config, path: String): Try[String] = Try(c.getString(path))
  }

  implicit val intGet: Get[Int] = new Get[Int] {
    override def get(c: Config, path: String): Try[Int] = Try(c.getInt(path))
  }

  implicit val configGet: Get[Config] = new Get[Config] {
    override def get(c: Config, path: String): Try[Config] = Try(c.getConfig(path))
  }

  implicit val listStringGet: Get[List[String]]  = new Get[List[String]] {
    override def get(c: Config, path: String): Try[List[String]] = Try(c.getStringList(path).asScala.toList)
  }

  def get[F[_], T](config: Config, path: String)(implicit arrow: FunctionK[Try, F], g: Get[T]): F[T] =
    arrow(g.get(config, path))
}


object Main {
  import SafeConfig._


  def main(args: Array[String]): Unit = {
    val cfg = ConfigFactory.load()

    println(get[Option, String](cfg, "notfound"))
    println(get[ErrorOf, String](cfg, "notfound"))
  }
}
