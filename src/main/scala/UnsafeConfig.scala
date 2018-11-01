import com.typesafe.config.Config
import scala.collection.JavaConverters._

object UnsafeConfig {

  def getConfigSafely[T](config: Config, field: String, dataType: Class[T]): Option[T] = {
    if (config.hasPath(field)) {
      val ret = dataType match {
        case dt if dt == classOf[Config] => config.getConfig(field)
        case dt if dt == classOf[String] => config.getString(field)
        case dt if dt == classOf[Int] => config.getInt(field)
        //TODO: Handle more list types.
        case dt if dt == classOf[List[String]] => config.getStringList("inputs").asScala.toList
        //TODO: Handle Map data types in a better way
        case dt if dt == classOf[Map[Any, Any]] =>
          config.getConfig(field).entrySet()
            .asScala
            .map(e => (e.getKey, e.getValue.render().replace("\"", ""))).toMap
      }
      Some(ret.asInstanceOf[T])
    }
    else {
      None
    }
  }

  def getConfigOrFail[T](config: Config, field: String, dataType: Class[T]): T = {
    if (config.hasPath(field)) {
      val ret = dataType match {
        case dt if dt == classOf[Config] => config.getConfig(field)
        case dt if dt == classOf[String] => config.getString(field)
        case dt if dt == classOf[Int] => config.getInt(field)
        //TODO: Handle more list types.
        case dt if dt == classOf[List[String]] => config.getStringList("inputs").asScala.toList
        case dt if dt == classOf[Map[Any, Any]] =>
          config.getConfig(field).entrySet()
            .asScala
            .map(e => (e.getKey, e.getValue.render().replace("\"", ""))).toMap
      }
      ret.asInstanceOf[T]
    }
    else {
      throw new RuntimeException(s"$field is not a valid entry.")
    }
  }

}
