
// base class for json-serializable custom types
class SqlJsonType [T:Format] extends SqlCustomType [T,String] (
  c => Json.parse(c).as[T],           // String => T
  c => Json.stringify(Json toJson c)  // T => STring
)   

implicit object dataT extends SqlJsonType [Data]

