import scala.collection.immutable.HashSet
import scala.util.matching.Regex
import scala.util.parsing.combinator.RegexParsers

object hello extends App {
      override def main(args: Array[String]): Unit = {
        val text = scala.io.Source.fromFile("testprogram").mkString
        val parser = new Parser
        val result = parser.parseAll(parser.Exp,text)
        println(result.toString)
  }
}
class Parser extends  RegexParsers {

  def Character: Parser[Any] = "[a-z]" | "[A-Z]"| "?" | "_"
  def Digit: Parser[Any] = "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9"
  def Delimiter: Parser[Any] = "(" | ")" | "[" | "]" | "," | ";"
  def Operator: Parser[Any] = "*" | "/" | "+" | "-" | "~" | ">=" | "<=" | "=" | "<" | ">" | "!=" | "&" | "|" | ":="

  def Exp: Parser[Any] = "if" ~ Exp ~ "then" ~ Exp ~ "else" ~ Exp | "let" ~ rep1(Def) ~ "in" ~ Exp | "map" ~ IdList ~ "to" ~ Exp |  Term ~ rep1(Binop ~ Term) | Term

  def Term: Parser[Any] = Unop ~ Term | Factor ~ rep1("(" ~ ExpList ~ ")") | Factor | Empty | Int | Bool

  def Factor: Parser[Any] = "(" ~ Exp ~ ")" | Prim | Id

  def ExpList: Parser[Any] = rep( PropExpList )

  def PropExpList: Parser[Any] = Exp ~ "," ~ PropExpList | Exp

  def IdList: Parser[Any] = rep1( PropIdList )

  def PropIdList: Parser[Any] = Id ~ "," ~ PropIdList | Id

  def Def: Parser[Any] = Id ~ ":=" ~ Exp ~ ";"

  def Bool: Parser[Any] = "True" | "False"

  def Unop: Parser[Any] = Sign | "~"

  def Sign: Parser[Any] = "+" | "-"

  def Empty: Parser[Any] = "null"

  def Binop: Parser[Any] = Sign | "*" | "/" | "=" | "!=" | "<=" | ">="| "<" | ">" | "&" | "|"

  def Prim: Parser[Any] = "number?" | "function?" | "list?" | "null?" | "cons?" | "cons" | "first" | "rest" | "arity"

  def Int: Parser[Any] = rep1(Digit)


  def reservedWords: HashSet[String] = HashSet("number?","function?","list?","empty?","cons?","cons","first","rest","arity","True","False","then","if","else","let","in","map","to","null")

  def idRegex: Regex = """[a-zA-Z][\.a-zA-Z0-9_-]*""".r

  def Id: Parser[String] = Parser(input =>
    idRegex(input).filterWithError(
      !reservedWords.contains(_),
      reservedWord => s"ERROR:This word is reserved: $reservedWord",
      input
    )
  )
}