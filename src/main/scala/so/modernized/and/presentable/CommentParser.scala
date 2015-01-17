package so.modernized.and.presentable

import scala.io.Source
import scala.util.parsing.combinator.RegexParsers

object CommentParser extends RegexParsers {
  override val skipWhitespace = false

  val singlelineCommentStart:Parser[String] = "//"
  val multilineCommentStart:Parser[String] = """/\*""".r
  val scalaDocCommentStart:Parser[String] = """/\*\*""".r
  val multilineStart = """\s+\*\s?(?!/)""".r.?
  val multilineEnd:Parser[String] = """\s*\*/""".r
  val commentText:Parser[String] = "(.*?)(?=\\*/|\n|\\z)".r("content")

  val spaces:Parser[String] = "( |\t)*".r
  val eol:Parser[String] = "\n"
  val eof:Parser[String] = "\\z".r

  val multilineCommentText = ((multilineCommentStart ~ eol.?) ~> (multilineStart ~> commentText <~ eol).* ~ commentText <~ (multilineEnd ~ spaces ~ eol.?)) ^^ { case ~(cs, c) =>
    (cs.mkString("\n") + (if (c.nonEmpty) "\n" + c else "")).markdown
  }
  val scalaDocCommentText = ((scalaDocCommentStart ~ eol.?) ~> (multilineStart ~> commentText <~ eol).* ~ commentText <~ (multilineEnd ~ spaces ~ eol.?)) ^^ { case ~(cs, c) =>
    (cs.mkString("\n") + (if (c.nonEmpty) "\n" + c else "")).scalaDoc
  }
  val singleMultilineCommentText = multilineCommentStart ~> commentText <~ multilineEnd
  val singleLineCommentText = singlelineCommentStart ~> commentText <~ eol.?

  val singleLineComments = singleLineCommentText.+ ^^ {_.mkString("\n").markdown}

  val comment = spaces ~> (singleMultilineCommentText | scalaDocCommentText | multilineCommentText | singleLineComments)

  val singleLineString:Parser[String] = "\".*\"".r
  val multilineString:Parser[String] = "(?s)\"\"\".*?\"\"\"".r
  val string = multilineString | singleLineString

  val codeText:Parser[String] = "(.+?)(?=//|/\\*|\n|\\z|\")".r("contents")  //<~ guard(comment | eof)

  val codeLine = (string.? ~ codeText ~ string.?).+ <~ (eol | eof) ^^ (_.map{case ~(~(a, b), c) => a.getOrElse("") + b + c.getOrElse("")})
  val codeBlock = codeLine.+ ^^ {_.mkString("\n")}


  val doc = (comment.* ~ codeLine).*

  val s = """/**
            |  * Scaladoc item representing a param as described in the ScalaDoc
            |  * @param param the parameter name.
            |  * @param doc the documentation associated to this param.
            |  */""".stripMargin

  val s2 = """/* a single line comment */"""

  val s3 =
    """/** stuff
      | * multiple lines of stuff
      | * @param this, that
      | * [java.lang.String]
      | */
      | case class Foo(bar:String)""".stripMargin

  val s4 =
    """// comment on a single line"""

  val s5 =
    """//Scalocco
      |//---------------""".stripMargin

  val s6 = "//"

  val s7 = """/****************************************************************************************
             | ______     ______     ______     __         ______     ______     ______     ______
             |/\  ___\   /\  ___\   /\  __ \   /\ \       /\  __ \   /\  ___\   /\  ___\   /\  __ \
             |\ \___  \  \ \ \____  \ \  __ \  \ \ \____  \ \ \/\ \  \ \ \____  \ \ \____  \ \ \/\ \
             | \/\_____\  \ \_____\  \ \_\ \_\  \ \_____\  \ \_____\  \ \_____\  \ \_____\  \ \_____\
             |  \/_____/   \/_____/   \/_/\/_/   \/_____/   \/_____/   \/_____/   \/_____/   \/_____/
             |
             |                         Scala implementation of Docco
             |                         -----------------------------
             |         Produces HTML pages that displays your comments alongside your code.
             |****************************************************************************************/""".stripMargin

  def main(args:Array[String]) {

    /*
    Seq(s,s2, s3, s4, s5, s6, s7) foreach { i =>
      println(parse(comment, i))  //<~ eol.?) ~ multilineStart ~ text ~ eol ~ multilineStart ~ text ~ eol ~ text, i))
    }
      */
    //println(parseAll(comment ~ codeBlock, s3))
    //println(parseAll(multilineCommentStart ~ eol.? ~> (multilineStart ~> commentText <~ eol).* ~ commentText.? <~ multilineEnd, s7))
    val sourceText = Source.fromFile(args(0)).mkString
    val res = parseAll((comment.* ~ (codeBlock <~ eol.*)).*, sourceText)
    println(res)
  }
}
