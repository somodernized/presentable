// **Presentable** is a literate programming framework for scala, inspired by
// [Docco](todo link) and friends. It takes scala source files and generates
// snazzy webpages, respecting scaladocs and using [markdown](todo link) to
// decorate non-scaladoc comments

package so.modernized.and.presentable

import scala.io.Source
import scala.util.parsing.combinator.RegexParsers
import scala.collection.mutable

// a case class that stores code and its corresponding commentary. Each file
// will become a list of these. Some will lack either commentary or code.
case class Section(commentary:Option[String], code:Option[String])

object Section {
  // groups together sections within the seq that are currently uncommented
  def foldOnCommentary(secs:Seq[Section]) = { // todo this definitely doesn't work
    val res = mutable.ArrayBuffer[Section]()
    var (toCollapse, remainder) = secs.span(_.commentary.isEmpty)
    res += Section(None, toCollapse.flatMap(_.commentary).mkString("\n"))
    res += remainder.head
    while(remainder.nonEmpty) {
      (toCollapse, remainder) = remainder.tail.span(_.commentary.isEmpty)
      res += Section(None, toCollapse.flatMap(_.commentary).mkString("\n"))
      res += remainder.head
    }
    res
  }
}


/* This is where the magic happens, *CommentParser* uses scala's parser
 * combinators to parse scala source files and generate the basic structure of
 * our pages
 */
object CommentParser extends RegexParsers {
  override val skipWhitespace = false // we care about newlines and indents

  // regexes to match parts of comments
  val singlelineCommentStart:Parser[String] = "//"
  val multilineCommentStart:Parser[String] = """/\*""".r
  val scalaDocCommentStart:Parser[String] = """/\*\*""".r // Since we want to treat these differently
  val multilineStart = """\s+\*\s?(?!/)""".r.?
  val multilineEnd:Parser[String] = """\s*\*/""".r
  val commentText:Parser[String] = "(.*?)(?=\\*/|\n|\\z)".r

  // regexes for different kinds of whitespace
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

  // we need to care about strings since comment characters inside them don't count
  val singleLineString:Parser[String] = "\".*\"".r
  val multilineString:Parser[String] = "(?s)\"\"\".*?\"\"\"".r
  val string = multilineString | singleLineString

  val codeText:Parser[String] = "(.+?)(?=//|/\\*|\n|\\z|\")".r //<~ guard(comment | eof)

  val codeLine = (string.? ~ codeText ~ string.? ~ singleLineCommentText.?).+ <~ (eol | eof) ^^ (_.map{
    case ~(~(~(str1, code), str2), lineEndComment) =>
    val line = Seq(str1, code, str2).flatten.mkString("")
    lineEndComment match {
      case Some(cmnt) => Left(Section(lineEndComment, Option(line)))
      case None => Right(line)
    }
  })
  val codeBlock = codeLine.+ ^^ {_.flatten}


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

  val s8 = """override val skipWhitespace = false // we care about newlines and indents"""

  def main(args:Array[String]) {

    /*
    Seq(s,s2, s3, s4, s5, s6, s7) foreach { i =>
      println(parse(comment, i))  //<~ eol.?) ~ multilineStart ~ text ~ eol ~ multilineStart ~ text ~ eol ~ text, i))
    }
      */
    println(parse(codeLine, s8))
    //println(parseAll(comment ~ codeBlock, s3))
    //println(parseAll(multilineCommentStart ~ eol.? ~> (multilineStart ~> commentText <~ eol).* ~ commentText.? <~ multilineEnd, s7))
    //val sourceText = Source.fromFile(args(0)).mkString
    //val res = parseAll(((comment.* <~ eol.*) ~ (codeBlock <~ eol.*)).*, sourceText)
    //println(res)
  }
}
