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

  // we may also need to deal with the case of single \" chars

  val codeText:Parser[String] = "(?!//)(.+?)(?=//|/\\*|\n|\\z|\")".r //<~ guard(comment | eof)

  val codeLine = (string.? ~ codeText ~ string.?).+ ~ singleLineCommentText.?  ^^ {case ~(lineBits, cmntStringOpt) =>
    val composedLine = lineBits.flatMap{case ~(~(s1Opt, cdeBit), s2Opt) => Seq(s1Opt, Option(cdeBit), s2Opt).flatten}.mkString("")
    cmntStringOpt match {
      case Some(cmntString) => Left(Section(Option(composedLine), cmntStringOpt))
      case None => Right(composedLine)
    }
  }

  val codeBlock = codeLine.+ ^^ { lines =>
    val lineBuffer = mutable.ArrayBuffer[String]()
    val collectedBlock = mutable.ArrayBuffer[Either[Section, String]]()
    lines foreach {
      case l @ Left(sec) =>
        collectedBlock += Right(lineBuffer.mkString("\n"))
        collectedBlock += l
        lineBuffer.clear()
      case Right(str) =>
        lineBuffer += str
    }
    if(lineBuffer.nonEmpty) {
      collectedBlock += Right(lineBuffer.mkString("\n"))
    }
    collectedBlock.toList
  }

  /*
  { lines =>
    val ei = lines.head
    ei.r
    var remainder = lines
    while(remainder.nonEmpty) {
      val tup = remainder.span(_.isRight)
      remainder = tup._2
      tup._1.map(_.r)
    }
  }
  */

  val doc = ((comment.* <~ eol.*) ~ (codeBlock <~ eol.*)).* ^^ (_.flatMap {
    case ~(commentStrings, codeBlocks) =>
      def collapseToCodeSections(coll:List[Either[Section, String]]) = coll map {
        case Right(cs) => Section(None, Some(cs))
        case Left(sec) => sec
      }
      val (commentsWithoutCode, attachableCommentList) = commentStrings.splitAt(commentStrings.size - 1)
      commentsWithoutCode.map(s => Section(Some(s), None)).++(codeBlocks match {
        case Right(codeString) :: rest => Section(attachableCommentList.headOption, Some(codeString)) :: collapseToCodeSections(rest)
        case Left(sec) :: rest => Section(attachableCommentList.headOption, None) :: sec :: collapseToCodeSections(rest)
      })
  })

    /*
    map {case ~(commentStrings, codeBlocks) =>
    def collapseToCodeSections(coll:List[Either[Section, String]]) = coll map {
      case Right(cs) => Section(None, Some(cs))
      case Left(sec) => sec
    }
    val (commentsWithoutCode, commentWithCode :: Nil) = commentStrings.splitAt(commentStrings.size - 1)
    commentsWithoutCode.map(s => Section(Some(s), None)).++(codeBlocks match {
      case Right(codeString) :: rest => Section(Some(commentWithCode), Some(codeString)) :: collapseToCodeSections(rest)
      case Left(sec) :: rest => Section(Some(commentWithCode), None) :: sec :: collapseToCodeSections(rest)
    })
  }
  }
  */
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

  val s8 = """override val skipWhitespace = false "//we care about newlines and indents" // we care about newlines and indents"""

  def main(args:Array[String]) {

    /*
    Seq(s,s2, s3, s4, s5, s6, s7) foreach { i =>
      println(parse(comment, i))  //<~ eol.?) ~ multilineStart ~ text ~ eol ~ multilineStart ~ text ~ eol ~ text, i))
    }
      */
    //println(parseAll(codeBlock, s8))
    //println(parseAll(comment ~ codeBlock, s3))
    //println(parseAll(multilineCommentStart ~ eol.? ~> (multilineStart ~> commentText <~ eol).* ~ commentText.? <~ multilineEnd, s7))
    val sourceText = Source.fromFile(args(0)).mkString
    val res = parseAll(doc, sourceText)
    println(res)
  }
}
