// #Presentable
// is a literate programming framework for scala, inspired by
// (Docco)[http://jashkenas.github.io/docco/],
// (Marginalia)[https://github.com/gdeer81/marginalia] and friends. It takes
// scala source files and generates snazzy webpages, respecting scaladocs and
// using (markdown)[http://daringfireball.net/projects/markdown/] to decorate
// non-scaladoc comments

package so.modernized.and.presentable

// ##Imports
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


  val codeText:Parser[String] = "(?!//)(.+?)(?=//|/\\*|\n|\\z|\")".r

  val codeLine = (string.? ~ codeText ~ string.?).+ ~ singleLineCommentText.?  ^^ {case ~(lineBits, cmntStringOpt) =>
    val composedLine = lineBits.flatMap{case ~(~(s1Opt, cdeBit), s2Opt) => Seq(s1Opt, Option(cdeBit), s2Opt).flatten}.mkString("")
    cmntStringOpt match {
      case Some(cmntString) => Left(Section(cmntStringOpt, Option(composedLine)))
      case None => Right(composedLine)
    }
  }

  val codeBlock = (codeLine <~ eol.?).+ ^^ { lines =>
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

  val doc = ((comment.* <~ eol.*) ~ (codeBlock <~ eol.*)).* <~ eof ^^ (_.flatMap {
    case ~(commentStrings, codeBlocks) =>
      def collapseToCodeSections(coll:List[Either[Section, String]]) = coll map {
        case Right(cs) => Section(None, Some(cs))
        case Left(sec) => sec
      }
      val (commentsWithoutCode, attachableCommentList) = commentStrings.splitAt(commentStrings.size - 1)
      commentsWithoutCode.map(s => Section(Some(s), None)).++(codeBlocks match {
        case Right(codeString) :: rest => Section(attachableCommentList.headOption, Some(codeString)) :: collapseToCodeSections(rest)
        case Left(sec) :: rest => Section(attachableCommentList.headOption, None) :: sec :: collapseToCodeSections(rest)
        case Nil => Nil
      })
  })

  def processDocText(docString:String) = parseAll(doc, docString)
}
