package so.modernized.and.presentable

import scala.util.parsing.combinator.RegexParsers
import scalatags.Text.all._

/**
 * @author John Sullivan
 */
object ScaladocParser extends RegexParsers {
  override val skipWhitespace = false

  private val inlineTags = Seq("''" -> i, // italics
                               "'''" -> b, // boldface
                               "__" -> u, // underline
                               "`" -> code, // monospace
                               "^" -> sup, // superscript
                               ",," -> sub) // subscript

  private def inlineElementParser(boundString:String, tag:ConcreteHtmlTag[String]) =
    boundString ~> "[^\n]+".r <~ boundString ^^ { str => tag(str) }

  val inlineParser = inlineTags.map{case (bound, tag) => inlineElementParser(bound, tag)}.reduce(_ | _)

  val scalaLink = "[[" ~> """(?<=\[\[)[^\s]+(?=\]\])""".r <~ "]]" ^^ { // surrounded by double square brackets with no spaces within
    s => a(href:=s)(s) //todo fix path for these
  }
  val externalLink = "[[" ~> """(?<=\[\[)[^\s]+\s.+(?=\]\])""".r <~ "]]" ^^ {str =>
      val address :: titleSegments = str.split(" ").toList
      a(href:=address)(titleSegments.mkString(" "))
    }

  val codeBlock = "{{{" ~> """(?<=\{\{\{).+(?=\}\}\})""".r <~ "}}}" ^^ {codeString => code(codeString)}
  val catchall = ".*".r ^^ {s => RawFrag(s)}

  val scalaDocsegment = inlineParser | scalaLink | externalLink | codeBlock | catchall

  val scalaDocComment = scalaDocsegment.+ ^^ (_.map(_.render).mkString)


  def docify(scalaDocString:String) = parseAll(scalaDocComment, scalaDocString).get
}
