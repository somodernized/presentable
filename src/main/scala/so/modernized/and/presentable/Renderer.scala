package so.modernized.and.presentable

import scala.io.Source
import java.io.{File, BufferedWriter, FileWriter}
import scalatags.Text.all._
import java.net.URL

/* The renderer is responsible for taking a list of Sections that have been
 * parsed from a scala source file and generating an html from them.
 */
object Renderer {

  def generate(sects:Seq[Section]) = {
    "<!DOCTYPE html>" + html(
      head(
        script(src:="http://cdnjs.cloudflare.com/ajax/libs/prettify/r224/prettify.js", `type`:="text/javascript"),
        script(src:="https://google-code-prettify.googlecode.com/svn/trunk/src/lang-scala.js", `type`:="text/javascript"),
        link(rel:="stylesheet", `type`:="text/css", href:="file:///Users/johnsullivan/dev/presentable/style.css")
      ),
      table("cellpadding".attr:=0, "cellspacing".attr:=0)(
        tbody(
          sects.zipWithIndex map {case (Section(docOpt, codeOpt), idx) =>
          tr(id:=s"section_$idx")(
            td(cls:="docs")(
              div(cls:="pilwrap")(
                a(cls:="pilcrow", href:=s"#section_$idx")("⋅")
              ),
              RawFrag(docOpt.getOrElse(""))
            ),
            td(cls:="code")(
              pre(code(cls:="prettyprint lang-scala")(RawFrag(codeOpt.getOrElse(""))))
            )
          )}:_*
        )
      )
    )
  }

  def main(args:Array[String]) {

    val sourceText = Source.fromFile(args(0)).mkString
    val out = new BufferedWriter(new FileWriter("res.html"))

    out.write(generate(CommentParser.processDocText(sourceText).get).toString)
    out.flush()
    out.close()

  }
}
