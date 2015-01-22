package so.modernized.and

import org.markdown4j.Markdown4jProcessor

/**
 * @author John Sullivan
 */
package object presentable {

  implicit class StringExtras(s:String) {
    private val md = new Markdown4jProcessor
    def markdown = (md process s).replaceAll("<br[ |\t]+/>"," ")

    //todo implement some function to provide scaladoccin'
    def scalaDoc = "Scaladoc of:\n" + s
  }

}
