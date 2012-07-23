package code
package snippet

import scala.xml.{NodeSeq, Text}
import net.liftweb._
import util._
import common._
import http._
import rest._
import json._
import JsonAST.JValue
import JsonDSL._
import Helpers._

object Problem extends RestHelper with Logger {

	lazy val boxedItemTemplate = TemplateFinder.findAnyTemplate("templates-hidden/row" :: Nil)
	
	def generateItem(id:Int, itemTemplate:NodeSeq) = {
		val link = SHtml.a(() => js.JsCmds.Alert("clicked"), Text("" + id))
		
		// Append our link
		val item = ("* *+" #> link)(itemTemplate)
		
		// Modify our div id. Needed by js code in index.html
		("* [id]" #> id)(item)
	}
	
	serve {
		case Req("item" :: AsInt(start) :: AsInt(limit) :: Nil, "", GetRequest) => {
			val itemTemplate = boxedItemTemplate openOr NodeSeq.Empty
			var rowItems = NodeSeq.Empty
	
			var trueLimit = limit
			
			// limit to 50 items
			if (start+limit > 50) {
				trueLimit = 50 - start
			}
			
			for (i:Int <- start to start + trueLimit) {
				rowItems ++= generateItem(i, itemTemplate)
			}
			JsonResponse.apply(("items" -> rowItems.toString))
		}
	}
	
	def render = {
		val itemTemplate = boxedItemTemplate openOr NodeSeq.Empty
		var rowItems = NodeSeq.Empty
		
		for (i:Int <- 1 to 20) {
			rowItems ++= generateItem(i, itemTemplate)
		}
		"#content *" #> rowItems
	}
}

