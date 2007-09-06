<?

////////////////////////////////////////////////////////////////////////////////////////////////////////////
// CLASS NAME      : RSS_GENERATOR                                                                        //
// LANGUAGE        : PHP                                                                                  //
// LANGUAGE VERSION: 5.0 (Kambi versions: PHP 4 compatible)
// AUTHOR          : Julien PACHET                                                                        //
//                   Michalis Kamburelis
// EMAIL           : j|u|l|i|e|n| [@] |p|a|c|h|e|t|.|c|o|m                                                //
// VERSION         : 1.0-kambi1
////////////////////////////////////////////////////////////////////////////////////////////////////////////

////////////////////////////////////////////////////////////////////////////////////////////////////////////
// History:                                                                                               //
// class based from php class easy-rss                                                                    //
//   by Paulius Lescinskas (http://www.phpclasses.org/browse/package/1820.html)                           //
// -------                                                                                                //
//  Date        Version   Actions                                                                         //
// ------------------------------------------------------------------------------------------------------ //
//  10/09/2005  0.9       Tested version                                                                  //
//  10/09/2005  1.0       Prod version                                                                    //
//  Kambi:
//              1.0-kambi1  PHP 4 compatible, guid added
//                          (I need to use this on SourceForge server,
//                          so I have essentially no choice, I have to be
//                          PHP 4 compatible still).
////////////////////////////////////////////////////////////////////////////////////////////////////////////

////////////////////////////////////////////////////////////////////////////////////////////////////////////
// What the class need:                                                                                   //
// * Nothing                                                                                              //
////////////////////////////////////////////////////////////////////////////////////////////////////////////
// What the class do:                                                                                     //
// * Generate RSS feed from array of items                                                                //
////////////////////////////////////////////////////////////////////////////////////////////////////////////

class rss_generator {

	var $encoding="UTF-8";
	var $title="";
	var $language="en-us";
	var $description="";
	var $link="";
	var $generator="rss_generator (version 1.0-kambi1)";

	function rss_generator($title) {
		$this->title=$title;
	}

	/**
	Make an xml document of the rss stream
	@param: items: n row of associative array with theses field:
			'title': title of the item
			'description': short description of the item
			'pubData': publication timestamp of the item
			'link': url to show the item
                        (Kambi+)
                        'guid': guid (any string to uniquely identify this item)
	@result: xml document of rss stream
	**/
	function get($items) {
		$res="";
		// header
		$res.="<?xml version=\"1.0\" encoding=\"".stripslashes($this->encoding)."\"?>\n";
		$res.="<rss version=\"2.0\">\n";
		$res.="\t<channel>\n";
		$res.="\t\t<title><![CDATA[".stripslashes($this->title)."]]></title>\n";
		$res.="\t\t<description><![CDATA[".stripslashes($this->description)."]]></description>\n";
		$res.="\t\t<link>".stripslashes($this->link)."</link>\n";
		$res.="\t\t<language>".stripslashes($this->language)."</language>\n";
		$res.="\t\t<generator>".stripslashes($this->generator)."</generator>\n";
		//items
		foreach($items as $item) {
	        	//$date = date("r", stripslashes($item["pubDate"]));
		        $res.="\t\t<item>\n";
			$res.="\t\t\t<title><![CDATA[".stripslashes($item["title"])."]]></title>\n";
		        $res.="\t\t\t<description><![CDATA[".stripslashes($item["description"])."]]></description>\n";
	        	if (!empty($item["pubDate"]))
				$res.="\t\t\t<pubDate>".date("r", stripslashes($item["pubDate"]))."</pubDate>\n";
			if (!empty($item["link"]))
				$res.="\t\t\t<link>".stripslashes($item["link"])."</link>\n";
                        /* Kambi+ guid */
                        if (!empty($item["guid"]))
				$res.="\t\t\t<guid isPermaLink=\"false\">".stripslashes($item["guid"])."</guid>\n";
			$res.="\t\t</item>\n";
		}
		//footer
		$res.="\t</channel>\n";
		$res.="</rss>\n";
		return $res;
	}
}

?>
