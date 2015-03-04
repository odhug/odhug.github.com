var eventList = [{"eventDescription":"<p><time datetime=\"2012-10-04\">4 октября 2012</time> <a href=\"./posts/2012-10-02-announce.html\">Октябрьская встреча</a></p>\n","eventDate":"2012-10-04"},{"eventDescription":"<p><time datetime=\"2012-11-08\">8 ноября 2012</time> <a href=\"./posts/2012-11-06-announce.html\">Ноябрьская встреча</a></p>\n","eventDate":"2012-11-08"},{"eventDescription":"<p><time datetime=\"2012-12-16\">16 декабря 2012</time> <a href=\"./posts/2012-12-13-announce.html\">Декабрьская встреча</a></p>\n","eventDate":"2012-12-16"},{"eventDescription":"<p><time datetime=\"2013-01-10\">10 января 2013</time> <a href=\"./posts/2013-01-06-announce.html\">Январская встреча</a></p>\n","eventDate":"2013-01-10"},{"eventDescription":"<p><time datetime=\"2013-02-06\">6 февраля 2013</time> <a href=\"./posts/2013-01-31-announce.html\">Февральская встреча</a></p>\n","eventDate":"2013-02-06"},{"eventDescription":"<p><time datetime=\"2013-05-03\">3 мая 2013</time> <a href=\"./posts/2013-02-27-odhac.html\">Haskell-хакатон в Одессе</a></p>\n","eventDate":"2013-05-03"},{"eventDescription":"<p><time datetime=\"2013-03-09\">9 марта 2013</time> <a href=\"./posts/2013-02-28-announce.html\">Мартовская встреча</a></p>\n","eventDate":"2013-03-09"},{"eventDescription":"<p><time datetime=\"2013-04-04\">4 апреля 2013</time> <a href=\"./posts/2013-04-02-announce.html\">Апрельская встреча</a></p>\n","eventDate":"2013-04-04"},{"eventDescription":"<p><time datetime=\"2013-06-21\">21 июня 2013</time> <a href=\"./posts/2013-06-17-announce.html\">Июньская встреча</a></p>\n","eventDate":"2013-06-21"},{"eventDescription":"<p><time datetime=\"2013-07-17\">17 июля 2013</time> <a href=\"./posts/2013-07-13-announce.html\">Июльская встреча</a></p>\n","eventDate":"2013-07-17"},{"eventDescription":"<p><time datetime=\"2013-08-13\">13 августа 2013</time> <a href=\"./posts/2013-08-07-announce.html\">Встреча в августе</a></p>\n","eventDate":"2013-08-13"},{"eventDescription":"<p><time datetime=\"2013-09-19\">19 сентября 2013</time> <a href=\"./posts/2013-09-16-announce.html\">Встреча в сентябре</a></p>\n","eventDate":"2013-09-19"},{"eventDescription":"<p><time datetime=\"2013-10-15\">15 октября 2013</time> <a href=\"./posts/2013-10-03-announce.html\">Haskell: знакомство с экосистемой</a></p>\n","eventDate":"2013-10-15"},{"eventDescription":"<p><time datetime=\"2013-11-14\">14 ноября 2013</time> <a href=\"./posts/2013-10-26-announce.html\">Haskell и ООП</a></p>\n","eventDate":"2013-11-14"},{"eventDescription":"<p><time datetime=\"2013-12-19\">19 декабря 2013</time> <a href=\"./posts/2013-12-05-announcement.html\">Практикум по Haskell</a></p>\n","eventDate":"2013-12-19"},{"eventDescription":"<p><time datetime=\"2014-01-23\">23 января 2014</time> <a href=\"./posts/2014-01-15-announcement.html\">Тестирование и Haskell</a></p>\n","eventDate":"2014-01-23"},{"eventDescription":"<p><time datetime=\"2015-01-22\">22 января 2015</time> <a href=\"./posts/2015-01-16-announcement.html\">Январская встреча</a></p>\n","eventDate":"2015-01-22"},{"eventDescription":"<p><time datetime=\"2015-02-13\">13 февраля 2015</time> <a href=\"./posts/2015-02-12-ann-master-class.html\">Мастер-классы по Haskell</a></p>\n","eventDate":"2015-02-13"},{"eventDescription":"<p><time datetime=\"2015-02-26\">26 февраля 2015</time> <a href=\"./posts/2015-02-13-announcement.html\">Февральская встреча</a></p>\n","eventDate":"2015-02-26"},{"eventDescription":"<p><time datetime=\"2015-02-26\">26 февраля 2015</time> <a href=\"./posts/2015-02-16-announcement.html\">Февральская встреча</a></p>\n","eventDate":"2015-02-26"},{"eventDescription":"<p><time datetime=\"2015-03-05\">5 марта 2015</time> <a href=\"./posts/2015-03-03-haskell-jazz.html\">Мастер-класс по Haskell</a></p>\n","eventDate":"2015-03-05"}]
var noFutureEvents = "Нет предстоящих событий"

var divideEvents = function(){
	var cmpEvents = function(a, b){
		var adate = a.eventDate;
		var bdate = b.eventDate;
		return (adate < bdate) ? -1 : (adate > bdate ? 1 : 0);
		}

	var eventToDescr = function(a){ return a.eventDescription }

	eventList.sort(cmpEvents);

	var today = (new Date()).yyyy_mm_dd();

	var fevents = eventList .filter( function(a){ return a.eventDate >= today } ) .map(eventToDescr);
	var pevents = eventList .filter( function(a){ return a.eventDate <  today } ) .map(eventToDescr);

	if(!fevents.length)
		fevents.push(noFutureEvents);
	if(pevents.length)
		pevents = [pevents.pop()];

	var feventsDom = document.getElementById("fevents");
	if(feventsDom !== undefined)
		feventsDom.innerHTML = fevents.join("\n");

	var peventsDom = document.getElementById("pevents");
	if(peventsDom !== undefined)
		peventsDom.innerHTML = pevents.join("\n");
	}

/* Convert date to YYYY-MM-DD format
 * Inspired by http://stackoverflow.com/questions/3066586/get-string-in-yyyymmdd-format-from-js-date-object
 */
Date.prototype.yyyy_mm_dd = function() {
	var yyyy = this.getFullYear().toString();
	var mm   = (this.getMonth() + 1).toString();
	var dd   = this.getDate().toString();
	return yyyy + "-" + (mm.charAt(1) ? mm : "0" + mm) + "-" + (dd.charAt(1) ? dd : "0" + dd);
  };

/* For old browsers (like IE <=8) we extend Array.prototype with filter()
 * Minified from https://developer.mozilla.org/en-US/docs/JavaScript/Reference/Global_Objects/Array/filter#Compatibility
 */
Array.prototype.filter||(Array.prototype.filter=function(c,f){if(null==this)throw new TypeError;var b=Object(this),g=b.length>>>0;if("function"!=typeof c)throw new TypeError;for(var d=[],a=0;a<g;a++)if(a in b){var e=b[a];c.call(f,e,a,b)&&d.push(e)}return d});

/* For old browsers (like IE <=8) we extend Array.prototype with map()
 * https://developer.mozilla.org/en-US/docs/JavaScript/Reference/Global_Objects/Array/map#Compatibility
 */
Array.prototype.map||(Array.prototype.map=function(d,f){var g,e,a;if(null==this)throw new TypeError(" this is null or not defined");var b=Object(this),h=b.length>>>0;if("function"!==typeof d)throw new TypeError(d+" is not a function");f&&(g=f);e=Array(h);for(a=0;a<h;){var c;a in b&&(c=b[a],c=d.call(g,c,a,b),e[a]=c);a++}return e});

/* Analog of $(document).ready() from jQuery
 * Minified from http://javascript.ru/unsorted/top-10-functions#9-onready
 */
function bindReady(b){function a(){c||(c=!0,b())}var c=!1;if(document.addEventListener)document.addEventListener("DOMContentLoaded",function(){a()},!1);else if(document.attachEvent){if(document.documentElement.doScroll&&window==window.top){var d=function(){if(!c&&document.body)try{document.documentElement.doScroll("left"),a()}catch(b){setTimeout(d,0)}};d()}document.attachEvent("onreadystatechange",function(){document.readyState==="complete"&&a()})}window.addEventListener?window.addEventListener("load",
a,!1):window.attachEvent&&window.attachEvent("onload",a)}
readyList=[];
function onReady(b){readyList.length||bindReady(function(){for(var a=0;a<readyList.length;a++)readyList[a]()});readyList.push(b)};

/* Bind divideEvents on DOMContentLoaded event */
onReady(divideEvents);
