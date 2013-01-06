var eventList = $events$
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