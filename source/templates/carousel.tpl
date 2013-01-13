/* -------------------------------Carousel--------------------------------*/

.carousel {
	border: 10px solid #ffffff;
	box-shadow: 0 0.3em 0.8em #000000;
	margin: 0 auto;
	padding: 0;
}

.horizontal {
	width: $itemsize$px;
	position: relative;
	overflow: hidden;
}

.horizontal .item {
	margin: 0;
	float: left;
	width: $itemsize$px;
}

.horizontal .items {
	width: $maxsize$px;
	-webkit-animation: hscroll 20s infinite;
	   -moz-animation: hscroll 20s infinite;
	    -ms-animation: hscroll 20s infinite;
	     -o-animation: hscroll 20s infinite;
	        animation: hscroll 20s infinite;
}

@-webkit-keyframes hscroll {
 $items$	
}
@-moz-keyframes hscroll {
 $items$ 
}
@-ms-keyframes hscroll {
$items$
}
@-o-keyframes hscroll {
$items$
}
@keyframes hscroll {
$items$
}
