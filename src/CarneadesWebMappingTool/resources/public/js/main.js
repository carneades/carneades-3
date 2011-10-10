var svgScale = 1;
var svgWrapper = null;
var svgLayout = "radial";
var svgTreeify = "false";
var translate = 'translate(0,0)';   
var debug = true;

$(function(){ // init
    // Slider for the graph SVG
    $('#slider').slider({orientation: 'horizontal', value: 0, min: -50, max: 50, step: 1, change: onSliderMove});
    
   $("#tabs").tabs();
    
});

function loadSVG() {
    var graphBox = $("#graphbox");
    var path = debug ? "/files?layout="+svgLayout+"&treeify="+svgTreeify
        : "/CarneadesWebMappingTool/files?layout="+svgLayout+"&treeify="+svgTreeify;

    graphBox.svg();    
    graphBox.svg('get').load(path, onSVGLoad);     
    
}

// var handlerSet = false;
function onSVGLoad(svgW) {

    if($('#graph0').length == 0) {
        alert("An error occured while importing the LKIF file. Maybe you forgot to specify the main issue?");
        return;
    }
    var svgroot = document.getElementsByTagName('svg')[0];
    // SVGPan has a problem if there is already a viewBox, so we remove it
    svgroot.removeAttribute('viewBox');
    
    setSliderValue(0);
    resetSvgRoot();
    setupSVGHandlers(document.documentElement);
    
    svgWrapper = svgW;
    svgScale = 1;
    
    $("#tabs-1").height($("#wrapper").height());
}

function updateSVG() {
    
    if ($("#Treeify").attr("checked")) {
        svgTreeify = "true";
    } else {
        svgTreeify = "false";
    };
    
    svgLayout = $("#Layout").val();
    
    
    $("#graphbox").svg("destroy");
    
    loadSVG();
    
}

var previousSliderValue = 0;

function onSliderMove(event, ui) {
    if(event.originalEvent == undefined) {
        // programmaticaly changed, do nothing
        return;
    }
    
    var plusDelta = 1/3;
    var minusDelta = -plusDelta;    
    var root = getSVGRoot();
    var point = root.createSVGPoint();
    var graph = $('#wrapper');

    point.x = graph.width() / 2;
    point.y = graph.height() / 2;

    var diffValue = ui.value - previousSliderValue;
    var absDiffValue = Math.abs(diffValue);

    /* for better performance we should do only one matrix transformation
       for the zoom but I don't know the inner working of SVGPan
       for that */
    if(diffValue > 0) {
        for(var i = 0; i < absDiffValue; i++) {
            doZoom(root, plusDelta, point);
        }
    } else {
        for(var i = 0; i < absDiffValue; i++) {
            doZoom(root, minusDelta, point);
        }
    }
    
    previousSliderValue = ui.value;    
}


function setupSVGHandlers(root) {
    // we redirect to our mousewheel handler to keep in sync the
    // zoom slider and the zoom state of the SVGPan library.
    setAttributes(getSVGRoot(), {
	"onmouseup" : "handleMouseUp(evt)",
	"onmousedown" : "handleMouseDown(evt)",
	"onmousemove" : "handleMouseMove(evt)",});

    if(navigator.userAgent.toLowerCase().indexOf('webkit') >= 0)
	    window.addEventListener('mousewheel', svgHandleMouseWheel, false); // Chrome/Safari
    else
	window.addEventListener('DOMMouseScroll', svgHandleMouseWheel, false); // Others
}

function getSliderValue() {
    return $('#slider').slider('value');
}

function setSliderValue(value) {
    $('#slider').slider('value', value);
    previousSliderValue = value;
}

function svgHandleMouseWheel(evt) {
    var delta;
    if(evt.wheelDelta)
	delta = evt.wheelDelta / 360; // Chrome/Safari
    else
	delta = evt.detail / -9; // Mozilla
    
    if(delta > 0) {
        if(getSliderValue() < 100) {
            setSliderValue(getSliderValue() + 1);
            handleMouseWheel(evt);
        }        
    } else {
        if(getSliderValue() > -100) {
            setSliderValue(getSliderValue() - 1);
            handleMouseWheel(evt);
        }
    }
}
