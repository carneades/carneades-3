var svgScale = 1;
var svgWrapper = null;
var svgDeltaX = 0;
var svgDeltaY = 0;
var svgLayout = "radial";
var svgTreeify = "false";


$(function(){ // init
    
   $("#tabs").tabs();
    
});

function loadSVG() {
    var graphBox = $("#graphbox");
    var path = "/CarneadesWebGUI/files?type=svg&layout="+svgLayout+"&treeify="+svgTreeify;
    
    graphBox.svg();    
    graphBox.svg('get').load(path, onSVGLoad); 
    
}

function onSVGLoad(svgW) {
    
    svgWrapper = svgW;
    svgScale = 1;
    svgDeltaX = 0;
    svgDeltaY = 0;
    
    var graphBox = $("#graphbox");
    
    
    //$("#graph0").animate({svgHeight: svgHeight, svgWidth: svgHeight}, 100);       
    //$("graph0", svgWrapper.root()).attr("width", svgWidth);
    //$("graph0", svgWrapper.root()).attr("height", svgHeight);
    
    graphBox.bind("mousewheel", function(event, delta){
        if(delta > 0) {
            // zoom in
            svgScale = svgScale * 1.1;
            $("#graph0").animate({svgTransform: 'scale('+svgScale+')'}, 100);
        } else {
            // zoom out
            svgScale = svgScale * 0.9;
            $("#graph0").animate({svgTransform: 'scale('+svgScale+')'}, 100);
        }
        $("html:not(:animated), body:not(:animated)").animate({scrollTop: 0}, 500);  
        return true;
    });
    
    graphBox.draggable({
        drag : function(event, ui){               
                    var translate = 'translate(' + svgDeltaX+ui.deltaX + ', '+ svgDeltaY+ui.deltaY + ')';   
                    svgWrapper.getElementById("graph0").setAttribute("transform", translate + ' scale(' + svgScale + ')');  
                },
        stop: function(event, ui){
                   svgDeltaX = svgDeltaX + ui.deltaX;
                   svgDeltaY = svgDeltaY + ui.deltaY; 
                }
    });
    
    $("#tabs-1").height($("#wrapper").height());
    
   /* graphBox.bind("drag", function(event, ui){          
        var translate = 'translate(' + svgDeltaX+ui.deltaX + ', '+ svgDeltaY+ui.deltaY + ')';   
        svgWrapper.getElementById("graph0").setAttribute("transform", translate + ' scale(' + svgScale + ')');  
    });
    
    graphBox.bind("stop", function(event, ui){
       svgDeltaX += ui.deltaX;
       svgDeltaY += ui.deltaY; 
    });
    
    // svgHeight = graphBox.height()-100;
    setSize();*/
    
}

function updateSVG() {
    /*var svgWidth = graphBox.width() - 10;
    var svgHeight = 350;
    svgWrapper.configure({height: svgHeight, width: svgWidth},false);*/
        
    if ($("#Treeify").attr("checked")) {
        svgTreeify = "true";
    } else {
        svgTreeify = "false";
    };
    
    svgLayout = $("#Layout").val();
    
    
    $("#graphbox").svg("destroy");
    
    loadSVG();
    
}