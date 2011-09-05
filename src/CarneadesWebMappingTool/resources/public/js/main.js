var svgScale = 1;
var svgWrapper = null;
var svgLayout = "radial";
var svgTreeify = "false";
var translate = 'translate(0,0)';   


$(function(){ // init
    
   $("#tabs").tabs();
    
});

function loadSVG() {

    var graphBox = $("#graphbox");
    var path = "/files?layout="+svgLayout+"&treeify="+svgTreeify;

    graphBox.svg();    
    graphBox.svg('get').load(path, onSVGLoad);     
    
}

function onSVGLoad(svgW) {
           
    svgWrapper = svgW;
    svgScale = 1;
        
    $("#graph0").bind("mousewheel", function(event, delta){
        if(delta > 0) {
            // zoom in
            svgScale = svgScale * 1.1;
            $("#graph0").animate({svgTransform: translate+'scale('+svgScale+')'}, 100);
        } else {
            // zoom out
            svgScale = svgScale * 0.9;
            $("#graph0").animate({svgTransform: translate+'scale('+svgScale+')'}, 100);
        }
        $("html:not(:animated), body:not(:animated)").animate({scrollTop: 0}, 500);  
        return true;
    });
    
    $("#graph0").draggable({
        drag : function(event, ui){               
                    translate = 'translate(' + (ui.position.left * 1.8) + ', '+ (ui.position.top * 1.8) + ')';   
                    svgWrapper.getElementById("graph0").setAttribute("transform", translate + ' scale(' + svgScale + ')');  
                }
    });
    
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