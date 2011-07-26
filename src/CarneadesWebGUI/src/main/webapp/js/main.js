var svgScale = 1;


function sourceOnLoad() {
    var graphBox = $("#graphbox");
    var path = "/CarneadesWebGUI/files?type=svg";
    
    graphBox.svg();
    
    graphBox.svg('get').load(path);     
    svgScale = 1;
    
    graphBox.bind("mousewheel", function(event, delta){
        if(delta > 0) {
            // zoom in
            svgScale += 0.1;
            $("#graph0").animate({svgTransform: 'scale('+svgScale+')'}, 100);
        } else {
            // zoom out
            svgScale -= 0.1;
            $("#graph0").animate({svgTransform: 'scale('+svgScale+')'}, 100);
        }
        $("html:not(:animated), body:not(:animated)").animate({scrollTop: 0}, 500);  
        return true;
    });
    
    graphBox.bind("drag", function(event){
        var translate = 'translate(' + Math.round(event.offsetX/30)*20 + ', '+ Math.round(event.offsetY/30)*20 + ')';   
        $("#graph").svg("get").getElementById("graph0").setAttribute("transform", translate + ' scale(' + svgScale + ')');  
    });
    
}