PM.display_sct_intro = function() {
    var sct_intro = new PM.SctIntro();
    sct_intro.render();
    $('#pm').html(sct_intro.$el);  
};