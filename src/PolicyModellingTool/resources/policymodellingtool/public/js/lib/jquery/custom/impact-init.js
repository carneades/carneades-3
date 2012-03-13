/*****************************************************************************/
/**                                COPYRIGHT                                **/
/*****************************************************************************/
/* Copyright © 2011 User Interface Design GmbH. All Rights Reserved.         */
/* File: impact-init.js                                                      */
/*                                                                           */
/* Description: JQuery initialization of the UI-Elements                     */
/*                                                                           */
/* Last modified at: 21 April 2011                                           */
/*                                                                           */
/* Last modified by: Daniel Kersting <daniel.kersting_ext@uid.com>           */
/*                                                                           */
/* ************************************************************************* */

$(function(){

	/*************************************************************/
	/*                     Text Field Inputs                     */
	/*************************************************************/
	
	// wraps the input with a span tag to apply custom text field input style (stretchable background image)
	$('input[type=text][readonly!=readonly]').wrap('<span class="ui-input-wrap"/>');
	$('input[type=text][readonly!=readonly]').addClass('hint');
	$('input[type=text][readonly=readonly]').wrap('<span class="ui-input-wrap ui-input-wrap-readonly" />');
	
	// Clears the input field on focus
	$('input[type=text][readonly!=readonly]').focus(function() {
		if ($(this).val() == $(this).attr('defaultValue')) {
			$(this).val('');
			$(this).removeClass('hint');
		}
		$(this).parent().addClass("focus");
	});
	
	// Places the default text into input field if it would be empty after loosing focus
	$('input[type=text][readonly!=readonly]').blur(function() {
		if ($(this).val() == '') {
			$(this).val($(this).attr('defaultValue'));
			$(this).addClass('hint');
		}
		$(this).parent().removeClass('focus');
	});
	
	/*************************************************************/
	/*                          Buttons                          */
	/*************************************************************/
	
	// wraps the buttons with a span tag to apply custom button styles (stretchable background image)
	$('button, input:submit').wrap(function(){
		if($(this).attr('disabled') == false){
			$(this).wrap('<span class="ui-button-wrap" />');
		} else {
			$(this).wrap('<span class="ui-button-wrap ui-button-wrap-disabled" />');
		}
	});

	// activates the jQuery button handling 
	$("button, input:submit").button();

	// set special css classes to handle focus, click, and hovers for the wrapping span tag
	$("button, input:submit").focus(function() {
		$(this).parent().addClass("ui-button-wrap-focus");
	});
	$("button, input:submit").blur(function() {
		$(this).parent().removeClass("ui-button-wrap-focus");
	});
	$("button, input:submit").mouseover(function() {
		$(this).parent().addClass("ui-button-wrap-hover");
	});
	$("button, input:submit").mouseout(function() {
		$(this).parent().removeClass("ui-button-wrap-hover");
		$(this).parent().removeClass("ui-button-wrap-active");
	});
	$("button, input:submit").mousedown(function() {
		$(this).parent().addClass("ui-button-wrap-active");
	});
	$("button, input:submit").mouseup(function() {
		$(this).parent().removeClass("ui-button-wrap-active");
	});

	//If a focused button is activated via enter-key or space-key set a class 
	//to style the button like it would be styled if it where clicked
	//Therefore the wrapping div needs a class...
	$("button, input:submit").keydown(function(event) {	   
		switch(event.keyCode) {
			case 13: // enter
				$(this).parent().addClass("ui-button-wrap-active");
				break;
			case 32: // space
				$(this).parent().addClass("ui-button-wrap-active");
				break;
		}
	});
	$("button, input:submit").keyup(function(event) {	   
		switch(event.keyCode) {
			case 13: // enter
				$(this).parent().removeClass("ui-button-wrap-active");
				break;
			case 32: // space
				$(this).parent().removeClass("ui-button-wrap-active");
				break;
		}
	});				

	/*************************************************************/
	/*                        Button Group                       */
	/*************************************************************/

	// Add a div to the button-group for custom styling
	$(".button-group").wrap('<div class="button-group-wrap"></div>');
	$(".toggle-button-group").wrap('<div class="button-group-wrap"></div>');
	
	
	/*************************************************************/
	/*                       Toggle Buttons                      */
	/*************************************************************/
	/* Toggle Buttons are build from <input type="checkbox">     */
	/* jQuery hides this <input> and displays a <label>          */
	/* But the focus will be on the hidden <input> to handle     */
	/* this some classes have to be set to his view.             */  
	/*************************************************************/
	
	// Activate jQuery toggle button handling
	$(".toggle-button").button();
	$(".checkbox-toggle-buttons" ).buttonset();
	$(".radio-toggle-buttons").buttonset();

	//mark the toggle buttons text as not selectable
	$("label.ui-button .ui-button-text").addClass("unselectable");

	//If a focused button is activated via enter-key or space-key set a class 
	//to style the button like it would be styled if it where clicked.
	//Therefore
	$("input.ui-helper-hidden-accessible").keydown(function(event) {	   
		switch(event.keyCode) {
			case 32: // space
				$(this).next().addClass("ui-button-active");
				break;
		}
	});
	$("input.ui-helper-hidden-accessible").keyup(function(event) {	   
		switch(event.keyCode) {
			case 32: // space
				$(this).next().removeClass("ui-button-active");
				break;
		}
	});			

	
	// If the toggle Button has focus set a class to his view
	$("input.ui-helper-hidden-accessible").focus(function() {
		$(this).next().addClass('ui-button-focus');
	});
	// If the toggle Button has no focus remove the class from his view
	$("input.ui-helper-hidden-accessible").blur(function() {
		$(this).next().removeClass('ui-button-focus');
	});


	/*************************************************************/
	/*                 Scroll Bars (jScrollPane)                 */
	/*************************************************************/
	
	// Activate jQuery Scrollbars (plugin jScrollPane)
	$(".scroll-pane").jScrollPane({showArrows: true});

	var setjspDragBottomHeight = function(){
		// Calculate the margin of the scroll bar track for custom styling
		// (the top and bottom images are 5px high)
		$.each($(".jspDragBottom"), function(index, value) { 
			$(this).css('marginTop', $(this).parent().height() - 10);
		});				
	};

	//Make the while page scrollable 				
	$('body').wrapInner('<div id="page-wrap-container"></div>');
	var isResizing;
	var setContainerHeight = function(){
		// IE triggers the onResize event internally when you do the stuff in this function
		// so make sure we don't enter an infinite loop and crash the browser
		if (!isResizing) { 
			isResizing = true;
			$w = $(window);
			$c = $('#page-wrap-container');
			var p = (parseInt($c.css('paddingLeft')) || 0) + (parseInt($c.css('paddingRight')) || 0);
			$('body>.jScrollPaneContainer').css({'height': $w.height() + 'px', 'width': $w.width() + 'px'});
			$c.css({'height': ($w.height()-p) + 'px', 'width': ($w.width() - p) + 'px', 'overflow':'auto'});
			$c.jScrollPane({showArrows: true});
			isResizing = false;
			setjspDragBottomHeight();
		}
	};
	
	$(window).bind('resize', setContainerHeight);
	// it seems like you need to call this twice to get consistantly correct results cross browser...
	setContainerHeight();
	setContainerHeight();

	//Handle the active (clicked) state of the track 
	//(set class jspMouseUpOutsideCatcher to body to get called, if the mouse is release outside the track) 
	$(".jspTrack").mousedown(function() {
		$(this).addClass("jspActive");
		$('body').addClass("jspMouseUpOutsideCatcher");
	});
	$(".jspTrack").mouseup(function() {
		$(this).removeClass("jspActive");
		$('body').removeClass("jspMouseUpOutsideCatcher");
	});
	$(".jspMouseUpCatcher").mouseup(function() {
		$(".jspTrack").removeClass("jspActive");
		$(this).removeClass("jspMouseUpOutsideCatcher");
	});	

	setjspDragBottomHeight();
	
	/*************************************************************/
	/*                        Check boxes                        */
	/*************************************************************/
	/* Check boxes are build from <input type="checkbox">        */
	/* jQuery hides this <input> and displays a <label>          */
	/* But the focus will be on the hidden <input> to handle     */
	/* this some classes have to be set to his view.             */  
	/*************************************************************/
	
	// Enable jQuery check box handling (via plugin)
	$(".checkbox").checkBox();

	//Handle clicked (acitive) states for custom styling
	$("span.ui-checkbox").mousedown(function() {
		$(this).addClass("ui-checkbox-active");
	});
	$("span.ui-checkbox").mouseup(function() {
		$(this).removeClass("ui-checkbox-active");
	});
	$("span.ui-checkbox").mouseout(function() {
		$(this).removeClass("ui-checkbox-active");
	});
	$("label.ui-checkbox").mousedown(function() {
		$("#" + $(this).attr("for")).next().addClass("ui-checkbox-active");
	});
	$("label.ui-checkbox").mouseup(function() {
		$("#" + $(this).attr("for")).next().removeClass("ui-checkbox-active");
	});
	$("label.ui-checkbox").mouseout(function() {
		$("#" + $(this).attr("for")).next().removeClass("ui-checkbox-active");
	});

	// If the check box has focus set a class to his view
	$("input.checkbox.ui-helper-hidden-accessible").focus(function() {
		$(this).next().addClass('ui-checkbox-focus');
	});
	// If the check box has no focus remove the class from his view
	$("input.checkbox.ui-helper-hidden-accessible").blur(function() {
		$(this).next().removeClass('ui-checkbox-focus');
	});

	//If a focused check box is activated space-key set a class 
	//to style the check box like it would be styled if it where clicked
	//Therefore the view needs a class...
	$("input.checkbox.ui-helper-hidden-accessible").keydown(function(event) {	   
		switch(event.keyCode) {
			case 32: // space
				$(this).next().addClass("ui-checkbox-active");
				break;
		}
	});
	$("input.checkbox.ui-helper-hidden-accessible").keyup(function(event) {	   
		switch(event.keyCode) {
			case 32: // space
				$(this).next().removeClass("ui-checkbox-active");
				break;
		}
	});
	
	/*************************************************************/
	/*                       Radio buttons                       */
	/*************************************************************/
	/* Radio Buttons are build from <input type="radio">         */
	/* jQuery hides this <input> and displays a <label>          */
	/* But the focus will be on the hidden <input> to handle     */
	/* this some classes have to be set to his view.             */
	/* So this goes analogous to Check boxes                     */  
	/*************************************************************/
	
	// Enable jQuery radio button handling (via plugin)
	$(".radiobutton").checkBox();

	//Handle clicked (acitive) states for custom styling
	$("span.ui-radio").mousedown(function() {
		$(this).addClass("ui-radio-active");
	});
	$("span.ui-radio").mouseup(function() {
		$(this).removeClass("ui-radio-active");
	});
	$("span.ui-radio").mouseout(function() {
		$(this).removeClass("ui-radio-active");
	});
	$("label.ui-radio").mousedown(function() {
		$("#" + $(this).attr("for")).next().addClass("ui-radio-active");
	});
	$("label.ui-radio").mouseup(function() {
		$("#" + $(this).attr("for")).next().removeClass("ui-radio-active");
	});
	$("label.ui-radio").mouseout(function() {
		$("#" + $(this).attr("for")).next().removeClass("ui-radio-active");
	});

	// If the check box has focus set a class to his view
	$("input.radiobutton.ui-helper-hidden-accessible").focus(function() {
		$(this).next().addClass('ui-radio-focus');
	});
	// If the check box has no focus remove the class from his view
	$("input.radiobutton.ui-helper-hidden-accessible").blur(function() {
		$(this).next().removeClass('ui-radio-focus');
	});

	
	/*************************************************************/
	/*               Drop Down Menu (Select boxes)               */
	/*************************************************************/
	/*      Combobox: Editable Drop Down Menu (Select boxes)     */
	/*************************************************************/
	/* Technically both are the same, with one difference, the   */ 
	/* input of the Combobx is enabled, that of the Drop         */
	/* Down is disabled (readonly)                               */
	/*************************************************************/
	
	//First, we need a wrapping span for css styling
	$(".dropdown-menu").wrap('<div class="dropdown-menu-wrap"></div>');
	$(".combobox").wrap('<div class="combobox-menu-wrap"></div>');

	// Enable jQuery Drop Down Menu (Select boxes)
	$(".dropdown-menu, .combobox").selectbox();

	// The dropdown must have same with as the box above it
	$(".dropdown-menu, .combobox").each(function(){
		$(this).prev().css("width", $(this).width());
		$(this).prev().prev().css("width", $(this).width());
	});

	//Enable editing for combobox only
	$(".combobox-menu-wrap input.selectbox").removeAttr('readonly');
	
	//This little hack unhides (opens) the selectbox, adds the custum scrollbars and closes the select box
	//This is requiered because scrollbars must know its size which can not be determined in closed state. 
	$(".selectbox-wrapper").show();
	$(".selectbox-wrapper").jScrollPane({showArrows: true});
	$(".selectbox-wrapper").hide();
	
	// Calculate the margin of the scroll bar track for custom styling
	$.each($(".jspDragBottom"), function(index, value) { 
		$(this).css('marginTop', $(this).parent().height() - 10);
	});	

	// The pane is to small (highlighted rows have 4px margin between scrollbar and begin of highlighted row...
	$.each($(".selectbox-wrapper .jspContainer .jspPane"), function(index, value) {
		$(this).width($(this).width() + 4);
	});

	// To style the box we need a wrapping div and a empty span for the arrow (the box is stretchable)
	$("input.selectbox").wrap('<div class="selectbox-wrap" />');
	$("div.selectbox-wrap").append('<span class="selectbox-icon-wrap"><span class="selectbox-icon"></span></span>');
	
	// A Click on the arrow should open the drop down
	$(".selectbox-icon-wrap").click(function() {
		var target = $(this).parent().next();
		if($(target).hasClass("selectbox-wrapper")){
			$(this).prev().focus();
		}
	});

	//Add a class for custom styling of the dropdwon has focus
	$("input.selectbox").focus( function(){
		$(this).parent().addClass('selectbox-focus');
	});
	$("input.selectbox").blur( function(){
		$(this).parent().removeClass('selectbox-focus');
	});	

	//If the selectbox was disabled, the drop down is made disabled too
	$(".dropdown-menu[disabled=disabled], .combobox[disabled=disabled]").prev().remove();
	$(".dropdown-menu[disabled=disabled], .combobox[disabled=disabled]").prev().addClass("selectbox-disabled");
	$(".selectbox-disabled input").attr('readonly', true);

	//make the selectbox text unselectable
	$(".dropdown-menu-wrap input.selectbox").addClass("unselectable");
	$(".selectbox-disabled input.selectbox").addClass("unselectable");
	$(".selectbox-disabled input.selectbox").focus(function(){$(this).blur();});
	$(".selectbox-wrapper ul li").addClass("unselectable");

	//the input field should not show a carret symbol if clicked
	$(".dropdown-menu-wrap input.selectbox").onmousedown=function(){return false;};
	$(".selectbox-disabled input.selectbox").onmousedown=function(){return false;};
	$(".selectbox-wrapper ul li").onmousedown=function(){return false;};
	
	
	/*************************************************************/
	/*                        Progress Bar                       */
	/*************************************************************/
	
	// Activate the jQuery Progress Bar
	$(".progressbar").progressbar( {

	});
	
	//Need some <div> for custom styling (stretchable background)
	$(".ui-progressbar").wrapInner('<div class="ui-progressbar-inner"></div>');
	$(".ui-progressbar-value").wrapInner('<div class="ui-progressbar-value-inner"></div>');


	/*************************************************************/
	/*                           Tabs                            */
	/*************************************************************/

	// Tabs
	$('.tabs').tabs();

	/*************************************************************/
	/*                         Sliders                           */
	/*************************************************************/
	
	// Slider
	$('.slider').slider();
	$('.disabled-slider').slider({
		disabled: true
	});

	$('.ui-slider').wrapInner('<div class="ui-slider-inner"></div>');



	/*************************************************************/
	/*                   Date- and Timepicker                    */
	/*************************************************************/
	
	// Date picker
	$('.datepicker-inline').datepicker( {
		inline : true
	});

	$('.datepicker').datepicker( {
		showButtonPanel: true
	});

	$('.datepicker-button').datepicker( {
		showButtonPanel: true,
		showOn: "button",
		buttonImage: "css/custom-theme/images/calendar.gif",
		buttonImageOnly: true
	});

	$('.ui-datepicker').wrap('<div class="ui-datepicker-wrap"><div>');
	
	
	// Time picker
	$('.timepicker-inline').timepicker( {
		showPeriodLabels: false
	});

	$('.timepicker').timepicker( {
		showPeriodLabels: false,
		showOn: 'both'
	});

	$('.timepicker-button').after('<img id="timepicker-button-trigger" src="css/custom-theme/images/calendar.gif"');
	
	$('.timepicker-button').timepicker( {
		showOn: 'button',
		button: "#timepicker-button-trigger",
		showPeriodLabels: false
	});

});