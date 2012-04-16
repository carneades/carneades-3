$(document).ready(function() {
                      onMainReady();
});

function onMainReady() {
    	$("body").data("navi", 999);
	increaseHight();
	setNavi(0);
	// getCookie();
	$(window).resize(function() {
		increaseHight();
	});

	token = readCookie("token");
	if (token != null) {
		// user is known by the Server
		infos = getUserInfos(token);
	}
	$("#container_outer").click(function(e) {
		var target = e.target;
		var loggedin = $("#user");
		var hasTheseElements = loggedin.has(target);
		// only remove the Options Popup if something else is clicked
		if (hasTheseElements.length == 0) {
			$("#useroptions").hide();
			$("#user").removeClass("openedOptions");
		}
	});

	$('#logout').click(function() {
		logout();
	});

	$("#user").click(function() {
		$("#useroptions").show();
		$("#user").addClass("openedOptions");

	});

}

function relaod() {
	window.location.reload();
}

function increaseHight() {

	var windowheight = $(window).height();
	var bodyheight = $("body").height();
	var stagehight = $("#stagebg").height();
	var navi = $("#navi");

	difference = windowheight - bodyheight;
	$("#stagebg").height(stagehight + difference);
	$("#stage").height(stagehight + difference);
	// 20 to fix IE bug
	navi.height(windowheight - navi.offset().top - 20);
}

function setToken(token) {
	createCookie("token", token, 7);
}

function getUserInfos(token) {
	console.log(token);
	$.ajax({
		type : 'POST',
		url : "/impact/api/userinfo",
		data : token,
		dataType : "json",
		contentType : "text/plain",

		success : function(jsonData) {
			console.log(jsonData);
			userName = jsonData.name;
			userPicUrl = jsonData.pic_url;
			error = jsonData.error;
			if (error == "") {
				loggedIn(userName, userPicUrl);
			} else {
				console.log(error);
				removeCookie("token");
			}

		}
	});
}

function loggedIn(name, picurl) {
	console.log(name);
	console.log(picurl);
	if (name != "undefined" && picurl != "undefined") {
		$("#login").hide();
		$("#username")[0].innerHTML = name;
		$("#user").children("img").attr("src", picurl);
		$("#loggedin").css("display", "inline-block");
	}
}

function logout() {
	$.ajax({
		type : 'POST',
		url : "/impact/api/logout",
		data : token,
		success : function() {
			removeCookie("token");
			$("#login").show();
			$("#loggedin").hide();
			location.reload();
		}
	});
}

function setNavi(id) {
	if (id != $("body").data("navi")) {
		$("body").data("navi_last", $("body").data("navi"));
		$("body").data("navi", id);
		updateNavi();
	}
}

function updateNavi() {
	current = $("body").data("navi");
	last = $("body").data("navi_last");

	$(".button.nav" + last).removeClass("active");
	$(".button.nav" + current).addClass("active");

	$("#buttonbg").removeClass("nav" + last);
	$("#buttonbg").addClass("nav" + current);

	$("#stage")[0].innerHTML = "";
	setPage();
}

function setPage() {
	try{
		$.address.path("/");
	}catch(e){
		console.log(e);
	}

	switch ($("body").data("navi")) {
	case 0:
		$("#toollogo").attr("src",
				"toolbox/css/impact-ui/images/toollogos/Icon_Home.png");
		$("#stage")[0].innerHTML = '<div id="rss" class=""></div>';
		var top = $("#rss").position().top;

		$("#rss")
				.rssfeed(
						'https://sites.google.com/a/policy-impact.eu/public/project-updates/posts.xml',
						{
							limit : 5,
							date : true,
							content : true,
							snippet : false,
						},
						function() {
							padding = $("#rss").position().top;
							$("#rss").before("<h2>Project Updates</h2>");
							//$("#rss").height($("#stagebg").height()	- $("#rss").position().top+ padding);

						});

		break;

	case 1:
		$("#toollogo").attr("src", "toolbox/css/impact-ui/images/toollogos/Icon_TopicOverview.png");

		Object.prototype.normalize = function() {
			for(name in this) {
				if(typeof this[name] !== 'function') {
					var normalized = name.replace(/-/g, '_');
					if(normalized !== name) {
						this[normalized] = this[name];
						delete this[name];                            
					}
				}
			}
		}
		$("#stage")[0].innerHTML = '<div id="browser" class=""></div>';
		$("#stage").addClass("toInit");
		$.address.change(url_changed);
		$.address.path("/argumentgraph/copyright");
	    ich.grabTemplates();
		init();

		
		/*
		 * $("#stage")[0].innerHTML = "<iframe id='iframe'" + //"
		 * src='http://87.106.42.134:8080/PolicyModellingTool2/' " + "
		 * src='http://87.106.42.134:8080/policymodellingtool/' " +
		 * "width='100%' scrolling='no' style=''></iframe>";
		 * 
		 * height = $('#stagebg').height()+"px"; console.log(height);
		 * $('#iframe').css("height",height);
		 */
		break;

	case 2:
		$("#toollogo")
				.attr("src", "toolbox/css/impact-ui/images/toollogos/Icon_ArgumentReconstrustion.png");

		$("#stage")[0].innerHTML = "<iframe id='iframe' src='http://localhost/art/index.html' width='100%''></iframe>";

		height = ($('#stagebg').height() - 10) + "px";
		
		$('#iframe').css("height", height);		

		/*
		 * $.ajax({ type : 'Get', url : "toolbox/control/controls.html", success :
		 * function(data) { $("#stage")[0].innerHTML = data;
		 * $("#stage").addClass("toInit"); init(); } });
		 */

		break;

	case 3:
		// AAAAAAAAAAAAAAAAAAAAAAAAARGH
		Object.prototype.normalize = function() {}
	
		$("#toollogo").attr(
				"src",
				"toolbox/css/impact-ui/images/toollogos/"
						+ "Icon_ArgumentAnalysisTrackingVisual.png");
		$.ajax({
			type : 'Get',
			url : "toolbox/nodes.html",
			success : function(data) {
				$("#stage")[0].innerHTML = data;
				$("#stage").addClass("toInit");
				init();
			}
		});
		break;
	case 4: 

		$("#toollogo").attr("src","toolbox/css/impact-ui/images/toollogos/Icon_PolicyModelling.png");
		
		Object.prototype.normalize = function() {
			for(name in this) {
				if(typeof this[name] !== 'function') {
					var normalized = name.replace(/-/g, '_');
					if(normalized !== name) {
						this[normalized] = this[name];
						delete this[name];                            
					}
				}
			}
		}
		
		$("#stage")[0].innerHTML = "<h1>Policy Modeling Tool</h1><div id='pm'></div>";
		$("#stage").addClass("toInit");
		initPM();
	    ich.grabTemplates();
		init();

		break;

	case 5:
		$("#toollogo")
				.attr("src",
						"toolbox/css/impact-ui/images/toollogos/Icon_StructuredConsultation.png");
		/*
		 * $.ajax({ type : 'Get', url :
		 * "http://127.0.0.1:8090/carneadesws/map/aston2", success :
		 * function(data) { $("#stage").svg(data); console.log(data); } });
		 */

		$("#stage")[0].innerHTML = "<iframe id='iframe' src='http://localhost/copyrightScreens/index.php' width='100%'  style=''></iframe>";

		height = ($('#stagebg').height() - 10) + "px";
		
		$('#iframe').css("height", height);

		break;

	case 6:
		$("#toollogo").attr("src",
				"toolbox/css/impact-ui/images/toollogos/Icon_Home.png");
		$.ajax({
			type : 'Get',
			url : "toolbox/local.html",
			success : function(data) {
				$("#stage")[0].innerHTML = data;
				$("#stage").addClass("toInit");
				init();
			}

		});
		break;
	}

	$("#stage").addClass("toInit");
	init();
}

function showLocalContent(question) {
	$.ajax({
		type : 'POST',
		url : "/impact/callotherserver",
		data : "question=" + question,
		success : function(data) {
			$("#result").innerHTML = data;
		}
	});
}

function showTwitterContent(question) {
	$.ajax({
		type : 'POST',
		url : "/impact/calltwitter",
		data : "question=" + question,
		success : function(data) {
			$("#result").html("");
			var tweetsJSON = $.parseJSON(data);

			$.each(tweetsJSON.results, function(i, tweet) {
				// Uncomment line below to show tweet
				// data in Fire Bug console
				// Very helpful to find out what is
				// available in the tweet objects

				// Before we continue we check that we
				// got data
				if (tweet.text !== undefined) {

					// Build the html string for the
					// current tweet
					var tweet_html = '<div class="tweet_text">';
					tweet_html += "<div style='font-weight: bold;'>"
							+ tweet.from_user + ": </div>";
					tweet_html += tweet.text;
					tweet_html += '<a href="http://www.twitter.com/';
					tweet_html += tweet.from_user + '/status/' + tweet.id_str
							+ '">';
					tweet_html += ' [link]<\/a><\/div>';

					// Append html string to
					// tweet_container div
					var res = $("tweetresult");
					$("tweetresult").append(tweet_html);
				}
			});
		}
	});
}

/*
 * function showRemoteContent(pos) { $.ajax({ type : 'POST', url :
 * "/impact/remote", data : "position=" + pos, success : function(data) { pos++;
 * parent.frames[0].document.getElementById("result").innerHTML = "The movie on
 * position " + pos + " is " + data; } }); }
 */

/*
 * function loadSVG(text) { $.ajax({ type : 'POST', url :
 * "/SampleRestServer/server/svg", data : text, contentType : "text/plain",
 * success : function(data) { //
 * document.getElementById("tab-content-conn").innerHTML = "<object // id =
 * 'svg_graphic' type='image/svg+xml' // data='"+data+"'></object>"; } }); }
 */

function loadData() {

	$.ajax({
		type : 'Get',
		url : "/SampleRestServer/server/data",
		success : function(data) {
			// document.getElementById("tab-content-conn").innerHTML = data;
		}
	});
}

function loginGoogle() {

	$.ajax({
		type : 'Get',
		url : "/impact/login/google",
		dataType : "text",
		success : function(url) {

			var authwindow = window.open(url, "");
			console.log(url);
		}
	});
}

function loginFacebook() {

	$.ajax({
		type : 'Get',
		url : "/impact/login/facebook",
		dataType : "text",
		success : function(url) {

			var authwindow = window.open(url, "");
			console.log(url);
		}
	});
}

function getTextAndSubmit(num) {
	$.ajax({
		type : 'GET',
		url : "/impact/node/" + num,
		dataType : "json",
		success : function(data) {
			drawNetwork(data);

		},
		error : function(data) {
			console.log(data);
		}
	});
}

function createCookie(name, value, days) {
	if (days) {
		var date = new Date();
		date.setTime(date.getTime() + (days * 24 * 60 * 60 * 1000));
		var expires = "; expires=" + date.toGMTString();
	} else
		var expires = "";
	document.cookie = name + "=" + value + expires + "; path=/";
}

function readCookie(name) {
	var nameEQ = name + "=";
	var ca = document.cookie.split(';');
	for ( var i = 0; i < ca.length; i++) {
		var c = ca[i];
		while (c.charAt(0) == ' ')
			c = c.substring(1, c.length);
		if (c.indexOf(nameEQ) == 0)
			return c.substring(nameEQ.length, c.length);
	}
	return null;
}

function removeCookie(name) {
	createCookie(name, "", -1);
}

// onMainReady();