var nextSide = "left";
var mostRecentMatchup = undefined;
var levelMap = {};
var shouldRefreshNameList = false;
$(setUp);

function setUp() {
	refreshNameList();
	// TODO: bind is deprecated in favor of on
	$(" #left .level").bind("keyup input", "left" , synchronizeLevelAndDescribeTie);
	$("#right .level").bind("keyup input", "right", synchronizeLevelAndDescribeTie);
	$("#new-player form").on("submit", addName);
}

function refreshNameList() {
	$.get("/players").done(function(data) {
		nameList = $("#name-list");
		nameList.children().remove();
		data.forEach(function(player) {
			$("#templates .player-name").clone().text(player).appendTo(nameList);
		});
	});
}

function addName() {
	var name = $("#new-player .name").prop("value");
	// Just to filter out mistaken clicks. The server itself doesn't care what
	// name you pick.
	if(name.match(/[a-zA-Z]/)) {
		queueName(name);
		$("#name").val('');
	}
	shouldRefreshNameList = true;
	return false;
}

function queueName(name) {
	$(".name").each(function() {
		if(this.textContent == name)
			$(this).parent().remove();
	});

	var side;
	var leftSize  = $("#left  .name").length;
	var rightSize = $("#right .name").length;
	if     (leftSize < rightSize) side = "left";
	else if(rightSize < leftSize) side = "right";
	else                          side = nextSide;

	nextSide = otherSide(side);
	$("#" + side + " .name-container").slice(3).remove();
	div = $("#templates .name-container").clone();
	div.children(".name").text(name);
	div.prependTo($("#" + side + " .player-list"));
	refreshMatchup();
}

function refreshMatchup() {
	clearSelectors();
	disableSelectors();

	var teams = jsonTeams();
	if(!teams) return;

	var path = "/matchup/" + encodeURIComponent(teams.left ) +
	           "/"         + encodeURIComponent(teams.right);
	mostRecentMatchup = $.get(path, setMatchup);
}

function clearPlayer(img, ev) {
	$(img).parent().remove();
	refreshMatchup();
	// TODO: does this work on IE??
	// see also https://stackoverflow.com/q/387736/791604
	if(ev.stopPropagation) ev.stopPropagation();
	ev.cancelBubble = true;
}

function clearSelectors() {
	$(".team .level").prop("value", "");
	$("#sync").prop("checked", false);
	$("#game-record-error-box").addClass("hidden");
}

function disableSelectors() {
	$(".team .level").prop("disabled", true);
	$("#sync").prop("disabled", true);
	$("#tie").addClass("uninitialized");
}

function enableSelectors() {
	$(".team .level").prop("disabled", false);
	$("#sync").prop("disabled", false);
	$("#tie").removeClass("uninitialized");
}

function setMatchup(data, result, jqxhr) {
	if(jqxhr !== mostRecentMatchup) return;
	$("#left  .level").prop("value", data.left );
	$("#right .level").prop("value", data.right);
	$("#sync").prop("checked", data.confidence === "Confident");
	levelMap["left" ] = data.leftToRight;
	levelMap["right"] = data.rightToLeft;
	enableSelectors();
	describeTie();
}

function synchronizeLevelAndDescribeTie(ev) {
	synchronizeLevel(ev);
	describeTie();
}

function synchronizeLevel(ev) {
	if(!$("#sync").prop("checked")) return;
	var here = ev.data;
	var levelHere = ev.target.value;
	if(!levelHere in levelMap[here]) return;
	var there = otherSide(here);
	var levelThere = levelMap[here][levelHere];
	$("#" + there + " .level").prop("value", levelThere);
}

function describeTie() {
	var tieDescription = $("#templates .overtime").text();
	if($("#left .level").prop("value") !== $("#right .level").prop("value"))
		tieDescription = $("#templates .tie").text();
	$("#tie").text(tieDescription);
}

function jsonTeams() {
	var left  = $( "#left .name"); if( left.length == 0) return false;
	var right = $("#right .name"); if(right.length == 0) return false;
	var  leftNames = [];
	var rightNames = [];
	 left.each(function() {  leftNames.push(this.textContent); });
	right.each(function() { rightNames.push(this.textContent); });
	return {  left: JSON.stringify( leftNames)
	       , right: JSON.stringify(rightNames)
	       };
}

function recordGame(winner) {
	var teams = jsonTeams();
	if(!teams) return;
	disableSelectors();
	$(".team .clear").addClass("disabled");

	var postData =
		{   "blue"           : teams.left
		, "orange"           : teams.right
		,   "blue-multiplier": $("#left  .level").prop("value")
		, "orange-multiplier": $("#right .level").prop("value")
		, "winner"           : winner
		};
	// TODO: why are we getting an XML parsing error every time we record a game?
	$.post("/game", postData, gameRecorded, "text").fail(gameNotRecorded);
}

function gameRecorded() {
	$(".team .name-container").remove();
	refreshMatchup();
	// TODO: occasionally refresh the name list just to get the latest name
	// ordering
	if(shouldRefreshNameList) {
		refreshNameList();
		shouldRefreshNameList = false;
	}
}

function gameNotRecorded() {
	enableSelectors();
	$(".team .clear").removeClass("disabled");
	$("#game-record-error-box").removeClass("hidden");
}

function otherSide(here) { return (here === "left") ? "right" : "left"; }
