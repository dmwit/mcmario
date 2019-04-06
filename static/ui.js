var nextSide = "left";
var mostRecentMatchup = undefined;
var levelMap = {};
var shouldRefreshNameList = false;
$(setUp);

function setUp() {
	refreshNameList();
	$(" #left .name ").click("blue"  , recordGame);
	$("#right .name ").click("orange", recordGame);
	$("#tie         ").click("tie"   , recordGame);
	$( "#left .clear").click("left" , clearPlayer);
	$("#right .clear").click("right", clearPlayer);
	// TODO: bind is deprecated in favor of on
	$(" #left .level").bind("keyup input", "left" , synchronizeLevelAndDescribeTie);
	$("#right .level").bind("keyup input", "right", synchronizeLevelAndDescribeTie);
	$("#new-player form").on("submit", addName);
}

function refreshNameList() {
	var replacement = document.createElement("div");
	replacement.id = "name-list";
	$.get("/players").done(function(data) {
		data.forEach(function(player) {
			div = document.createElement("div");
			div.textContent = player;
			div.addEventListener("click", selectName);
			replacement.appendChild(div);
		});
		$("#name-list").replaceWith(replacement);
	});
}

function selectName() {
	queueName(this.textContent);
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
	var side;
	if($("#left .name").hasClass("uninitialized"))
		side = "left";
	else if($("#right .name").hasClass("uninitialized"))
		side = "right";
	else
		side = nextSide;

	nextSide = otherSide(side);
	var here  = $("#" + side     + " .name");
	var there = $("#" + nextSide + " .name");

	here.text(name).removeClass("uninitialized");
	$("#" + side + " .clear").removeClass("disabled");
	refreshMatchup();
}

function refreshMatchup() {
	var left  = $( "#left .name");
	var right = $("#right .name");
	if(left.hasClass("uninitialized") || right.hasClass("uninitialized"))
		return;

	function forHTTP(div) { return encodeURIComponent(JSON.stringify([div.text()])); }
	var path = "/matchup/" + forHTTP(left) + "/" + forHTTP(right);
	clearSelectors();
	disableSelectors();
	mostRecentMatchup = $.get(path, setMatchup);
}

function clearPlayer(ev) {
	var side = ev.data;
	$("#" + side + " .name").text("???").addClass("uninitialized");
	$("#" + side + " .clear").addClass("disabled");
	clearSelectors();
	disableSelectors();
}

function clearSelectors() {
	$(".player .level").prop("value", "");
	$("#sync").prop("checked", false);
	$("#game-record-error-box").addClass("hidden");
}

function disableSelectors() {
	$(".player .level").prop("disabled", true);
	$("#sync").prop("disabled", true);
	$("#tie").addClass("uninitialized");
}

function enableSelectors() {
	$(".player .level").prop("disabled", false);
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
	var tieDescription = "Overtime";
	if($("#left .level").prop("value") !== $("#right .level").prop("value"))
		tieDescription = "Tie"
	$("#tie").text(tieDescription);
}

function recordGame(ev) {
	if($("#left  .name").hasClass("uninitialized") ||
	   $("#right .name").hasClass("uninitialized"))
		return;
	disableSelectors();
	$(".clear").addClass("disabled");

	var postData =
		{   "blue"           : JSON.stringify([$("#left  .name").text()])
		, "orange"           : JSON.stringify([$("#right .name").text()])
		,   "blue-multiplier": $("#left  .level").prop("value")
		, "orange-multiplier": $("#right .level").prop("value")
		, "winner"           : ev.data
		};
	// TODO: why are we getting an XML parsing error every time we record a game?
	$.post("/game", postData, gameRecorded, "text").fail(gameNotRecorded);
}

function gameRecorded() {
	clearPlayer({ data: "left" });
	clearPlayer({ data: "right"});
	// TODO: occasionally refresh the name list just to get the latest name
	// ordering
	if(shouldRefreshNameList) {
		refreshNameList();
		shouldRefreshNameList = false;
	}
}

function gameNotRecorded() {
	enableSelectors();
	$(".clear").removeClass("disabled");
	$("#game-record-error-box").removeClass("hidden");
}

function otherSide(here) { return (here === "left") ? "right" : "left"; }
