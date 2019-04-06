var nextSide = "left";
var mostRecentMatchup = undefined;
var levelMap = {};
var shouldRefreshNameList = false;
$(setUp);

function setUp() {
	refreshNameList();
	$(" #left .name ").click("left" , recordGame);
	$("#right .name ").click("right", recordGame);
	$( "#left .clear").click("left" , clearPlayer);
	$("#right .clear").click("right", clearPlayer);
	// TODO: bind is deprecated in favor of on
	$(" #left .level").bind("keyup input", "left" , synchronizeLevel);
	$("#right .level").bind("keyup input", "right", synchronizeLevel);
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

	var path = "/matchup/" + encodeURIComponent(left.text()) + "/" + encodeURIComponent(right.text());
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
	$(".player .speed option[value=Low]").prop("selected", true);
	$(".player .level").prop("value", "");
	$("#sync").prop("checked", false);
	$("#game-record-error-box").addClass("hidden");
}

function disableSelectors() {
	$(".player .speed").prop("disabled", true);
	$(".player .level").prop("disabled", true);
	$("#sync").prop("disabled", true);
}

function enableSelectors() {
	$(".player .speed").prop("disabled", false);
	$(".player .level").prop("disabled", false);
	$("#sync").prop("disabled", false);
}

function setMatchup(data, result, jqxhr) {
	if(jqxhr !== mostRecentMatchup) return;
	$("#left  .speed option[value=" + data.left .speed + "]").prop("selected", true);
	$("#right .speed option[value=" + data.right.speed + "]").prop("selected", true);
	$("#left  .level").prop("value", data.left .level);
	$("#right .level").prop("value", data.right.level);
	$("#sync").prop("checked", data.confidence === "Confident");
	levelMap["left" ] = data.leftToRight;
	levelMap["right"] = data.rightToLeft;
	enableSelectors();
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

function recordGame(ev) {
	if($("#left  .name").hasClass("uninitialized") ||
	   $("#right .name").hasClass("uninitialized"))
		return;
	disableSelectors();
	$(".clear").addClass("disabled");

	var winner = ev.data, loser = otherSide(winner);
	var postData =
		{ winner: JSON.stringify(playerSettings(winner))
		, loser : JSON.stringify(playerSettings(loser ))
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

function playerSettings(side) {
	// not just return { ... } because the semicolon insertion algorithm gets
	// it wrong
	var result =
		{ name:           $("#" + side + " .name" ).text()
		, level: parseInt($("#" + side + " .level").prop("value"))
		, speed:          $("#" + side + " .speed").prop("value")
		};
	return result;
}

function otherSide(here) { return (here === "left") ? "right" : "left"; }
