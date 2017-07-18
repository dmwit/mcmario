var nextSide = "left";
var mostRecentMatchup = undefined;
var levelMap = {};
$(setUp);

function setUp() {
	refreshNameList();
	$( "#left .clear").click(function() { clearPlayer("left" ); });
	$("#right .clear").click(function() { clearPlayer("right"); });
	$(" #left .level").bind("keyup input", "left" , synchronizeLevel);
	$("#right .level").bind("keyup input", "right", synchronizeLevel);
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
	var side;
	if($("#left .name").hasClass("uninitialized"))
		side = "left";
	else if($("#right .name").hasClass("uninitialized"))
		side = "right";
	else
		side = nextSide;

	nextSide = otherSide(side);
	here  = $("#" + side     + " .name");
	there = $("#" + nextSide + " .name");

	here.text(this.textContent).removeClass("uninitialized");
	refreshMatchup();
}

function refreshMatchup() {
	var left  = $( "#left .name");
	var right = $("#right .name");
	if(left.hasClass("uninitialized") || right.hasClass("uninitialized"))
		return;

	var path = "/matchup/" + left.text() + "/" + right.text();
	disableSelectors();
	mostRecentMatchup = $.get(path, setMatchup);
}

function clearPlayer(side) {
	$("#" + side + " .name").text("???").addClass("uninitialized");
	disableSelectors();
}

function disableSelectors() {
	$(".player .speed option[value=Low]").attr("selected", "true");
	$(".player .speed").prop("disabled", true);
	$(".player .level").prop("disabled", true).attr("value", "");
	$("#sync").prop("checked", false).prop("disabled", true);
}

function enableSelectors() {
	$(".player .speed").prop("disabled", false);
	$(".player .level").prop("disabled", false);
	$("#sync").prop("disabled", false);
}

function setMatchup(data, result, jqxhr) {
	if(jqxhr !== mostRecentMatchup) return;
	$("#left  .speed option[value=" + data.left .speed + "]").attr("selected", "true");
	$("#right .speed option[value=" + data.right.speed + "]").attr("selected", "true");
	$("#left  .level").attr("value", data.left .level);
	$("#right .level").attr("value", data.right.level);
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

function otherSide(here) { return (here === "left") ? "right" : "left"; }
