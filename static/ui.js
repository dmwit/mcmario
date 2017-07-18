var nextSide = "left";
var mostRecentMatchup = undefined;
$(setUp);

function setUp() {
	refreshNameList();
	$( "#left-clear").click(function() { $( "#left-name").text("???").addClass("uninitialized"); });
	$("#right-clear").click(function() { $("#right-name").text("???").addClass("uninitialized"); });
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
	if($("#left-name").hasClass("uninitialized"))
		side = "left";
	else if($("#right-name").hasClass("uninitialized"))
		side = "right";
	else
		side = nextSide;

	nextSide = (side === "left") ? "right" : "left";
	here  = $("#" + side     + "-name");
	there = $("#" + nextSide + "-name");

	here.text(this.textContent).removeClass("uninitialized");
	refreshMatchup();
}

function refreshMatchup() {
	var left  = $( "#left-name");
	var right = $("#right-name");
	if(left.hasClass("uninitialized") || right.hasClass("uninitialized"))
		return;

	var path = "/matchup/" + left.text() + "/" + right.text();
	disableSelectors();
	mostRecentMatchup = $.get(path, setMatchup);
}

function disableSelectors() {
	// TODO
}

function setMatchup(data, result, jqxhr) {
	if(jqxhr !== mostRecentMatchup) return;
	// TODO
}
