shinyjs.playme_js = function(params) {
  var a = document.getElementById(params.audio_id);
  a.currentTime = params.from;
  let dur_ms = (params.to - params.from) * 1000;
  a.play();
  setTimeout(function() {
    a.pause();
    a.currentTime = params.from;
  }, dur_ms);
  // alert('playing audio');
};

shinyjs.stopAudio_js = function(params) {
  var a = document.getElementById(params.audio_id);
  a.pause();
  a.currentTime = 0;
};

// Manually remove all brush div's in case the brush is not properly cleared
// (seems like a bug in Shiny)
shinyjs.clearBrush = function(params) {
  // select all elements whose id contains "_brush"
  // alert('running clearBrush');
  var myId = document.querySelectorAll('[id*=' + CSS.escape(params.s) + ']');
  for (var i=0; i < myId.length; i++) {
    // remove element (detour via its parentNode)
    myId[i].parentNode.removeChild(myId[i]);
  }
};

// Override Shiny's default assignment of height = '400px' to plot divisions
// (otherwise I can't find a way to make overlaid plots resizable)
shinyjs.inheritSize = function(params) {
  // alert('inheritSize');
  var plotDiv = document.getElementById(params.parentDiv).children;
  for (var i = 0; i < plotDiv.length; i++) {
    plotDiv[i].style.height = 'inherit';
  }
};

// Navigation slider
shinyjs.navSlider = function(params) {
  // params: id, width, left
  var sl = document.getElementById(params.id);
  sl.style.width = params.width;
  sl.style.left = params.left;
};

