shinyjs.playme_js = function(params) {
  var a = document.getElementById(params.audio_id);
  a.currentTime = params.from;
  let dur_ms = (params.to - params.from) * 1000;
  a.play();
  setTimeout(function() {
    a.pause();
    a.currentTime = params.from;
  }, dur_ms);
};

shinyjs.stopAudio_js = function(params) {
  var a = document.getElementById(params.audio_id);
  a.pause();
  a.currentTime = 0;
};
