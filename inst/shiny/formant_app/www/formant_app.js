// js functions needed for formant_app
// NB: ui.R should include "includeScript("www/formant_app.js")"

document.addEventListener('DOMContentLoaded', () => {
  // execute when the page is loaded

  // Functions imported by shinyjs - must begin with "shinyjs." and have "params"
  shinyjs.playme_js = function(params) {
    // named params: audio_i, from, to
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
    // named params: audio_id
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
    // params: parentDiv
    // alert('inheritSize');
    var plotDiv = document.getElementById(params.parentDiv).children;
    for (var i = 0; i < plotDiv.length; i++) {
      plotDiv[i].style.height = 'inherit';
    }
  };

  // Move the scrollbar as ordered from R
  shinyjs.scrollBar = function(params) {
    // params: id, width, left
    var sl = document.getElementById(params.id);
    sl.style.width = params.width;
    sl.style.left = params.left;
  };


  // Functions for making the scrollbar draggable
  // (see https://codepen.io/thenutz/pen/VwYeYEE for inspiration)
  const slider = document.getElementById('scrollBar');
  const track = document.getElementById('scrollBarCont');
  var max_right = track.clientWidth - slider.clientWidth;
  var mouseDown = false;
  var startX;
  var scrollBarLeft = 0, scrollBarLeft_new = 0;
  slider.style.left = scrollBarLeft + 'px';

  rmPx = function(x) {
    return(Number(x.slice(0, -2)));
  };
  // rmPx('200px');  // returns "200" as a number

  slider.addEventListener('mousedown', (e) => {
    mouseDown = true;
    slider.classList.add('active');
    scrollBarLeft = rmPx(slider.style.left);
    startX = e.pageX;
    // prevent the slider from going beyond its track
    max_right = track.clientWidth - slider.clientWidth;
  });

  document.addEventListener('mouseup', () => {
    mouseDown = false;
    slider.classList.remove('active');
    // send to R: scrollBarLeft as proportion of track width
    // https://shiny.rstudio.com/articles/communicating-with-js.html
    Shiny.setInputValue('scrollBarLeft', scrollBarLeft_new / track.clientWidth);
  });

  document.addEventListener('mousemove', (e) => {
    // NB: could be attached to slider or track instead of document,
    // but this way the slider can be dragged regardless of the vertical
    // position of the mouse - handy, ~Audacity
    if(!mouseDown) return;
    e.preventDefault();
    scrollBarLeft_new = scrollBarLeft + e.pageX - startX;

    // prevent the slider from going into negative or beyond audio dur
    if (scrollBarLeft_new < 0) {
      scrollBarLeft_new = 0;
    }
    if (scrollBarLeft_new > max_right) {
        scrollBarLeft_new = max_right;
    }

    // move the slider to the next pos
    slider.style.left = scrollBarLeft_new + 'px';
  });
  // Shiny.onInputChange(scrollBarLeft_new, scrollBarLeft_new);

  // scroll by clicking the track left/right of the scrollbar
  track.addEventListener('mousedown', (e) => {
    var sl_rect = slider.getBoundingClientRect();
    if (e.clientX < sl_rect.left) {
      Shiny.setInputValue('scrollBarMove', 'l' + Math.random());
      // see https://shiny.rstudio.com/articles/communicating-with-js.html
      // with priority: event" it fires twice
    } else if (e.clientX > sl_rect.right) {
      Shiny.setInputValue('scrollBarMove', 'r' + Math.random());
    }
  });


  // Event listeners for hotkeys
  $(document).on("keydown", function (e) {
    Shiny.onInputChange("userPressedSmth", e.which + Math.random() / 3);
    // w/o Math.random() only the first of a series of identical
    // keydown events is sent to server()
  });

    // prevent spacebar from activating the last pressed button
    // see https://stackoverflow.com/questions/22280139/prevent-space-button-from-triggering-any-other-button-click-in-jquery
    $(document).keyup(function(event) {
      if(event.which === 32) {
  	    event.preventDefault();
      }
    });



  // Working with the annotation table
  // Highlight the row with currentAnn
  // https://stackoverflow.com/questions/4524661/how-to-tell-which-row-number-is-clicked-in-a-table/37331546
  function highlightRow(r) {
    var annTbl_rows = document.querySelectorAll('#ann_table table tbody tr');
    for (var i = 0; i < annTbl_rows.length; i++) {
      if (i == (r-1)) {
        // highlight the active annotation
        annTbl_rows[r-1].classList.add('selected');
      } else {
        // reset all other rows to default color
        annTbl_rows[i].classList.remove('selected');
      }
    }
  }

  document.querySelector('#ann_table').onclick = function(ev) {
    // ev.target <== td element
    // ev.target.parentElement <== tr
    // debugger;
    var tn = ev.target.constructor.name;
    if (tn == 'HTMLTableCellElement') {
      // click inside a table row
      let r = ev.target.parentElement.rowIndex;
      if (r > 0) {
        // not the header: send word to R
    		Shiny.setInputValue('tableRow', r);
        highlightRow(r);  // done from R anyway b/c currentAnn can change in different ways
      }
    }
  };

  Shiny.addCustomMessageHandler('highlightRow', function(message) {
    highlightRow(Number(message));
  });


});
