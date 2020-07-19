$(document).ready(function() {
  const slider = document.getElementById('navSlider');
  const track = document.getElementById('navSliderCont');
  var max_right = track.clientWidth - slider.clientWidth;
  var mouseDown = false;
  var startX;
  var scrollBarLeft = 0, scrollBarLeft_new = 0;
  slider.style.left = scrollBarLeft + 'px';

  rmPx = function(x) {
    return(Number(x.slice(0, -2)));
  };
  // rmPx('200px');

  slider.addEventListener('mousedown', (e) => {
    mouseDown = true;
    slider.classList.add('active');
    scrollBarLeft = rmPx(slider.style.left);
    startX = e.pageX;
    max_right = track.clientWidth - slider.clientWidth;
  });

  document.addEventListener('mouseup', () => {
    mouseDown = false;
    slider.classList.remove('active');
    // send to R: scrollBarLeft as proportion of track width
    Shiny.setInputValue('scrollBarLeft', scrollBarLeft_new / track.clientWidth);
  });

  document.addEventListener('mousemove', (e) => {
    if(!mouseDown) return;
    e.preventDefault();
    scrollBarLeft_new = scrollBarLeft + e.pageX - startX;
    if (scrollBarLeft_new < 0) {
      scrollBarLeft_new = 0;
    }
    if (scrollBarLeft_new > max_right) {
        scrollBarLeft_new = max_right;
    }
    slider.style.left = scrollBarLeft_new + 'px';
  });

  // Shiny.onInputChange(scrollBarLeft_new, scrollBarLeft_new);
});
