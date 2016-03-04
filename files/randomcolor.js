
window.onload = function () {
  var colors = ['#ff0000', '#00ff00', '#0000ff'];
  var index;
  var a = document.getElementsByClassName('randomcolor');
  for (index = 0; index < a.length; ++index) {
    a[index].style.background = colors[Math.floor(Math.random() * colors.length)];
  }
}
