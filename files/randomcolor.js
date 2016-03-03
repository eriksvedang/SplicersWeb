
window.onload = function () {
  var colors = ['#ff0000', '#00ff00', '#0000ff'];
  var random_color = colors[Math.floor(Math.random() * colors.length)];
  document.getElementById('page').style.background = random_color;
}
