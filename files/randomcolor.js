
window.onload = function () {
  var colors = ['#C5AFA4', '#CC7E85', '#CF4D6F', '#C98CA7', '#9CC4B2','#D8CC34','#E5B769','#EDC9FF','#1582A5','#FE9000'];
  var index;
  var a = document.getElementsByClassName('randomcolor');
  for (index = 0; index < a.length; ++index) {
    a[index].style.background = colors[Math.floor(Math.random() * colors.length)];
  }
}
