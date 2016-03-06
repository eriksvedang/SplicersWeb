
window.onload = function () {
  var colors = ['#0AA9C9','#74A57F','#9ECE9A','#A7BED3','#C6E2E9','#F1FFC4','#FFCAAF','#DAB894','#B6CB9E','#92B4A7','#8C8A93','#75B9BE','#B2F7EF','#EFF7F6','#F7D6E0','#F2B5D4','#FBEA79','#C0F3A8','#95AFBA','#E2F89C','#FF829D','#F9704A','#8FFF63','#4D9BF9'];
  var index;
  var a = document.getElementsByClassName('randomcolor');
  for (index = 0; index < a.length; ++index) {
    a[index].style.background = colors[Math.floor(Math.random() * colors.length)];
  }
}
// '#FDE74C', '#5ADBFF', '#9BC53D', '#C98CA7', '#E55934','#D8CC34','#FA7921','#FFD105','#1582A5','#7EB77F','#06BEE1','#D99AC5','#746DFF',
