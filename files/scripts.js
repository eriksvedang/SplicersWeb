window.onload = function () {
  var converter = new showdown.Converter(),
    text      = $('.markdown').html(),
    html      = converter.makeHtml(text);
  $('.markdown').html(html);

  var cardtitle = $('[name="title"]');
  cardtitle.bind('input', function() {
    $(".title").html(cardtitle.val())
  });

  var cardrules = $('[name="rules"]');
  cardrules.bind('input', function() {
    $(".ability span").html(cardrules.val());
    $(".ability span").each(function () {
      if ($(this).children().length == 0) {
        var newHTML = $(this).html().replace(/\[artificial]/g,'<img src="files/gen_artificial.png" class="inlinesymbol"/>');
        $(this).html(newHTML);
      }
   });
  });

  var cardflavor = $('[name="flavor"]');
  cardflavor.bind('input', function() {
    $(".flavor").html(cardflavor.val())
  });

  var carddom = $('[name="domination"]');
  carddom.bind('input', function() {
    $(".dominance span").html(carddom.val())
  });

  var cardtype = $('[name="cardType"]');
  cardtype.bind('input', function() {
    $(".types span").html(cardtype.val() + " - " + cardsubtype.val())
    $(".dominance span").html(carddom.val())
    $(".card").attr("class","card " + cardtype.val())

  });

  var cardsubtype = $('[name="subType"]');
  cardsubtype.bind('input', function() {
    $(".types span").html(cardtype.val() + " - " + cardsubtype.val())
    $(".dominance span").html(carddom.val())
  });

  var cardg1 = $('[name="gene1"]');
  cardg1.bind('input', function() {
    $(".gene1").html(cardg1.val())
  });

  var cardg2 = $('[name="gene2"]');
  cardg2.bind('input', function() {
    $(".gene2").html(cardg2.val())
  });

  var cardillustration = $('[name="illustration"]');
  cardillustration.bind('input', function() {
    $(".illustration").html("<img src='" + cardillustration.val() + "' >")
  });

  var colors = ['#0AA9C9','#74A57F','#9ECE9A','#A7BED3','#C6E2E9','#F1FFC4','#FFCAAF','#DAB894','#B6CB9E','#92B4A7','#8C8A93','#75B9BE','#B2F7EF','#EFF7F6','#F7D6E0','#F2B5D4','#FBEA79','#C0F3A8','#95AFBA','#E2F89C','#FF829D','#F9704A','#8FFF63','#4D9BF9'];
  var index;
  var a = document.getElementsByClassName('randomcolor');
  for (index = 0; index < a.length; ++index) {
    var b = Math.floor(Math.random() * colors.length);
    var color = colors[b];
    colors.splice(b,1);
    a[index].style.background = color;
  };
}

$(window).scroll(function () {
  var scrollTop = $('#logo').height();
    if($(this).scrollTop()<=scrollTop){
      // alert('header just passed.');
      var smallmenu = document.getElementById('smallmenu')
      smallmenu.style.top = '-40';
    }else{
      var smallmenu = document.getElementById('smallmenu')
      smallmenu.style.top = '-0';
    }
});
