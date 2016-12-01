window.onload = function () {
  var cardtitle = $('[name="title"]');
  cardtitle.bind('input', function() {
    $(".title").html(cardtitle.val())
  });

  var cardrules = $('[name="rules"]');
  cardrules.bind('input', function() {
    $(".ability span").html(cardrules.val())
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
}
