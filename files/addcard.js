


window.onload = function () {
  

  // add card live preview
  var cardtitle = $('[name="title"]');
  cardtitle.bind('input', function() {
    $(".title").html(cardtitle.val())
  });

  var cardrules = $('[name="rules"]');
  function replaceInlineSymbol () {
    $(".ability .rules").each(function () {
      if ($(this).children().length == 0) {
        var newHTML = $(this).html()
        newHTML = $(this).html().replace(/\[air]/g , '<img src="/files/gen_air.png" class="inlinesymbol"/>');
        newHTML = newHTML.replace(/\[artificial]/g , '<img src="/files/gen_artificial.png" class="inlinesymbol"/>');
        newHTML = newHTML.replace(/\[bug]/g , '<img src="/files/gen_bug.png" class="inlinesymbol"/>');
        newHTML = newHTML.replace(/\[fungi]/g , '<img src="/files/gen_fungi.png" class="inlinesymbol"/>');
        newHTML = newHTML.replace(/\[mini]/g , '<img src="/files/gen_mini.png" class="inlinesymbol"/>');
        newHTML = newHTML.replace(/\[plant]/g , '<img src="/files/gen_plant.png" class="inlinesymbol"/>');
        newHTML = newHTML.replace(/\[nautic]/g , '<img src="/files/gen_nautic.png" class="inlinesymbol"/>');
        newHTML = newHTML.replace(/\[sinister]/g , '<img src="/files/gen_sinister.png" class="inlinesymbol"/>');
        newHTML = newHTML.replace(/\[land]/g , '<img src="/files/gen_land.png" class="inlinesymbol"/>');
        $(this).html(newHTML);
      }
    });
  }
  replaceInlineSymbol( )

  var cardrules = $('[name="rules"]');
  cardrules.bind('input', function() {
    $(".rules").html(cardrules.val());
    replaceInlineSymbol( )
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
  function dynamicForm () {
    if (cardtype.val() == 'Ting') {
      $('[name="cost"]').parent().css( "display", "none" );
      $('[name="startMatter"]').parent().css( "display", "none" );
      $('[name="startCards"]').parent().css( "display", "none" );
      $('[name="domination"]').parent().css( "display", "block" );
      $('[name="gene1"]').parent().css( "display", "block" );
      $('[name="gene2"]').parent().css( "display", "block" );
      $('[name="designer"]').css( "display", "none" );
      $('.carddraw').css("display", "none" );
    }
    if (cardtype.val() == 'Biom') {
      $('[name="cost"]').parent().css( "display", "none" );
      $('[name="startMatter"]').parent().css( "display", "none" );
      $('[name="startCards"]').parent().css( "display", "none" );
      $('[name="domination"]').parent().css( "display", "block" );
      $('[name="gene1"]').parent().css( "display", "none" );
      $('[name="gene2"]').parent().css( "display", "none" );
      $('[name="designer"]').css( "display", "none" );
      $('.carddraw').css("display", "none" );
    }
    if (cardtype.val() == 'Event') {
      $('[name="cost"]').parent().css( "display", "none" );
      $('[name="startMatter"]').parent().css( "display", "none" );
      $('[name="startCards"]').parent().css( "display", "none" );
      $('[name="domination"]').parent().css( "display", "none" );
      $('[name="gene1"]').parent().css( "display", "block" );
      $('[name="gene2"]').parent().css( "display", "block" );
      $('[name="designer"]').css( "display", "none" );
      $('.carddraw').css("display", "none" );
    }
    if (cardtype.val() == 'Mutation') {
      $('[name="cost"]').parent().css( "display", "none" );
      $('[name="startMatter"]').parent().css( "display", "none" );
      $('[name="startCards"]').parent().css( "display", "none" );
      $('[name="domination"]').parent().css( "display", "none" );
      $('[name="gene1"]').parent().css( "display", "block" );
      $('[name="gene2"]').parent().css( "display", "block" );
      $('[name="designer"]').css( "display", "none" );
      $('.carddraw').css("display", "none" );
    }
    if (cardtype.val() == 'Splicer') {
      $('[name="cost"]').parent().css( "display", "none" );
      $('[name="startMatter"]').parent().css( "display", "none" );
      $('[name="startCards"]').parent().css( "display", "block" );
      $('[name="domination"]').parent().css( "display", "none" );
      $('[name="gene1"]').parent().css( "display", "none" );
      $('[name="gene2"]').parent().css( "display", "none" );
      $('[name="designer"]').css( "display", "none" );
      $('.carddraw').css("display", "block" );
    }
  }

  //force live preview update on load
  dynamicForm()
  $(".card").attr("class","card " + cardtype.val())

  cardtype.bind('input', function() {
    // adjust form input to card type
    dynamicForm()
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
    $(".gene1").attr("class", "gene1 " + cardg1.val() )
  });

  var cardg2 = $('[name="gene2"]');
  cardg2.bind('input', function() {
    $(".gene2").attr("class", "gene2 " + cardg2.val() )
  });

  var cardillustration = $('[name="illustration"]');
  cardillustration.bind('input', function() {
    $(".illustration").html("<img src='" + cardillustration.val() + "' >")
  });
};
