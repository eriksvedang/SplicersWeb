
function httpGetAsync(theUrl, element)
{
  var xmlHttp = new XMLHttpRequest();
  xmlHttp.onreadystatechange = function() {
      if (xmlHttp.readyState == 4 && xmlHttp.status == 200) {
          //callback(xmlHttp.responseText, element);
          console.log(xmlHttp.responseText);
      }
  }
  xmlHttp.open("GET", theUrl, true); // true for asynchronous
  xmlHttp.send(null);
}

window.onload = function () {
  smallMenu()

  // markdown converter
  var converter = new showdown.Converter(),
    text      = $('.markdown').html(),
    html      = converter.makeHtml(text);
  $('.markdown').html(html);

  // add to deck
    function onCardAddedToDeck(element) {
        var deckid = $.cookie("deck");
        var cardtitle = element.find('.title').html();

        element.unbind( "click" );

        if (element.hasClass("selectable")) {
            console.log("SELECTABLE");
            element.click(function() {
                httpGetAsync('/remove-card-from-deck?deckId=' + deckid + '&cardTitle=' + cardtitle, element );
                onCardAddedToDeck(element);
            });
            element.removeClass("selectable");
            element.addClass("selected");
        } else if (element.hasClass("selected")) {
            console.log("SELECTED");
            element.click(function() {
                httpGetAsync('/add-card-to-deck?deckId=' + deckid + '&cardTitle=' + cardtitle, element );
                onCardAddedToDeck(element)
            })
            element.removeClass("selected");
            element.addClass("selectable");
        } else {
            console.log("Failed to match class!");
        }
    }

    if (document.cookie.indexOf("deck") >= 0) {
        var deckid = $.cookie("deck");

        $('.selected').each(function () {
            var element = $(this)
            var cardtitle = element.find('.title').html();
            element.attr("href", "#" + cardtitle );
            element.click(function() {
                httpGetAsync('/remove-card-from-deck?deckId=' + deckid + '&cardTitle=' + cardtitle, element );
                onCardAddedToDeck(element);
            });
            element.attr("class","cardLink selected");
            console.log("!");
        });

        $('a:not(.selected) .card').each(function () {
            var element = $(this).parent()
            var cardtitle = element.find('.title').html();
            element.attr("href", "#" + cardtitle );
            element.click(function() {
                httpGetAsync('/add-card-to-deck?deckId=' + deckid + '&cardTitle=' + cardtitle, element );
                onCardAddedToDeck(element);
            });
            element.attr("class","cardLink selectable");
        });
    }

  // add card live preview
  var cardtitle = $('[name="title"]');
  cardtitle.bind('input', function() {
    $(".title").html(cardtitle.val())
  });

  var cardrules = $('[name="rules"]');
  function replaceInlineSymbol () {
    $(".ability span").each(function () {
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

  cardrules.bind('input', function() {
    $(".ability span").html(cardrules.val());
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
      $('[name="domination"]').parent().css( "display", "inline" );
      $('[name="gene1"]').parent().css( "display", "inline" );
      $('[name="gene2"]').parent().css( "display", "inline" );
      $('[name="designer"]').css( "display", "none" );
    }
    if (cardtype.val() == 'Biom') {
      $('[name="cost"]').parent().css( "display", "none" );
      $('[name="startMatter"]').parent().css( "display", "none" );
      $('[name="startCards"]').parent().css( "display", "none" );
      $('[name="domination"]').parent().css( "display", "inline" );
      $('[name="gene1"]').parent().css( "display", "none" );
      $('[name="gene2"]').parent().css( "display", "none" );
      $('[name="designer"]').css( "display", "none" );
    }
    if (cardtype.val() == 'Event') {
      $('[name="cost"]').parent().css( "display", "none" );
      $('[name="startMatter"]').parent().css( "display", "none" );
      $('[name="startCards"]').parent().css( "display", "none" );
      $('[name="domination"]').parent().css( "display", "none" );
      $('[name="gene1"]').parent().css( "display", "inline" );
      $('[name="gene2"]').parent().css( "display", "inline" );
      $('[name="designer"]').css( "display", "none" );
    }
    if (cardtype.val() == 'Mutation') {
      $('[name="cost"]').parent().css( "display", "none" );
      $('[name="startMatter"]').parent().css( "display", "none" );
      $('[name="startCards"]').parent().css( "display", "none" );
      $('[name="domination"]').parent().css( "display", "none" );
      $('[name="gene1"]').parent().css( "display", "inline" );
      $('[name="gene2"]').parent().css( "display", "inline" );
      $('[name="designer"]').css( "display", "none" );
    }
    if (cardtype.val() == 'Splicer') {
      $('[name="cost"]').parent().css( "display", "none" );
      $('[name="startMatter"]').parent().css( "display", "none" );
      $('[name="startCards"]').parent().css( "display", "inline" );
      $('[name="domination"]').parent().css( "display", "none" );
      $('[name="gene1"]').parent().css( "display", "none" );
      $('[name="gene2"]').parent().css( "display", "none" );
      $('[name="designer"]').css( "display", "none" );
    }
  }
  dynamicForm()
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

  // random color backgrounds

  var colors = ['#0AA9C9','#74A57F','#9ECE9A','#A7BED3','#C6E2E9','#F1FFC4','#FFCAAF','#DAB894','#B6CB9E','#92B4A7','#8C8A93','#75B9BE','#B2F7EF','#EFF7F6','#F7D6E0','#F2B5D4','#FBEA79','#C0F3A8','#95AFBA','#E2F89C','#FF829D','#F9704A','#8FFF63','#4D9BF9'];
  var colors2 = [];
  var index;
  var a = document.getElementsByClassName('randomcolor');
  for (index = 0; index < a.length; ++index) {
    var b = Math.floor(Math.random() * colors.length);
    var color = colors[b];
    colors2.push(color);
    colors.splice(b,1);
    if (colors.length == 0) {
      colors = colors2
      colors2 = []
    };
    a[index].style.background = color;

    //deck editing
    if (document.cookie.indexOf("deck") >= 0) {
      var deckid = $.cookie("deck");
      var currentdeck = $('[name = "deckid"]').val();
      if (deckid == currentdeck) {
        $('.whileediting').each(function () {$(this).attr("style", "display:inline;")});
      } else {
        $('.deckedit input').attr("readonly", "true");
      }
    } else {
      $('.deckedit input').attr("readonly", "true");
    }

  };



    // title search
  $('[name="filter"]').change( function () {
    var filter = $(this).val();
    $('div').find(".title").parent().css( "display", "inline-block" );
    if (filter) {
      $('div').find(".title:not(:Contains(" + filter + "))").parent().css( "display", "none" );
      $('div').find(".title:Contains(" + filter + ")").parent().css( "padding", "inline-block" );
    }
    return false;
  })
  .keyup( function () {
  			$(this).change();
  });

  $('[name="deckname"]').change( function() {
    var name = $(this).val();
    var deckid = $.cookie("deck");
    httpGetAsync('/set-deck-name?deckId='+ deckid +'&deckName='+name);
  });

};

  // hide small menu
function smallMenu () {
  var scrollTop = $('#logo').height();
  if($(this).scrollTop()<=scrollTop){
    // alert('header just passed.');
    var smallmenu = document.getElementById('smallmenu')
    smallmenu.style.top = '-40';
  }else{
    var smallmenu = document.getElementById('smallmenu')
    smallmenu.style.top = '-0';
  }
}
$(window).scroll(function () {
  smallMenu()
});

// case insensitive search
jQuery.expr[':'].Contains = function(a,i,m){
  return (a.textContent || a.innerText || "").toUpperCase().indexOf(m[3].toUpperCase())>=0;
};

//remove cookie
function removeCookie() {
  $.removeCookie('deck', { path: '/' });
  location.reload();
}

function deleteDeck() {
  var deckid = $.cookie("deck");
  httpGetAsync('/delete-deck?deckId='+ deckid);
  $.removeCookie('deck', { path: '/' });
  window.location.href = '/player';
}
