
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
  replaceInlineSymbol( )
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
          deckStatus()
      } else if (element.hasClass("selected")) {
          console.log("SELECTED");
          element.click(function() {
              httpGetAsync('/add-card-to-deck?deckId=' + deckid + '&cardTitle=' + cardtitle, element );
              onCardAddedToDeck(element)
          })
          element.removeClass("selected");
          element.addClass("selectable");
          deckStatus()
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
  if ($(document).find("title").text() == "Splicers - Add Card") {
    // add card live preview
    var cardtitle = $('[name="title"]');
    cardtitle.bind('input', function() {
      $(".title").html(cardtitle.val())
    });

    var cardrules = $('[name="rules"]');

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

    var carddom = $('[name="dominance"]');
    carddom.bind('input', function() {
      $(".dominance span").html(carddom.val())
    });

    var cardtype = $('[name="cardType"]');
    function dynamicForm () {
      if (cardtype.val() == 'Ting') {
        $('[name="startcards"]').parent().css( "display", "none" );
        $('[name="dominance"]').parent().css( "display", "block" );
        $('[name="gene1"]').parent().css( "display", "block" );
        $('[name="gene2"]').parent().css( "display", "block" );
        $('[name="designer"]').css( "display", "none" );
        $('.carddraw').css("display", "none" );
      }
      if (cardtype.val() == 'Biom') {
        $('[name="startcards"]').parent().css( "display", "none" );
        $('[name="dominance"]').parent().css( "display", "block" );
        $('[name="gene1"]').parent().css( "display", "none" );
        $('[name="gene2"]').parent().css( "display", "none" );
        $('[name="designer"]').css( "display", "none" );
        $('.carddraw').css("display", "none" );
      }
      if (cardtype.val() == 'Event') {
        $('[name="startcards"]').parent().css( "display", "none" );
        $('[name="dominance"]').parent().css( "display", "none" );
        $('[name="gene1"]').parent().css( "display", "block" );
        $('[name="gene2"]').parent().css( "display", "block" );
        $('[name="designer"]').css( "display", "none" );
        $('.carddraw').css("display", "none" );
      }
      if (cardtype.val() == 'Mutation') {
        $('[name="startcards"]').parent().css( "display", "none" );
        $('[name="dominance"]').parent().css( "display", "none" );
        $('[name="gene1"]').parent().css( "display", "block" );
        $('[name="gene2"]').parent().css( "display", "block" );
        $('[name="designer"]').css( "display", "none" );
        $('.carddraw').css("display", "none" );
      }
      if (cardtype.val() == 'Splicer') {
        $('[name="startcards"]').parent().css( "display", "block" );
        $('[name="dominance"]').parent().css( "display", "none" );
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

    var cardsubtype = $('[name="subtype"]');
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
  }
  //show filter at /cards
  if (getCurrentFileName() == 'cards') {showFilter()};
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
        $('.notediting').each(function () {$(this).attr("style", "display:none;")})
        $('.whileediting').each(function () {$(this).attr("style", "display:inline;")});
        $('.deckedit input').attr("id", "decknameEdit");
      } else if ($(document).find("title").text() == "Splicers - Cards") {
        $('.notediting').each(function () {$(this).attr("style", "display:none;")})
        $('.whileediting').each(function () {$(this).attr("style", "display:block;")});
      } else {
        $('.deckedit input').attr("readonly", "true");
      }
    } else {
      $('.deckedit input').attr("readonly", "true");
    }

    //deck status (count cards etc)
    function deckStatus () {
      console.log("!")
      if (document.cookie.indexOf("deck") >= 0) {
        var cardsInDeck = 0;
        $(".selected").each( function () {
          cardsInDeck += 1;
        });
        var splicerInDeck = 0;
        $(".selected").children(".splicer").each( function () {
          splicerInDeck += 1;
        });
        var biomsInDeck = 0;
        $(".selected").children(".biom").each( function () {
          biomsInDeck += 1;
        });
        var tingsInDeck = 0;
        $(".selected").children(".ting").each( function () {
          tingsInDeck += 1;
        });
        var eventsInDeck = 0;
        $(".selected").children(".event").each( function () {
          eventsInDeck += 1;
        });
        var mutationsInDeck = 0;
        $(".selected").children(".mutation").each( function () {
          mutationsInDeck += 1;
        });

        var counter = ("Cards: <b>"+cardsInDeck+"</b>/18+   Splicer: <b>"+splicerInDeck+"</b>/1   Biom: <b>"+biomsInDeck+"</b>/2   Ting: <b>"+tingsInDeck+"</b>   Event: <b>" +eventsInDeck+ "</b>   Mutation: <b>" + mutationsInDeck + "</b> <br/>")
        $(".deckcounter").html(counter);
      }else{
        // hide statusbar in /cards
        if ($(document).find("title").text() == "Splicers - Cards") {
          $(".deckstatus").css("display", "none");
        };
        var cardsInDeck = 0;
        $(".card").each( function () {
          cardsInDeck += 1;
        });
        var splicerInDeck = 0;
        $(".splicer").each( function () {
          splicerInDeck += 1;
        });
        var biomsInDeck = 0;
        $(".biom").each( function () {
          biomsInDeck += 1;
        });
        var tingsInDeck = 0;
        $(".ting").each( function () {
          tingsInDeck += 1;
        });
        var eventsInDeck = 0;
        $(".event").each( function () {
          eventsInDeck += 1;
        });
        var mutationsInDeck = 0;
        $(".mutation").each( function () {
          mutationsInDeck += 1;
        });

        var counter = ("Cards: <b>"+cardsInDeck+"</b>/18+   Splicer: <b>"+splicerInDeck+"</b>/1   Biom: <b>"+biomsInDeck+"</b>/2   Ting: <b>"+tingsInDeck+"</b>   Event: <b>" +eventsInDeck+ "</b>   Mutation: <b>" + mutationsInDeck + "</b> <br/>")
        $(".deckcounter").html(counter);
      };
    };
    deckStatus()

  };


    // title search
  $('[name="filter"]').change( function () {
    var filter = $(this).val();
    var type = $('[name="filterType"]').val();
    $(".card").css("display", "inline-block");
    if (filter) {
      $("."+ type +":not(:Contains(" + filter + "))").parent(".card").css( "display", "none" );
      $("."+ type +":Contains(" + filter + ")").parent(".card").css( "display", "inline-block" );
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

function getCurrentFileName(){
    var pagePathName= window.location.pathname;
    return pagePathName.substring(pagePathName.lastIndexOf("/") + 1);
}

function showFilter() {
  $('#filter').attr('style','display: inline-block;')

}
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

function deleteDeck(deckid) {
  if (confirm("Are you sure you want to delete this deck?") == true) {
    $.removeCookie('deck', { path: '/' });
    window.location = '/delete-deck?deckId='+ deckid;
  }
}

function editDeck() {
  var currentdeck = $('[name = "deckid"]').val();
  console.log(currentdeck)
  window.location.href = '/edit-deck/'+ currentdeck
}
