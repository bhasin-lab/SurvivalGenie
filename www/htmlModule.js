$(function() {
  $(document).on('shiny:connected', function(e) {
    callValue();
    myModuleJS();
  });
});

$(window).resize(function() {
  callValue();
});

var setheight = $(window).innerHeight();
var setwidth = $(window).innerHeight();

function myModuleJS(ns) {
//  $('#' + ns + 'htmlout').css('background', '#ecf0f5');
  //console.log(ns);
}

// make sure that the namespace id is read
function callValue(ns) {
  if (ns !== null) {
    var divId = '#' + ns + 'htmlout';
    var dimId = ns + 'dimval';
    console.log(dimId);
    Shiny.setInputValue(dimId, {
      height: setheight,
      width: setwidth
    });
  }

}
// To add css style
// var iframe = document.getElementsByTagName("iframe");
// for (var i = 0; i < iframe.length; i++) {
//   this.style.backgroundColor = "red";
// }
