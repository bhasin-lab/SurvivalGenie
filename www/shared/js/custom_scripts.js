$(document).ready(function(){
    adjustImage();
    createiFrame();
});

$( window ).resize(function() {
  adjustImage();
  createiFrame();
});

function adjustImage(){
  $('img')
      //.attr('src', 'https://www.encodedna.com/images/theme/html5.png')
      .width('800px')
      .height('500px');
}

function createiFrame(){
  $("#iframe").load(function() {
      $(this).height( $(this).contents().find("html").height() );
  });
  $("iframe").css({"position: absolute; height: 100%; border: 0px none transparent; background-color: transparent; padding: 0px;overflow: hidden; ",});
}
