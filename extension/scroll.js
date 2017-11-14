$(document).ready(function () {
    // Handler for .ready() called.
    /*$('html, body').animate({
        scrollTop: $('#footer').offset().top
    }, 'slow');*/

    //$(window).scrollTop() + $(window).height();

    $('html, body').animate({
      scrollTop: $("#bodyContent *:contains('England'):eq(10)").offset().top
    }, 'slow');
});

//
//$("window").scrollTop($("*:contains('England'):eq(10)").offset().top);
