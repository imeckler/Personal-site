$(document).ready(function(){
    if ($.url.segment(0)) {
        $('.nav li.'+$.url.segment(0)).addClass('active');
    } else {
        $('.nav .home').addClass('active');
    }
});