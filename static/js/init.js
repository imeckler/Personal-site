$(document).ready(function(){
    if ($.url.segment(0)) {
        console.log('poop')
        $('.nav li.'+$.url.segment(0)).addClass('active');
    } else {
        console.log('pee');
        $('.nav .home').addClass('active');
    }
});