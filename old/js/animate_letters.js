function l(o) { console.log(o) }
document.addEventListener("DOMContentLoaded", function() {

    /*perspective: 1000,
    			origin: '50% 100%',
    			in: {
    				duration: 400,
    				delay: function(el, index) { return index*50; },
    				easing: 'easeOutSine',
    				opacity: 1,
    				rotateY: [-90,0]
    			},
    			out: {
    				duration: 200,
    				delay: function(el, index) { return index*50; },
    				easing: 'easeOutSine',
    				opacity: 0,
    				rotateY: 45
    			}*/


    // animate intro message 
    // Wrap every letter in a span
    var words = document.querySelectorAll('.text-slider .letters');
    [].forEach.call(words, function(word) {
        word.innerHTML = word.innerHTML.replace(/(\w|\.)/g, "<span class='letter'>$&</span>");
    });

    function lettersIn(id) {
        return {
            targets: "#" + id + " .letter",
            rotateY: [-90, 0],
            opacity: [0, 1],
            duration: 1300,
            delay: function(el, i) {
                return 45 * i;
            }
        };
    }

    function lettersOut(id, id2) {
        return {
            targets: "#" + id + " .letter",
            rotateY: [0, 90],
            opacity: [1, 0],
            easing: "easeInExpo",
            duration: 1300,
            delay: function(el, i) {
                return 45 * i;
            },
            complete: function() {
                document.getElementById(id).setAttribute("style", "display: none;");
                document.getElementById(id2).setAttribute("style", "display: inline-block;");

            }
        };
    }

    anime.timeline({ loop: false })
        .add(lettersIn('do1'))
        .add(lettersOut('do1', 'do2'))
        .add(lettersIn('do2'))
        .add(lettersOut('do2', 'do3'))
        .add(lettersIn('do3'))
        .add(lettersOut('do3', 'do4'))
        .add(lettersIn('do4'))
        .add(lettersOut('do4', 'do1'))
        .add(lettersIn('do1'))
        .add(lettersOut('do1', 'do2'))
        .add(lettersIn('do2'))
        .add(lettersOut('do2', 'do3'))
        .add(lettersIn('do3'))
        .add(lettersOut('do3', 'do4'))
        .add(lettersIn('do4'))
        .add(lettersOut('do4', 'do1'))
        .add(lettersIn('do1'))
        .add(lettersOut('do1', 'do2'))
        .add(lettersIn('do2'))
        .add(lettersOut('do2', 'do3'))
        .add(lettersIn('do3'))
        .add(lettersOut('do3', 'do4'))
        .add(lettersIn('do4'))
        .add(lettersOut('do4', 'do5'))
        .add(lettersIn('do5'));

    // animate hover on the nav
    /* var nav = document.querySelectorAll('#MainNav');

    function animateButton(scale, duration, elasticity) {
        anime.remove(link);
        anime({
            targets: link,
            scale: scale,
            duration: duration,
            elasticity: elasticity
        });
    }

    function enterButton() { animateButton(1.2, 800, 400) };
    function leaveButton() { animateButton(1.0, 600, 300) };

    [].forEach.call(nav, function(link) {
        link.addEventListener('mouseenter', enterButton, false);
        link.addEventListener('mouseleave', leaveButton, false);
    });


    var scrollAnimation = anime({
        targets: '.my-el',
        translateY: 100,
        delay: 200,
        autoplay: false
    }); //scrollAnimation.play();

    //this.DOM.trigger.addEventListener('mouseenter', this.mouseenterFn);
    //this.DOM.trigger.addEventListener('mouseleave', this.mouseleaveFn);
    //this.DOM.trigger.addEventListener('touchstart', this.mouseenterFn);
    //this.DOM.trigger.addEventListener('touchend', this.mouseleaveFn);*/
});