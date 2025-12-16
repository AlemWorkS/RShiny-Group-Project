(function () {
  var carouselTimer = null;

  function startCarousel() {
    var slides = document.querySelectorAll("#hero-carousel .carousel-image");
    if (!slides.length) return;
    slides.forEach(function (s, i) {
      s.classList.toggle("active", i === 0);
    });
    if (carouselTimer) clearInterval(carouselTimer);
    var index = 0;
    carouselTimer = setInterval(function () {
      index = (index + 1) % slides.length;
      slides.forEach(function (s, i) {
        s.classList.toggle("active", i === index);
      });
    }, 3500);
  }

  document.addEventListener("DOMContentLoaded", function () {
    startCarousel();
  });

  document.addEventListener("shiny:connected", function () {
    setTimeout(startCarousel, 500);
  });

  if (window.Shiny) {
    Shiny.addCustomMessageHandler("restart-carousel", function () {
      setTimeout(startCarousel, 300);
    });
  }
})();
