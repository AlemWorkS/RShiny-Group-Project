(function () {
  var carouselTimer = null;
  var waitTimer = null;

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

  function ensureCarousel() {
    if (carouselTimer) return; // already running
    var slides = document.querySelectorAll("#hero-carousel .carousel-image");
    if (slides.length) {
      startCarousel();
      if (waitTimer) clearInterval(waitTimer);
    }
  }

  document.addEventListener("DOMContentLoaded", function () {
    waitTimer = setInterval(ensureCarousel, 800);
    setTimeout(ensureCarousel, 400);
  });

  document.addEventListener("shiny:connected", function () {
    setTimeout(ensureCarousel, 500);
  });

  if (window.Shiny) {
    Shiny.addCustomMessageHandler("restart-carousel", function () {
      if (carouselTimer) {
        clearInterval(carouselTimer);
        carouselTimer = null;
      }
      setTimeout(ensureCarousel, 200);
    });
  }
})();
