document.addEventListener("DOMContentLoaded", function () {
  const images = Array.from(document.querySelectorAll("main img.r-plt"));
  if (images.length === 0) return;

  const lightbox = document.createElement("div");
  lightbox.className = "influ-lightbox";
  lightbox.setAttribute("role", "dialog");
  lightbox.setAttribute("aria-modal", "true");
  lightbox.setAttribute("aria-label", "Expanded figure");
  lightbox.innerHTML = [
    '<button class="influ-lightbox__close" type="button" aria-label="Close expanded figure">&times;</button>',
    '<button class="influ-lightbox__previous" type="button" aria-label="Previous figure">&#8249;</button>',
    '<figure class="influ-lightbox__figure">',
    '  <img class="influ-lightbox__image" alt="">',
    '  <figcaption class="influ-lightbox__caption"></figcaption>',
    '</figure>',
    '<button class="influ-lightbox__next" type="button" aria-label="Next figure">&#8250;</button>'
  ].join("");
  document.body.appendChild(lightbox);

  const expanded = lightbox.querySelector(".influ-lightbox__image");
  const caption = lightbox.querySelector(".influ-lightbox__caption");
  const close = lightbox.querySelector(".influ-lightbox__close");
  const previous = lightbox.querySelector(".influ-lightbox__previous");
  const next = lightbox.querySelector(".influ-lightbox__next");
  let activeIndex = 0;
  let trigger = null;

  function figureCaption(image) {
    const figure = image.closest("figure");
    const labelledCaption = figure && figure.querySelector("figcaption");
    return image.getAttribute("alt") ||
      (labelledCaption && labelledCaption.textContent) || "";
  }

  function show(index) {
    activeIndex = (index + images.length) % images.length;
    const image = images[activeIndex];
    expanded.src = image.currentSrc || image.src;
    expanded.alt = image.alt || "Expanded figure";
    caption.textContent = figureCaption(image);
    caption.hidden = caption.textContent.length === 0;
  }

  function open(image) {
    trigger = image;
    show(images.indexOf(image));
    lightbox.classList.add("is-open");
    document.body.classList.add("influ-lightbox-open");
    close.focus();
  }

  function dismiss() {
    lightbox.classList.remove("is-open");
    document.body.classList.remove("influ-lightbox-open");
    if (trigger) trigger.focus();
  }

  images.forEach(function (image) {
    image.tabIndex = 0;
    image.setAttribute("role", "button");
    image.setAttribute("aria-label", (image.alt || "Figure") + ". Open larger view.");
    image.addEventListener("click", function () { open(image); });
    image.addEventListener("keydown", function (event) {
      if (event.key === "Enter" || event.key === " ") {
        event.preventDefault();
        open(image);
      }
    });
  });

  close.addEventListener("click", dismiss);
  previous.addEventListener("click", function () { show(activeIndex - 1); });
  next.addEventListener("click", function () { show(activeIndex + 1); });
  lightbox.addEventListener("click", function (event) {
    if (event.target === lightbox) dismiss();
  });
  document.addEventListener("keydown", function (event) {
    if (!lightbox.classList.contains("is-open")) return;
    if (event.key === "Escape") dismiss();
    if (event.key === "ArrowLeft") show(activeIndex - 1);
    if (event.key === "ArrowRight") show(activeIndex + 1);
  });
});
