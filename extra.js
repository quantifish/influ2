document.addEventListener("DOMContentLoaded", function () {
  const images = Array.from(document.querySelectorAll("main img.r-plt"));
  if (images.length === 0) return;

  const figures = Array.from(document.querySelectorAll("main .figure"));
  figures.forEach(function (figure, index) {
    const caption = figure.querySelector(".caption, figcaption");
    const figureImages = Array.from(figure.querySelectorAll("img.r-plt"));
    if (!caption || figureImages.length === 0) return;

    const number = "Figure " + (index + 1);
    const captionText = caption.textContent.trim();
    const numberLabel = document.createElement("span");
    numberLabel.className = "influ-figure-number";
    numberLabel.textContent = number + ". ";
    caption.prepend(numberLabel);

    figureImages.forEach(function (image) {
      const description = image.alt || captionText;
      image.dataset.influFigureNumber = number;
      image.dataset.influLightboxCaption = number + ". " + description;
    });
  });

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
    return image.dataset.influLightboxCaption || image.getAttribute("alt") || "";
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
    const number = image.dataset.influFigureNumber || "Figure";
    const description = (image.alt || "plot").replace(/[.!?]\s*$/, "");
    image.setAttribute(
      "aria-label",
      number + ": " + description + ". Open larger view."
    );
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
