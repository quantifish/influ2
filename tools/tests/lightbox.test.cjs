// Dependency-free checks of the actual shared lightbox script. The optional
// JSON input is extracted from every rendered page by check-figure-captions.R.
const assert = require("node:assert/strict");
const fs = require("node:fs");
const vm = require("node:vm");
const path = require("node:path");
const source = fs.readFileSync(path.join(__dirname, "../../pkgdown/extra.js"), "utf8");

function element(text = "") {
  const classes = new Set();
  return {
    textContent: text, dataset: {}, attributes: {}, events: {}, hidden: false,
    classList: { add: x => classes.add(x), remove: x => classes.delete(x), contains: x => classes.has(x) },
    addEventListener(name, action) { this.events[name] = action; },
    setAttribute(name, value) { this.attributes[name] = value; },
    getAttribute(name) { return name === "alt" ? this.alt : (this.attributes[name] ?? null); },
    prepend(node) { this.textContent = node.textContent + this.textContent; },
    appendChild() {},
    focus() { this.focused = true; }
  };
}

function checkPage(page) {
  const makeImage = x => Object.assign(element(), { alt: x.alt, src: x.src });
  const figures = page.figures.map(f => {
    const images = f.images.map(makeImage);
    const caption = f.caption === null ? null : element(f.caption);
    return {
      images, caption,
      querySelector: () => caption,
      querySelectorAll: () => images
    };
  });
  const loose = (page.loose_images || []).map(makeImage);
  const images = figures.flatMap(f => f.images).concat(loose);
  const controls = Object.fromEntries(["image", "caption", "close", "previous", "next"].map(name =>
    [".influ-lightbox__" + name, element()]));
  const lightbox = element();
  lightbox.querySelector = selector => controls[selector];
  const document = element();
  document.body = element();
  document.createElement = tag => tag === "div" ? lightbox : element();
  document.querySelectorAll = selector => selector === "main img.r-plt" ? images : figures;
  vm.runInNewContext(source, { document }, { filename: "pkgdown/extra.js" });
  document.events.DOMContentLoaded();
  if (!images.length) return 0;

  figures.forEach((f, index) => {
    if (!f.caption || !f.images.length) return;
    f.images.forEach(image => {
      const number = "Figure " + (index + 1) + ". ";
      const originalCaption = page.figures[index].caption.trim();
      const expected = originalCaption ? f.caption.textContent.trim() : number + image.alt;
      assert.equal(image.dataset.influLightboxCaption, expected, page.file);
      assert.equal(image.dataset.influFigureNumber, "Figure " + (index + 1));
    });
  });
  images.forEach((image, index) => {
    controls[".influ-lightbox__caption"].scrollTop = 100;
    image.events.click();
    assert.equal(controls[".influ-lightbox__caption"].scrollTop, 0);
    assert.ok(lightbox.classList.contains("is-open"));
    assert.equal(controls[".influ-lightbox__caption"].textContent,
      image.dataset.influLightboxCaption || image.alt || "", page.file + " image " + (index + 1));
    assert.equal(controls[".influ-lightbox__image"].alt, image.alt || "Expanded figure");
    assert.equal(controls[".influ-lightbox__image"].src, image.src);
    assert.ok(controls[".influ-lightbox__close"].focused);
    controls[".influ-lightbox__close"].events.click();
    assert.ok(!lightbox.classList.contains("is-open"));
    assert.ok(image.focused);
  });
  images[0].events.keydown({ key: "Enter", preventDefault() {} });
  controls[".influ-lightbox__caption"].scrollTop = 100;
  controls[".influ-lightbox__next"].events.click();
  assert.equal(controls[".influ-lightbox__caption"].scrollTop, 0);
  const second = images[1 % images.length];
  assert.equal(controls[".influ-lightbox__caption"].textContent,
    second.dataset.influLightboxCaption || second.alt || "");
  document.events.keydown({ key: "ArrowLeft" });
  assert.equal(controls[".influ-lightbox__image"].src, images[0].src);
  document.events.keydown({ key: "Escape" });
  assert.ok(!lightbox.classList.contains("is-open"));
  return images.length;
}

const synthetic = { file: "caption-versus-alt regression", figures: [
  { caption: "Full explanation, including grey bars and uncertainty.", images: [{ alt: "Short accessibility description.", src: "first.png" }] },
  { caption: "A shared caption for two images.", images: [{ alt: "Left panel.", src: "left.png" }, { alt: "Right panel.", src: "right.png" }] },
  { caption: "", images: [{ alt: "Fallback when no caption text exists.", src: "fallback.png" }] }
], loose_images: [{ alt: "Uncaptioned example.", src: "loose.png" }] };
checkPage(synthetic);
checkPage({ file: "empty page", figures: [], loose_images: [] });
const pages = process.argv[2] ? JSON.parse(fs.readFileSync(process.argv[2], "utf8")) : [];
let count = 0;
for (const page of pages) count += checkPage(page);
console.log(`Lightbox checks passed: ${pages.length} rendered pages, ${count} plotted images, plus caption/alt and keyboard regressions.`);
