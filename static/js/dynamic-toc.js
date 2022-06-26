const tocId = "TableOfContents";
const actualContentId = "actual-content";
let showAll = false;
let currentActiveLinkId = null;
let elemsToHide = [];
let linksById = {};

let headerObserver = null;

function observeHeadings() {
  const links = document.querySelectorAll(`#${tocId} a`);
  const headings = document.querySelectorAll(`${actualContentId} h1,h2,h3,h4`);

  for (const link of links) {
    linksById[link.getAttribute("href")] = link;
  }
  for (const elem of document.querySelectorAll(`#${tocId} ul`)) {
    if (elem.parentElement.id !== tocId) {
      elemsToHide.push(elem);
    }
  }

  headerObserver = new IntersectionObserver(
    (entries) => {
      for (const entry of entries) {
        if (entry.isIntersecting && linksById[`#${entry.target.id}`]) {
          currentActiveLinkId = `#${entry.target.id}`;
          break;
        }
      }
      if (currentActiveLinkId) {
        for (const link of links) {
          link.classList.remove("active");
        }
        linksById[currentActiveLinkId].classList.add("active");

        if (!showAll) {
          hideHeadings();
        }
      }
    },
    {
      threshold: 0.1,
      root: document.querySelector(`${actualContentId}`),
    }
  );

  for (const heading of headings) {
    headerObserver.observe(heading);
  }
  hideHeadings();
}

function observeButtons() {
  const unhideButton = document.getElementById("unhide-all-button");
  const hideButton = document.getElementById("hide-all-button");
  unhideButton.addEventListener("click", () => {
    showAll = true;
    showHeadings();
    unhideButton.classList.add("hidden");
    hideButton.classList.remove("hidden");
  });
  hideButton.addEventListener("click", () => {
    showAll = false;
    hideHeadings();
    hideButton.classList.add("hidden");
    unhideButton.classList.remove("hidden");
  });
  unhideButton.classList.remove("hidden");
}

function hideHeadings() {
  for (const elem of elemsToHide) {
    elem.classList.add("hidden");
  }
  if (!currentActiveLinkId) {
    return;
  }
  for (
    let elem = linksById[currentActiveLinkId];
    (elem = elem.parentElement);
    elem.id !== tocId
  ) {
    elem.classList.remove("hidden");
  }
  for (const elem of linksById[currentActiveLinkId].parentElement.children) {
    elem.classList.remove("hidden");
  }
}

function showHeadings() {
  for (const elem of elemsToHide) {
    elem.classList.remove("hidden");
  }
}

function setUpObserver() {
  if (document.documentElement.clientWidth >= (750 + 350 + 25)) {
    if (headerObserver === null) {
      observeHeadings();
      observeButtons();
    }
  } else {
    if (headerObserver !== null) {
      headerObserver.disconnect();
      headerObserver = null;
      showHeadings();
    }
  }
}

window.addEventListener("load", (event) => {
  if ("IntersectionObserver" in window) {
    setUpObserver();
    window.addEventListener("resize", setUpObserver);
  }
});

window.addEventListener("unload", (event) => {
  headerObserver.disconnect();
});
