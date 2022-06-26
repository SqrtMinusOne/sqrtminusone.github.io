const tocId = "TableOfContents";
const actualContentId = "actual-content";
let headerObserver;

function observeHeadings() {
  const links = document.querySelectorAll(`#${tocId} a`);
  const headings = document.querySelectorAll(`${actualContentId} h1,h2,h3,h4`);
  const elemsToHide = [];
  const linksById = {};

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
      let newActiveLinkId;
      for (const entry of entries) {
        if (entry.isIntersecting && linksById[`#${entry.target.id}`]) {
          newActiveLinkId = `#${entry.target.id}`;
          break;
        }
      }
      if (newActiveLinkId) {
        for (const link of links) {
          link.classList.remove("active");
        }
        for (const elem of elemsToHide) {
          elem.classList.add("hidden");
        }
        linksById[newActiveLinkId].classList.add("active");
        for (
          let elem = linksById[newActiveLinkId];
          (elem = elem.parentElement);
          elem.id !== tocId
        ) {
          elem.classList.remove("hidden");
        }
        for (const elem of linksById[newActiveLinkId].parentElement.children) {
          elem.classList.remove("hidden");
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
}

window.addEventListener("load", (event) => {
  if ("IntersectionObserver" in window) {
    observeHeadings();
  }
});

window.addEventListener("unload", (event) => {
  headerObserver.disconnect();
});
