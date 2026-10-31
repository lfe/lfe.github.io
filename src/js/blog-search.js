import "/pagefind/pagefind-component-ui.js";

await Promise.all([
  customElements.whenDefined("pagefind-input"),
  customElements.whenDefined("pagefind-summary"),
  customElements.whenDefined("pagefind-results"),
]);

const manager = window.PagefindComponents?.getInstanceManager();
const instance = manager?.getInstance("default");

if (instance) {
  const updateQuery = (term) => {
    const url = new URL(window.location.href);

    if (term.trim() === "") {
      url.searchParams.delete("q");
    } else {
      url.searchParams.set("q", term);
    }

    try {
      window.history.replaceState(window.history.state, "", url);
    } catch {
      // Search should still work if this page cannot update its URL.
    }
  };

  instance.on("search", (term) => updateQuery(term));

  const query = new URLSearchParams(window.location.search).get("q") ?? "";

  if (query.trim() === "") {
    if (query !== "") updateQuery("");
  } else {
    instance.triggerSearch(query);
  }
}
