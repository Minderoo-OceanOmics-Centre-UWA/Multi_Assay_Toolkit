// MAT progress popups: when Shiny opens a withProgress() notification, add
// the DNA helix (same markup as dna_helix() in app.R) and a scrolling DNA
// sequence. Styling lives in www/mat_progress.css.
(function () {
  var BASES = "ACGT";

  function helix(n) {
    var html = '<span class="dna-helix mat-progress-helix" aria-hidden="true">';
    for (var i = 0; i < n; i++) html += '<span class="bp" style="--i:' + i + '"><span class="rung"></span></span>';
    return html + "</span>";
  }

  function sequence(len) {
    var s = "";
    for (var i = 0; i < len; i++) {
      var b = BASES.charAt(Math.floor(Math.random() * 4));
      s += '<b class="' + b + '">' + b + "</b>";
    }
    // Repeated twice so the CSS scroll (-50%) loops without a jump.
    return '<div class="mat-seq" aria-hidden="true"><span class="mat-seq-track">' + s + s + "</span></div>";
  }

  function decorate(el) {
    if (!el || el.classList.contains("mat-progress")) return;
    el.classList.add("mat-progress");
    el.insertAdjacentHTML("afterbegin", helix(7));
    el.insertAdjacentHTML("beforeend", sequence(48));
  }

  function scan(root) {
    if (root.classList && root.classList.contains("shiny-progress-notification")) decorate(root);
    if (root.querySelectorAll) root.querySelectorAll(".shiny-progress-notification").forEach(decorate);
  }

  new MutationObserver(function (mutations) {
    mutations.forEach(function (m) { m.addedNodes.forEach(function (n) { if (n.nodeType === 1) scan(n); }); });
  }).observe(document.documentElement, { childList: true, subtree: true });
})();
