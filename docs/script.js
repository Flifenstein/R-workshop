window.addEventListener('DOMContentLoaded', (event) => {
  $(document.links).filter(function() {
    var fileext = this.pathname.split("/").slice(-1)[0].split(".").slice(1).slice(-1)[0] || "html";
    return this.hostname != window.location.hostname || fileext != "html";
  }).attr('target', '_blank');
});