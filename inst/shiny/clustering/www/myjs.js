if (typeof window.Ujaen === "undefined") {
    window.Ujaen = {}
}
if (!Ujaen.clustering) {
    Ujaen.shiny = {}
}

Ujaen.shiny = $.extend(Ujaen.shiny, {

  inicializar: function () {
  }
})

$(document).ready(function () {
    Ujaen.shiny.inicializar();

});
