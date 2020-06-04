if (typeof window.Ujaen === "undefined") {
    window.Ujaen = {}
}
if (!Ujaen.clustering) {
    Ujaen.shiny = {}
}

Ujaen.shiny = $.extend(Ujaen.shiny, {

  inicializar: function () {

    $("ul.nav > li > a").eq(1).css("color","#4e5d6c");

    $("ul.nav > li").on("click",function(){
      if ($("ul.nav > li").first().attr("class")==="active"){
        $("ul.nav > li > a").eq(1).css("color","#ffffff");
        $("ul.nav > li > a").first().css("color","#4e5d6c");
      } else {
        $("ul.nav > li > a").first().css("color","#ffffff");
        $("ul.nav > li > a").eq(1).css("color","#4e5d6c");
      }
    })

    $("ul.nav > li > a").hover(function(){

      if ($(this).parent().attr("class") !=="active"){
        $(this).css("color","#ffffff");
      }

    }, function(){
      if ($(this).parent().attr("class") !=="active"){
        $(this).css("color","#4e5d6c");
      }
    }
    )

  }
})

$(document).ready(function () {
    Ujaen.shiny.inicializar();

});
