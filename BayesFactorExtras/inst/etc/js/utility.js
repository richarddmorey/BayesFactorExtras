function expString(x)
{
  if( isNaN(x) ) return "NA";
  
  var toBase10log = x / Math.log(10);
  var numMax = Math.log(Number.MAX_VALUE);
  var numMin = Math.log(Number.MIN_VALUE);

  var first, second;
   

  if( x > numMax ){
    first = Math.pow( 10, toBase10log - Math.floor(toBase10log) );
    second = Math.floor(toBase10log);
    return first + "e+" + second;
  }else if( x < numMin ){
    first = Math.pow( 10, 1 - (Math.ceil(toBase10log) - toBase10log) );
    second = Math.ceil(toBase10log) - 1;
    return first + "e" + second;    
  }else{
    return Math.exp(x);
  }
}


function buildBFBayesFactor(divname)
{
  var jsonContent = $("#" + divname + "_json").text();
  var bfObj = $.parseJSON(jsonContent);
  $.each(bfObj, function(index, value){
    var model = $("<td/>", { class: "bfmodel" }).html(value['$row']);
    var bf = $("<td/>", { class: "bfnum" }).html(expString(value['bf']));
    var error = $("<td/>", { class: "bferr" }).html(value['error']*100 + "%");
    $("<tr/>", { class: "bfrow" }).appendTo("#" + divname + "_bf").append(model).append(bf).append(error);
  });
}
