zeroTermModels = { BFlinearModel: [ "Intercept only" ], };

function getSortOrder(toSort) {
  for (var i = 0; i < toSort.length; i++) {
    toSort[i] = [toSort[i], i];
  }
  toSort.sort(function(left, right) {
    return left[0] < right[0] ? -1 : 1;
  });
  var sortIndices = [];
  for (var j = 0; j < toSort.length; j++) {
    sortIndices.push(toSort[j][1]);
  }
  return sortIndices;
}


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
    return prettyNum( Math.exp(x) );
  }
}

// https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Math/round
/**
	* Decimal adjustment of a number.
	*
	* @param	{String}	type	The type of adjustment.
	* @param	{Number}	value	The number.
	* @param	{Integer}	exp		The exponent (the 10 logarithm of the adjustment base).
	* @returns	{Number}			The adjusted value.
	*/
function decimalAdjust(type, value, exp) {
	// If the exp is undefined or zero...
	if (typeof exp === 'undefined' || +exp === 0) {
		return Math[type](value);
	}
	value = +value;
	exp = +exp;
	// If the value is not a number or the exp is not an integer...
	if (isNaN(value) || !(typeof exp === 'number' && exp % 1 === 0)) {
		return NaN;
	}
	// Shift
	value = value.toString().split('e');
	value = Math[type](+(value[0] + 'e' + (value[1] ? (+value[1] - exp) : -exp)));
	// Shift back
	value = value.toString().split('e');
	return +(value[0] + 'e' + (value[1] ? (+value[1] + exp) : exp));
}


function prettyNum(x, digits, scipen){
  if(typeof(digits)==='undefined') digits = 7;
  if(typeof(scipen)==='undefined') scipen = 0;
  
  var expnot = x.toExponential(digits);
  var stdnot = x.toString();
  var pretty;
  if( ( expnot.length + scipen ) < stdnot.length ){
    pretty = stdnot;
  }else{
    pretty = expnot;
  }
  return pretty;
}

function prettyErr(x, digits){
  if(typeof(digits)==='undefined') digits = 2;
  if(x == 0) return "0%";
  var perc = x * 100;
  roundOrd = Math.floor( Math.log(perc) / Math.log(10) );
  if(roundOrd > 0){
    perc = decimalAdjust("round", perc, roundOrd - 2);
  }else{
    perc = decimalAdjust("round", perc, roundOrd);
  }
  return perc + "%";
}

function buildBFBayesFactor(divname, denom_index)
{
  destroyBFBayesFactor( divname );
  if(typeof(denom_index)==='undefined') denom_index = 0;
  var jsonContent = $("#" + divname + "_json").text();
  var bfObj = $.parseJSON(jsonContent);
  var denombf = bfObj[ denom_index ][ 'bf' ];
  var denomerr = bfObj[ denom_index ][ 'error' ];
  var modelType = $("#" + divname + "_modeltype").text();
  buildBFDenominator(bfObj.splice(denom_index,1)[0], divname);
  
  $( "#" + divname ).find(".bfhmodel").data("sortCol",".bfmodel");
  $( "#" + divname ).find(".bfhbf").data("sortCol",".bfdisplay");
  $( "#" + divname ).find(".bfherr").data("sortCol",".bferrdisplay");
  
  $.each(bfObj, function(index, value){    
    var bf = value['bf'] - denombf;
    var err = Math.sqrt( Math.pow( value['error'], 2 ) + Math.pow( denomerr, 2 ) );
    var signclass;
    var bfprefix = "";
    if( bf == 0 ){
      signclass = "bfneut";
    }else if( bf > 0 ){
      signclass = "bfpos";
    }else{
      signclass = "bfneg";
      bfprefix = "1 / ";
    }
    var model = $("<td/>", { class: "bfmodel", title: "Click to make this model the denominator." }).text( value['row'] ).data("sort", bfNterms( value['row'], modelType ));
    var bfdisplay = $("<td/>", { class: "bfdisplay" }).text( bfprefix + expString( Math.abs(bf) ) ).data( "sort", bf );
    var errordisplay = $("<td/>", { class: "bferrdisplay" }).html( "&#177;" + prettyErr( err ) ).data( "sort", err );
    $("<tr/>", { class: "bfrow " + signclass }).data( "index", value['index'])
    .appendTo("#" + divname + "_bf")
    .append(model)
    .append(bfdisplay)
    .append(errordisplay)
  });
  $("#" + divname + " .bfrow").click( setDenom );
  $("#" + divname + " .bfhrow").children( ".bfcolunsorted" )
    .unbind( "click" )
    .click( bfSort );
  $("#" + divname + " .bfhrow").children()
    .not( ".bfcolunsorted" )
    .click().click();
  $("#" + divname + " .BFBayesFactor_search").keyup( function(){
    bfSearch.call( this );
  }).keyup();
  
}

function bfSort(){
  console.log("Sorting" + $( this ).data("sortCol"));
  var divname = $( this ).parents(".BFBayesFactor").attr('id');
  var sortColClass;
  var sortOrder = $( this ).data("sorted");
  sortOrder = sortOrder ? -sortOrder : -1;
  sortClass = sortOrder == -1 ? "bfcolascsorted" : "bfcoldecsorted"; 
  $( this ).parent()
    .children( "th" )
    .removeData("sorted")
    .removeClass("bfcoldecsorted bfcolascsorted")
    .addClass("bfcolunsorted");
  $( this ).data( "sorted", sortOrder )
    .removeClass("bfcoldecsorted bfcolascsorted bfcolunsorted")
    .addClass( sortClass );
  
  sortColClass = $( this ).data( "sortCol" );
  
  var x = $("#" + divname + " .bfrow").map(function(){
    var idx = $( this ).data( "index" );
    var sortValue = $( sortColClass, this ).data("sort");
    return sortValue;
  }).get();
  
  var order = getSortOrder(x);
  if(sortOrder == -1) order.reverse();
  
  var els = $("#" + divname + " .bfrow");
  var el;
  for( var i=order.length - 1 ; i >= 0 ; i-- ){
    el = els.eq( order[ i ] );
    el.prependTo( el.parent() );
  }
}

function buildBFDenominator(obj, divname)
{
  var model = $("<span/>", { class: "bfmodel", style: "padding: 0px; min-width: 0px;" })
    .data( "index", obj['index'] ).text( obj['row'] );
  model.appendTo("#" + divname + "_denom");
}

function setDenom()
{
  var idx = $( this ).data( "index" );
  var divname = $( this ).parents(".BFBayesFactor").attr('id');
  buildBFBayesFactor( divname, idx );
}

function destroyBFBayesFactor( divname )
{
  $("#" + divname + " .BFBayesFactor_denom").empty();
  $("#" + divname + " .BFBayesFactor_bf > tbody").remove();
}

function bfSearch()
{
  var divname = $( this ).parents(".BFBayesFactor").attr('id');
  var txt = $( this ).val();
  var terms = txt.split(/\s/);
  var necessary = [ ];
  var sufficient = [ ];
  var exclude = [ ];
  
  if( !validHashSearch.call( this ) ){
    $( this ).addClass( "bfInvalidSearch" );
    return;
  }else{
    $( this ).removeClass( "bfInvalidSearch" );
  }
  
  $.each(terms, function( index, value ){
    var first = value.substr(0, 1);
    if( !value.length ) return; 
    if( ( first == "+" ) ){ 
      if( value.length > 1){
        necessary.push( value.substr( 1 ) );
      }
    }else if( ( first == "-" ) ){
      if( value.length > 1 ){
        exclude.push( value.substr( 1 ) );
      }
    }else{
      sufficient.push( value );
    }
  });
  
  $("#" + divname + " .bfrow").each( function(){
    var nec = true;
    var exc = true;
    var suf = true;
    var model = $( ".bfmodel", this ).text();
    var nterms = $( ".bfmodel", this ).data('sort');
    
    function mapSearch( value, index )
    {
      var first = value.substr(0, 1);
      if( ( first == "#" ) ){
        if( value.length > 1){
          return nterms == value.substr(1);
        }else{
          return true;
        }
      }else{
        return model.match( value ) !== null;
      }
    }

    if( sufficient.length )
      var suf = $.map( sufficient, mapSearch ).some( function(el){ return el; } );
    if( necessary.length )
      nec = $.map( necessary, mapSearch ).every( function(el){ return el; } );
    if( exclude.length )
      exc = $.map( exclude, mapSearch ).every( function(el){ return !el; } );

    if( !suf | !nec | !exc ){
      $( this ).hide();
      //$( this ).removeClass( "bfSearchInclude" ).addClass( "bfSearchExclude" );
    }else{
      $( this ).show();
      //$( this ).removeClass( "bfSearchExclude" ).addClass( "bfSearchInclude" );
    }
  });
}

function validHashSearch(){
  var modelType = $( this ).parents(".BFBayesFactor").find(".BFBayesFactor_modeltype").text();
  if( ( modelType != "BFlinearModel" ) & ( $( this ).val().match("#") !== null ) ) return false;
  return true;
}

function bfNterms( model, type ){
  if( ( type != "BFlinearModel" ) ) return NaN;
  ztm = zeroTermModels[ type ];
  isztm = $.map(ztm, function( value, index ){
    return value == model;
  }).some( function( el ) { return el; } );
  
  // model is a zero term model
  if(isztm) return 0;
  
  return model.split(" + ").length;
}

