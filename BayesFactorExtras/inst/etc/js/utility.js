zeroTermModels = { BFlinearModel: [ "Intercept only" ], };


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
    var nterms = $("<td/>", { class: "bfnterms bfhide" }).text( bfNterms( value['row'], modelType ) );
    var ind = $("<td/>", { class: "bfindex bfhide" }).text( value['index'] );
    var model = $("<td/>", { class: "bfmodel", title: "Click to make this model the denominator." }).text( value['row'] );
    var bfnum = $("<td/>", { class: "bfnum bfhide" }).text( bf );    
    var bfdisplay = $("<td/>", { class: "bfdisplay" }).text( bfprefix + expString( Math.abs(bf) ) );
    var error = $("<td/>", { class: "bferr" }).html( "&#177;" + prettyErr( err ) );
    $("<tr/>", { class: "bfrow " + signclass }).appendTo("#" + divname + "_bf").append(model).append(bfdisplay).append(error).append(ind).append(bfnum).append(nterms);
  });
  $("#" + divname + " .bfrow").click( setDenom );
  $("#" + divname + " .BFBayesFactor_search").keyup( function(){
    bfSearch.call( this );
  }).keyup();
  
}

function buildBFDenominator(obj, divname)
{
  var model = $("<div/>", { class: "bfmodel" }).text( obj['row'] );
  var ind = $("<div/>", { class: "bfindex bfhide" }).text( obj['index'] );
  model.appendTo("#" + divname + "_denom");
  ind.appendTo("#" + divname + "_denom");
}

function setDenom()
{
  var idx = $( this ).find(".bfindex").text();  
  var divname = $( this ).parents(".BFBayesFactor").attr('id');
  buildBFBayesFactor( divname, idx );
}

function destroyBFBayesFactor( divname )
{
  $("#" + divname + " .BFBayesFactor_denom").html("");
  $("#" + divname + " .BFBayesFactor_bf").html("");
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
    var nterms = $( ".bfnterms", this ).text();
    
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

