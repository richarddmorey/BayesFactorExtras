/* Functions for interacting with MCMC plots */

function buildBFmcmc( divname ){
  setupSlider( divname );
  $( "#" + divname + "_parselect" )
    .children().get(0).selected = true;
  $( "#" + divname + "_parselect" )
    .filterByText( "#" + divname + "_search", true, function(){ updateSlider( divname ); } )
    .on("change",  function(){ revealPlot.call( this ) })
    .trigger("change");
}

function revealPlot(){
  var divname = $( this ).parents(".BFmcmc").attr('id');
  var parCode = $( this ).val();
  var numSelected = $( this ).children( ":selected" ).index();
  $( "#" + divname + "_modelslider" ).val( numSelected + 1 );
  
  $( this ).parents(".BFmcmc").find( ".BFmcmc_plot").addClass( "bfhide" );
  var plotDiv = divname + "_plot_" + parCode;
  $( "#" + plotDiv ).removeClass( "bfhide" ); 

}

function setupSlider( divname ){
  var nPars = $( "#" + divname + "_parselect option" ).length;
  var numSelected = $( "#" + divname + "_parselect" ).children( ":selected" ).index();
  numSelected = parseInt( numSelected + 1 );
  
  if( nPars < 1 ){
     $('.slider').attr('disabled', 'disabled');
  }
  $( "#" + divname + "_modelslider" ).noUiSlider({
    start:  numSelected,
	  range: {
		  'min':  1,
		  'max': nPars 
	  },
    step: 1
  }, true );
  
  $('.slider').removeAttr('disabled');
  $( "#" + divname + "_modelslider" ).Link('lower')
    .to( function( value ){ useSlider.call( this ) });
}

function updateSlider( divname ){
  var nPars = $( "#" + divname + "_parselect option" ).length;
  var numSelected = $( "#" + divname + "_parselect" ).children( ":selected" ).index();
  numSelected = parseInt( numSelected + 1);

  if( nPars < 1 ){
     $('.slider').attr('disabled', 'disabled');
  }
  $( "#" + divname + "_modelslider" ).noUiSlider({
    start: numSelected,
    range: {
  	  'min': Number(1),
		  'max': Number(nPars) 
	  }
  }, true );
  $('.slider').removeAttr('disabled');
  
}

function useSlider(){
  var divname = $( this ).parents(".BFmcmc").attr('id');
  var value =  parseInt( this.val() );
  var setVal = $( "#" + divname + "_parselect option:nth-child(" + value + ")" ).val();
  //var curVal = $( "#" + divname + "_parselect" ).val();
  $( "#" + divname + "_parselect" ).val( setVal ).change();
}
