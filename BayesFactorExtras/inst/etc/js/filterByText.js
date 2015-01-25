// From http://www.lessanvaezi.com/filter-select-list-options/

jQuery.fn.filterByText = function( textbox, selectSingleMatch, funAfter ) {
  return this.each(function() {
    var select = this;
    var options = [];
    $(select).find('option').each(function() {
      options.push({value: $(this).val(), text: $(this).text(), className: $(this).attr('class')});
    });
    $(select).data('options', options);
    $(textbox).bind('change keyup', function() {
      var oldSel = $( select ).val();
      var options = $(select).empty().scrollTop(0).data('options');
      var search = $.trim($(this).val());
      var regex = new RegExp(search,'gi');
      var foundOldSel = false;
      
      $.each(options, function(i) {
        var option = options[i];
        if(option.text.match(regex) !== null) {
          if( option.value == oldSel ){ foundOldSel = true; }
          $(select).append(
			      $('<option>').text(option.text).val(option.value).addClass(option.className) 
			    );
        }
      });
      if( foundOldSel ){
        $(select).val(oldSel);   
      }
      if ( ( selectSingleMatch === true && $(select).children().length === 1 ) ||
          ( !foundOldSel) ) {
        $(select).change().children().get(0).selected = true;
      }
      funAfter();
    });
  });
};