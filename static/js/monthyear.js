/**
Year + month editable input.
Internally value stored as {year: "2014", month: "10"}

@class yearmonth
@extends abstractinput
@final
@example
<a href="#" id="datapub" data-type="yearmonth" data-pk="1">październik 2014</a>
<script>
$(function(){
  $('#datapub').editable({
    url: '...',
    title: 'Data publikacji',
    value: {
      year: "2014",
      month: "10"
    }
  });
});
</script>
**/
(function ($) {
  "use strict";

  var YearMonth = function (options) {
    this.init('yearmonth', options, YearMonth.defaults);
  };

  //inherit from Abstract input
  $.fn.editableutils.inherit(YearMonth, $.fn.editabletypes.abstractinput);

  $.extend(YearMonth.prototype, {
    /**
    Renders input from tpl

    @method render() 
    **/        
    render: function() {
      this.$input = this.$tpl.find('input');
    },

    /**
    Default method to show value in element. Can be overwritten by display option.

    @method value2html(value, element) 
    **/
    value2html: function(value, element) {
      $(this).html("Dummy");
    },

    /**
    Gets value from element's html

    @method html2value(html) 
    **/        
    html2value: function(html) {        
      /*
      You may write parsing method to get value by element's html
      e.g. "Moscow, st. Lenina, bld. 15" => {city: "Moscow", street: "Lenina", building: "15"}
      but for complex structures it's not recommended.
      Better set value directly via javascript.
      */ 
      return null;  
    },

    /**
    Converts value to string. 
    It is used in internal comparing (not for sending to server).

    @method value2str(value)  
    **/
    value2str: function(value) {
      var str = '';
      if(value) {
        for(var k in value) {
          str = str + k + ':' + value[k] + ';';  
        }
      }
      return str;
    }, 

    /*
    Converts string to value. Used for reading value from 'data-value' attribute.

    @method str2value(str)  
    */
    str2value: function(str) {
      /*
      This is mainly for parsing value defined in data-value attribute. 
      If you will always set value by javascript, no need to overwrite it
      */
      return str;
    },                

    /**
    Sets value of input.

    @method value2input(value) 
    @param {mixed} value
    **/         
    value2input: function(value) {
      if(!value) {
        return;
      }
      this.$input.filter('[name="year"]').val(value.year);
      this.$input.filter('[name="month"]').val(value.month);
    },       

    /**
    Returns value of input.

    @method input2value() 
    **/          
    input2value: function() { 
      return {
        year: this.$input.filter('[name="year"]').val(), 
        month: this.$input.filter('[name="month"]').val()
      };
    },        

    /**
    Activates input: sets focus on the first field.

    @method activate() 
    **/        
    activate: function() {
      this.$input.filter('[name="year"]').focus();
    },  

    /**
    Attaches handler to submit form in case of 'showbuttons=false' mode

    @method autosubmit() 
    **/       
    autosubmit: function() {
      this.$input.keydown(function (e) {
        if (e.which === 13) {
          $(this).closest('form').submit();
        }
      });
    }
  });

  YearMonth.defaults = $.extend({}, $.fn.editabletypes.abstractinput.defaults, {
    tpl: '<div class="editable-multi"><label><span>Rok: </span><input type="text" name="year" class="input-small"></label></div>'+
         '<div class="editable-multi"><label><span>Miesiąc: </span><input type="text" name="month" class="input-small"></label></div>',
    inputclass: ''
  });

  $.fn.editabletypes.yearmonth = YearMonth;

}(window.jQuery));
