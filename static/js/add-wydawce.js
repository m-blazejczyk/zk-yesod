/**
Editable input for adding a publisher (name + URL).
Internally value stored as {nazwa: "kultura gniewu", url: "http://www.kultura.com.pl"}

@class addWydawce
@extends abstractinput
@final
@example
<a class="btn btn-default btn-xs">
  <span class="glyphicon glyphicon-plus-sign editable-click-btn" id="addWydawce" data-type="addWydawce" data-pk="#{lookupId}" data-url=@{KopalniaAddWydawcaR} data-title="Nowy wydawca" aria-hidden="true">
</button>
<script>
$(function(){
  $('#addWydawce').editable({
    url: '...',
    title: 'Nowy wydawca',
    value: {
      nazwa: "kultura gniewu",
      url: "http://www.kultura.com.pl"
    }
  });
});
</script>
**/
(function ($) {
  "use strict";

  var AddWydawce = function (options) {
    this.init('addWydawce', options, AddWydawce.defaults);
  };

  //inherit from Abstract input
  $.fn.editableutils.inherit(AddWydawce, $.fn.editabletypes.abstractinput);

  $.extend(AddWydawce.prototype, {
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
      this.$input.filter('[name="nazwa"]').val(value.nazwa);
      this.$input.filter('[name="url"]').val(value.url);
    },       

    /**
    Returns value of input.

    @method input2value() 
    **/          
    input2value: function() { 
      return {
        nazwa: this.$input.filter('[name="nazwa"]').val(), 
        url: this.$input.filter('[name="url"]').val()
      };
    },        

    /**
    Activates input: sets focus on the first field.

    @method activate() 
    **/        
    activate: function() {
      this.$input.filter('[name="nazwa"]').focus();
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

  AddWydawce.defaults = $.extend({}, $.fn.editabletypes.abstractinput.defaults, {
    tpl: '<div class="editable-multi"><label><span>Nazwa: </span><input type="text" name="nazwa" class="input-small"></label></div>'+
         '<div class="editable-multi"><label><span>Strona (url): </span><input type="text" name="url" class="input-small"></label></div>',
    inputclass: ''
  });

  $.fn.editabletypes.addWydawce = AddWydawce;

}(window.jQuery));
