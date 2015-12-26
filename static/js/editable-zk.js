(function($) {

    $.fn.editableZK = function(options) {

        var field = this.attr('id') || 'dummy';
        var modalId = field + '-modal';
        var errorId = field + '-error';
        var okBtnId = field + '-ok-btn';
        var target = this;

        if ($('#' + modalId).length == 0) {
          // Merge with options from attributes.
          var settings = $.extend({
              type: target.attr('data-type'),
              pk: target.attr('data-pk'),                    // primary key in the database
              url: target.attr('data-url'),                  // url to send the POST request to
              title: target.attr('data-title'),              // title of the popup (will default to 'id')
              value: target.attr('data-value'),              // raw value (if single input)
              emptytext: target.attr('data-emptytext'),      // text to display when the value is empty
              placeholder: target.attr('data-placeholder'),  // placeholder for text values
              rows: target.attr('data-rows')                 // number of rows for text areas
          }, options);

          // TODO: Sanitize inputs!

          var select2Fields = new Array();

          function formatInput(type, id, value, settings) {
            function formatInputImpl(type, id, value, settings) {
              if (type === 'text') {

                var ph = settings.placeholder !== undefined ? ' placeholder="' + settings.placeholder + '"' : '';
                return '<input type="text" class="form-control" id="' + id + '" value="' + value + '"' + ph + '>';

              } else if (type === 'select') {

                select2Fields.push({id: id});

                var htmlArr = new Array();
                htmlArr.push('<select class="form-control" id="' + id + '" style="width: 100%">');
                for (var i = 0; i < settings.source.length; i++) {
                  var selected = (settings.source[i].value == value ? ' selected' : '');
                  htmlArr.push('<option value="' + settings.source[i].value + '"' + selected + '>' + settings.source[i].text + '</option>');
                };
                htmlArr.push('</select>');

                return htmlArr.join('');

              } else if (type === 'select2') {

                var sel2Opts = settings.select2;

                sel2Opts.initSelection = function(element, callback) {
                  var values = element.val().split("||");
                  var data = new Array;
                  for (var i = values.length - 1; i >= 0; i -= 2) {
                    data.push({id: values[i-1], text: values[i]});
                  }
                  element.val('');
                  callback (data);
                };

                select2Fields.push({ id: id, options: sel2Opts });

                return '<input type="text" class="form-control" id="' + id + '" value="' + value + '">';

              } else if (type === 'textarea') {

                var rows = settings.rows !== undefined ? ' rows="' + settings.rows + '"' : '';
                return '<textarea class="form-control" id="' + id + '"' + rows + '>' + value + '</textarea>';

              } else {

                return '';

              }
            }

            return formatInputImpl(type,
                                   id,
                                   typeof(settings.toInput) === 'function' ? settings.toInput(value) : value,
                                   settings);
          }

          // This function returns an array of data values grabbed from edit fields.
          function grabData() {
            function getInput(type, id) {
              if (type === 'text' || type === 'textarea') {
                return $('#' + id).val();
              } else if (type === 'select2' || type === 'select') {
                return $('#' + id).select2('data');
              }
            };

            var data = [];
            for (var i = fieldInfo.length - 1; i >= 0; i--) {
              data.push({ id: fieldInfo[i].id,
                          type: fieldInfo[i].type,
                          value: getInput(fieldInfo[i].type, fieldInfo[i].id) });
            }
            return data;
          }

          // This function uses data values previously grabbed from edit fields to reset the latter.
          function pushData(data) {
            function setInput(type, id, value) {
              if (type === 'text' || type === 'textarea') {
                $('#' + id).val(value);
              } else if (type === 'select' || type === 'select2') {
                $('#' + id).select2('data', value);
              }
            };

            for (var i = data.length - 1; i >= 0; i--) {
              setInput(data[i].type, data[i].id, data[i].value);
            }
          }

          if (!settings.title)
            settings.title = field;

          target.addClass('editable editable-click');
          target.attr('data-toggle', 'modal');
          target.attr('data-target', '#' + modalId);

          // Format the text to be displayed inside the element on the page.
          var htmlText = settings.value;

          if (settings.select2 != undefined) {
            var values = settings.value.split("||");
            var data = new Array;
            for (var i = values.length - 1; i >= 0; i -= 2) {
              data.push(values[i]);
            }
            htmlText = data.join(', ');
          } else if (settings.source != undefined && settings.source.length != undefined) {
            for (var i = settings.source.length - 1; i >= 0; i--) {
              if (settings.source[i].value == settings.value)
                htmlText = settings.source[i].text;
            }
          }

          if (typeof settings.emptytext == 'string' && htmlText === '')
            htmlText = settings.emptytext;

          if (typeof(settings.display) === 'function')
            htmlText = settings.display(htmlText);

          target.html(htmlText);

          // Format the DIV to contain the pop-up.
          var htmlArr = new Array();
          htmlArr.push('<div class="modal fade" id="' + modalId + '" tabindex="-1" role="dialog" aria-labelledby="' + modalId + '-label" aria-hidden="true">');
          htmlArr.push('  <div class="modal-dialog modal-sm">');
          htmlArr.push('    <div class="modal-content">');
          htmlArr.push('      <div class="modal-header">');
          htmlArr.push('        <button type="button" class="close" data-dismiss="modal" aria-hidden="true">&times;</button>');
          htmlArr.push('        <h4 class="modal-title" id="' + modalId + '-label">' + settings.title + '</h4>');
          htmlArr.push('      </div>');
          htmlArr.push('      <div id="' + errorId + '" class="alert alert-danger" role="alert" style="display: none;"></div>');
          htmlArr.push('      <div class="modal-body">');
          htmlArr.push('        <div class="row">');

          var fieldInfo = new Array;
          if (!settings.fields) {
            // No fields definition - create a simple editor
            fieldInfo.push({id: modalId + '-edit', type: settings.type});

            htmlArr.push('          <div class="col-md-12">');
            htmlArr.push('            ' + formatInput(settings.type, fieldInfo[0].id, settings.value, settings));
            htmlArr.push('          </div>');
          } else {
            // Fields definition present - create an editor with labels
            for (var key in settings.fields) {
              var fieldId = modalId + '-' + key;
              var fieldType = settings.fields[key].type || settings.type;

              fieldInfo.push({id: fieldId, name: key, type: fieldType});

              if (settings.fields[key].placeholder === undefined)
                settings.fields[key].placeholder = '';

              htmlArr.push('          <div class="col-md-4">');
              htmlArr.push('            <label for="' + fieldId + '">' + settings.fields[key].label + '</label>');
              htmlArr.push('          </div>');
              htmlArr.push('          <div class="col-md-8">');
              htmlArr.push('            ' + formatInput(fieldType,
                                                        fieldId,
                                                        typeof settings.value[key] == 'string' ? settings.value[key] : settings.value,
                                                        settings.fields[key]));
              htmlArr.push('          </div>');
            }
          }

          htmlArr.push('        </div>');
          htmlArr.push('      </div>');
          htmlArr.push('      <div class="modal-footer">');
          htmlArr.push('        <button type="button" class="btn btn-xs btn-default" data-dismiss="modal">Anuluj</button>');
          htmlArr.push('        <button type="button" class="btn btn-xs btn-zk" id="' + okBtnId + '">OK</button>');
          htmlArr.push('      </div>');
          htmlArr.push('    </div>');
          htmlArr.push('  </div>');
          htmlArr.push('</div>');

          $('body').append(htmlArr.join(''));

          function initHandler() {
            // Initialize select2 controls if there are any.
            if (select2Fields.length > 0) {
              for (var i = select2Fields.length - 1; i >= 0; i--) {
                var q = '#' + select2Fields[i].id;
                if (select2Fields[i].options !== undefined)
                  $(q).select2(select2Fields[i].options);
                else
                  $(q).select2();
              }
            }

            // Grab initial data - it's easier to do it here where we can reuse code.
            $('#' + modalId).data('data', grabData());

            // Only call this handler the first time around.
            $('#' + modalId).unbind('show.bs.modal', initHandler);
          };
          $('#' + modalId).on('show.bs.modal', initHandler);

          $('#' + modalId).on('shown.bs.modal', function () {
            // Reset the OK flag that is used to know if the popup was canceled or not.
            $('#' + modalId).data('okFlag', false);

            // Focus the first field.
            $('#' + fieldInfo[0].id).focus();

            // Open the select2 field if it is focused.
            if (fieldInfo[0].type.indexOf('select') == 0) {
              $('#' + fieldInfo[0].id).select2("open");
            }
          });

          $('#' + modalId).on('hidden.bs.modal', function () {
            if ($('#' + modalId).data('okFlag') === false) {
              // Popup was canceled - push saved data back into the controls.
              pushData($('#' + modalId).data('data'));
            }

            $('#' + modalId).data('okFlag', false);
          });

          function enterHandler(e) {
            if (e.which == 13)
              $('#' + okBtnId).click();
          };
          for (var i = fieldInfo.length - 1; i >= 0; i--) {
            // select2 controls automatically open on Enter so there is no point in setting the handler.
            // And we don't want this handler for multiline editors.
            if (fieldInfo[i].type == 'text')
              $('#' + fieldInfo[i].id).keyup(enterHandler);
          };

          $('#' + okBtnId).click(function (){
            // 1. Get the value
            function getRawVal(fieldInfo) {
              return $('#' + fieldInfo.id).val();
            }
            function getTextVal(fieldInfo, rawVal) {
              if (fieldInfo.type == 'select') {
                return $('#' + fieldInfo.id).select2('data').text;
              } else if (fieldInfo.type == 'select2') {
                var arr = new Array;
                var selData = $('#' + fieldInfo.id).select2('data');
                for (var i = 0; i < selData.length; i++) {
                  arr.push(selData[i].text);
                };
                return arr.join(', ');
              } else if (fieldInfo.type == 'textarea') {
                return rawVal.replace(/\n/g, '<br>');
              } else {
                return rawVal;
              }
            }

            if (fieldInfo.length > 1) {
              var rawVal = {};
              var textVal = {};
              for (var i = fieldInfo.length - 1; i >= 0; i--) {
                var thisRawVal = getRawVal(fieldInfo[i]);
                rawVal[fieldInfo[i].name] = thisRawVal;
                textVal[fieldInfo[i].name] = getTextVal(fieldInfo[i], thisRawVal);
              }
            } else {
              var thisRawVal = getRawVal(fieldInfo[0]);
              rawVal = thisRawVal;
              textVal = getTextVal(fieldInfo[0], thisRawVal);
            }

            // 2. translate (if required) and validate value
            var rawVal2 = typeof(settings.fromInput) === 'function' ? settings.fromInput(rawVal) : rawVal;
            var errorMsg = typeof(settings.validate) === 'function' ? settings.validate(rawVal2) : '';

            if (typeof errorMsg === 'string' && errorMsg != '') {
              // Display the error message
              $('#' + errorId).html(errorMsg).show();
            } else {
              // 3. TODO: send request to the server

              // 4. change element on page using display()
              if(typeof(settings.display) === 'function') {
                target.html(settings.display(rawVal2, textVal));
              } else if(textVal == '' && typeof settings.emptytext == 'string') {
                target.html(settings.emptytext);
              } else {
                // This won't work properly if this is a multi-field editor - and that's what we want
                // because in such cases the user should provide the display() function.
                target.html(textVal);
              }

              // 5. hide the modal
              $('#' + modalId).data('okFlag', true);
              $('#' + modalId).modal('hide');
              $('#' + errorId).hide();

              // 6. Reset the initialization data we're keeping around.
              $('#' + modalId).data('data', grabData());
            }
          });
        }

        return this;
    };

}(jQuery));
